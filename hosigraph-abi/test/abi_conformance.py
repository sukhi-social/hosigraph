#!/usr/bin/env python3
"""Conformance of libhosi, driven from Python (ctypes) the way a foreign host would drive it.

    python3 -I hosigraph-abi/test/abi_conformance.py PATH/TO/libhosi.{dylib,so}

No database is needed.  Two things stand in for one:

  * `open` with no :dir opens nothing; the connection answers :ping and :abi and refuses the rest.
  * To hold requests waiting, `open` is pointed at a fake `dtlv` that waits for a file to appear
    and then fails.  Until the file is there the connection is "still starting", so requests
    wait on it (and are all answered with the failure when it ends).

Rules tried (numbers are those of the requirements, section 6.2, and of setu's proposal T-13/T-14):
  T-01 exactly once · T-02 a duplicate id · T-03 cancel and finish meet · T-07 the limit
  T-08 close answers what waits · T-09 no start after shutdown · T-12 bodies are bytes the ABI
  does not read · T-13 never on the calling thread · T-14 close returns after the callbacks.

The runtime starts once per process and cannot start again, so the order is fixed:
before init, running, after shutdown.  Exit status 0 if every check passed.
"""
import collections
import ctypes
import errno
import os
import re
import shutil
import signal
import stat
import sys
import tempfile
import threading
import time
import traceback

if len(sys.argv) != 2:
    sys.exit(__doc__)
LIB = ctypes.CDLL(sys.argv[1])   # CDLL lets go of the GIL during a call, so callbacks can take it

HERE = os.path.dirname(os.path.abspath(__file__))
HEADER = os.path.join(HERE, "..", "gen", "include", "hosi.h")

OK, ERR_APP, ERR_PROTOCOL, ERR_IO, ERR_TIMEOUT, ERR_CANCELLED, ERR_CLOSED, ERR_INTERNAL = range(8)
LIMIT = 1024   # what Hosigraph.Abi.Library.libLimit says

DONE = ctypes.CFUNCTYPE(None, ctypes.c_uint64, ctypes.c_int,
                        ctypes.POINTER(ctypes.c_uint8), ctypes.c_size_t, ctypes.c_void_p)

LIB.hosi_runtime_init.restype = ctypes.c_int
LIB.hosi_runtime_shutdown.restype = None
LIB.hosi_open.restype = ctypes.c_void_p
LIB.hosi_open.argtypes = [ctypes.c_char_p, ctypes.c_void_p]
LIB.hosi_close.restype = None
LIB.hosi_close.argtypes = [ctypes.c_void_p]
LIB.hosi_submit.restype = ctypes.c_int
LIB.hosi_submit.argtypes = [ctypes.c_void_p, ctypes.c_uint64, ctypes.c_char_p, ctypes.c_size_t, DONE, ctypes.c_void_p]
LIB.hosi_cancel.restype = ctypes.c_int
LIB.hosi_cancel.argtypes = [ctypes.c_void_p, ctypes.c_uint64]
LIB.hosi_thread_done.restype = None
LIB.hosi_abi_version.restype = ctypes.c_uint32

tls = threading.local()   # tls.in_call is true while this thread is inside a call of the library


def call(f, *args):
    tls.in_call = True
    try:
        return f(*args)
    finally:
        tls.in_call = False


class Rec:
    """Records every callback: id -> [(status, body, thread ident)], and the ones that ran inside a call."""

    def __init__(self, slow_for=None, slow_for_s=0.0):
        self.cv = threading.Condition()
        self.events = collections.defaultdict(list)
        self.reentrant = []
        self.finished = set()           # ids whose callback has returned
        self.slow_for, self.slow_for_s = slow_for, slow_for_s
        self.cb = DONE(self._on_done)   # kept alive by being an attribute

    def _on_done(self, i, st, buf, n, ud):
        data = bytes(buf[:n]) if n else b""
        if getattr(tls, "in_call", False):
            self.reentrant.append(i)
        with self.cv:
            self.events[i].append((st, data, threading.get_ident()))
            self.cv.notify_all()
        if i == self.slow_for:
            time.sleep(self.slow_for_s)
        with self.cv:
            self.finished.add(i)
            self.cv.notify_all()

    def wait(self, ids, timeout=30.0):
        end = time.time() + timeout
        with self.cv:
            while not all(self.events.get(i) for i in ids):
                left = end - time.time()
                if left <= 0:
                    missing = [i for i in ids if not self.events.get(i)]
                    raise AssertionError(f"timeout: {len(missing)} ids never answered, e.g. {missing[:5]}")
                self.cv.wait(left)

    def once(self, ids, settle=0.2):
        """Every id answered, and (after a pause) none answered twice."""
        self.wait(ids)
        time.sleep(settle)
        twice = [i for i in ids if len(self.events[i]) != 1]
        assert not twice, f"answered other than once: {[(i, len(self.events[i])) for i in twice[:5]]}"

    def status(self, i):
        return self.events[i][0][0]

    def body(self, i):
        return self.events[i][0][1]


def expect(cond, what):
    if not cond:
        raise AssertionError(what)


def expect_eq(actual, wanted, what):
    if actual != wanted:
        raise AssertionError(f"{what}: wanted {wanted!r}, got {actual!r}")


def submit(c, rec, i, body):
    return call(LIB.hosi_submit, c, i, body, len(body), rec.cb, None)


def unattached():
    c = call(LIB.hosi_open, b"{}", None)
    expect(c, "open gave NULL while the runtime runs")
    return c


# ---- a connection that stays "starting" until the gate is opened ---------------------------

GATES = []   # every gate made, so that a test that fails half way does not leave a start waiting for ever


class Gated:
    def __init__(self):
        self.dir = tempfile.mkdtemp(prefix="hosi-abi-")
        GATES.append(self)
        self.gate = os.path.join(self.dir, "gate")
        self.exe = os.path.join(self.dir, "fake-dtlv")
        with open(self.exe, "w") as f:
            # waits for the gate (at most a minute, in case nobody ever opens it), then fails
            f.write(f'#!/bin/sh\ni=0\nwhile [ ! -e "{self.gate}" ] && [ $i -lt 3000 ]; do sleep 0.02; i=$((i+1)); done\nexit 1\n')
        os.chmod(self.exe, os.stat(self.exe).st_mode | stat.S_IXUSR)
        self.uri = ('{:dir "%s" :config {:khana [:dtlv "%s"]}}' % (os.path.join(self.dir, "db"), self.exe)).encode()

    def open(self):
        c = call(LIB.hosi_open, self.uri, None)
        expect(c, "open gave NULL while the runtime runs")
        return c

    def release(self):
        open(self.gate, "w").close()


PING = b"{:op :ping :args 1}"

# ---------------------------------------------------------------------------------------------
# before init

def t_before_init():
    expect_eq(call(LIB.hosi_open, b"{}", None), None, "open before init")
    expect_eq(call(LIB.hosi_submit, 5, 1, PING, len(PING), Rec().cb, None), -errno.EPIPE, "submit before init")
    expect_eq(call(LIB.hosi_cancel, 5, 1), -errno.EPIPE, "cancel before init")
    call(LIB.hosi_close, 5)              # nothing to close; must not crash
    call(LIB.hosi_close, None)
    call(LIB.hosi_thread_done)
    call(LIB.hosi_runtime_shutdown)      # nothing to stop
    expect_eq(LIB.hosi_abi_version(), header_version(), "abi_version works without the runtime")


def header_version():
    with open(HEADER) as f:
        return int(re.search(r"#define HOSI_ABI_VERSION (\d+)", f.read()).group(1))


# ---------------------------------------------------------------------------------------------
# running

def t_init_twice():
    expect_eq(LIB.hosi_runtime_init(), 0, "second init")


def t_ping():
    rec, c = Rec(), unattached()
    body = '{:op :ping :args [1 "é" :k 2.5 true nil {:a [1]}]}'.encode()
    expect_eq(submit(c, rec, 1, body), 0, "submit")
    rec.once([1])
    expect_eq(rec.status(1), OK, "status")
    expect_eq(rec.body(1), '{:ok [1 "é" :k 2.5 true nil {:a [1]}]}'.encode(), "body")
    call(LIB.hosi_close, c)


def t_abi_op():
    rec, c = Rec(), unattached()
    expect_eq(submit(c, rec, 1, b"{:op :abi}"), 0, "submit")
    rec.once([1])
    expect_eq(rec.status(1), OK, "status")
    b = rec.body(1)
    # a comma is whitespace in EDN, and the writer puts one between the pairs of a map
    head = rb'\{:ok \{:name "hosi"[,\s]+:version %d[,\s]+:ops \[:ping :abi :write ' % header_version()
    expect(re.match(head, b), b)
    for op in (b":write-many", b":get-entities", b":query-many", b":history-trace", b":rebuild-khana"):
        expect(op in b, f"{op!r} is not listed in {b!r}")
    call(LIB.hosi_close, c)


def t_not_the_contract():
    rec, c = Rec(), unattached()
    cases = {
        1: b"not edn",
        2: b"\xff\xfe",
        3: b"",
        4: b"{:op :nope}",
        5: b"{:op :get-entities :args {:ids \"x\"}}",
        6: b"{:op :ping :extra 1}",
        7: b"[:op :ping]",
        8: b"{:op :get-entities :args {:ids [1] :surprise 2}}",
    }
    for i, body in cases.items():
        expect_eq(call(LIB.hosi_submit, c, i, body if body else None, len(body), rec.cb, None), 0, f"submit {i}")
    rec.once(list(cases))
    for i in cases:
        expect_eq(rec.status(i), ERR_PROTOCOL, f"status of {cases[i]!r}: {rec.body(i)!r}")
        expect(rec.body(i).startswith(b"{:error :"), rec.body(i))
    call(LIB.hosi_close, c)


def t_uri_not_utf8():
    rec = Rec()
    c = call(LIB.hosi_open, b"\xff\xfe", None)
    expect(c, "open gave NULL")
    expect_eq(submit(c, rec, 1, PING), 0, "submit")
    rec.once([1])
    expect_eq(rec.status(1), ERR_PROTOCOL, "status")
    expect(b":bad-open" in rec.body(1), rec.body(1))
    call(LIB.hosi_close, c)


def t_uri_not_the_shape():
    rec = Rec()
    c = call(LIB.hosi_open, b'{:dir 5}', None)
    expect_eq(submit(c, rec, 1, PING), 0, "submit")
    rec.once([1])
    expect_eq(rec.status(1), ERR_PROTOCOL, "status")
    expect(b":bad-open" in rec.body(1) and b":dir" in rec.body(1), rec.body(1))
    call(LIB.hosi_close, c)


def t_no_database_refuses():
    rec, c = Rec(), unattached()
    for i, op in enumerate([b"stats", b"seal", b"verify", b"rebuild-khana"], 1):
        expect_eq(submit(c, rec, i, b"{:op :%s}" % op), 0, "submit")
    rec.once([1, 2, 3, 4])
    for i in (1, 2, 3, 4):
        expect_eq(rec.status(i), ERR_APP, f"status {rec.body(i)!r}")
        expect(b":bad-argument" in rec.body(i), rec.body(i))
    call(LIB.hosi_close, c)


def t_request_is_copied():
    rec, c = Rec(), unattached()
    buf = ctypes.create_string_buffer(b'{:op :ping :args "first"}')
    expect_eq(call(LIB.hosi_submit, c, 1, buf, len(buf.raw) - 1, rec.cb, None), 0, "submit")
    ctypes.memset(buf, ord("x"), len(buf.raw) - 1)   # the host reuses its buffer at once
    rec.once([1])
    expect_eq(rec.body(1), b'{:ok "first"}', "answer to the request as it was when submitted")
    call(LIB.hosi_close, c)


def t_never_on_the_calling_thread():       # T-13
    rec, c = Rec(), unattached()
    me = threading.get_ident()
    ids = list(range(1, 2001))
    for i in ids:
        expect_eq(submit(c, rec, i, PING), 0, "submit")
        if i % 3 == 0:
            expect_eq(call(LIB.hosi_cancel, c, i), 0, "cancel")
    rec.once(ids)
    expect_eq(rec.reentrant, [], "callbacks that ran inside the call that caused them")
    expect(all(t != me for i in ids for (_, _, t) in rec.events[i]), "a callback ran on the host's own thread")
    sts = collections.Counter(rec.status(i) for i in ids)
    expect(set(sts) <= {OK, ERR_CANCELLED}, f"unexpected statuses {sts}")
    call(LIB.hosi_close, c)


def t_cancel_meets_finish_from_threads():   # T-01, T-03
    rec, c = Rec(), unattached()
    threads, per = 8, 1500
    errors = []

    def work(t):
        try:
            for k in range(per):
                i = t * per + k + 1
                if submit(c, rec, i, PING) != 0:
                    errors.append(f"submit {i}")
                call(LIB.hosi_cancel, c, i)
                call(LIB.hosi_cancel, c, i)       # and again: idempotent
        except Exception as e:                    # noqa: BLE001
            errors.append(repr(e))

    ts = [threading.Thread(target=work, args=(t,)) for t in range(threads)]
    [t.start() for t in ts]
    [t.join() for t in ts]
    expect(not errors, errors[:3])
    ids = list(range(1, threads * per + 1))
    rec.once(ids, settle=0.5)
    sts = collections.Counter(rec.status(i) for i in ids)
    expect(set(sts) <= {OK, ERR_CANCELLED}, f"unexpected statuses {sts}")
    expect_eq(rec.reentrant, [], "reentrant callbacks")
    print(f"      (finish won {sts[OK]} times, cancel won {sts[ERR_CANCELLED]} times)")
    call(LIB.hosi_close, c)


def t_many_threads_submit():
    rec, c = Rec(), unattached()
    threads, per = 8, 2000

    def work(t):
        for k in range(per):
            i = 1_000_000 + t * per + k
            expect_eq(submit(c, rec, i, PING), 0, "submit")
        call(LIB.hosi_thread_done)

    ts = [threading.Thread(target=work, args=(t,)) for t in range(threads)]
    [t.start() for t in ts]
    [t.join() for t in ts]
    ids = [1_000_000 + n for n in range(threads * per)]
    rec.once(ids, settle=0.3)
    expect(all(rec.status(i) == OK for i in ids), "not all ok")
    call(LIB.hosi_close, c)


def t_bad_handles():
    rec = Rec()
    expect_eq(call(LIB.hosi_submit, None, 1, PING, len(PING), rec.cb, None), -errno.EINVAL, "NULL handle")
    expect_eq(call(LIB.hosi_submit, 0xdead, 1, PING, len(PING), rec.cb, None), -errno.EINVAL, "handle never given")
    expect_eq(call(LIB.hosi_cancel, None, 1), -errno.EINVAL, "cancel NULL")
    expect_eq(call(LIB.hosi_cancel, 0xdead, 1), -errno.EINVAL, "cancel handle never given")
    c = unattached()
    expect_eq(call(LIB.hosi_submit, c, 1, PING, len(PING), ctypes.cast(None, DONE), None), -errno.EINVAL, "NULL callback")
    expect_eq(call(LIB.hosi_submit, c, 1, None, 5, rec.cb, None), -errno.EINVAL, "NULL body with a length")
    call(LIB.hosi_close, c)
    expect_eq(call(LIB.hosi_submit, c, 1, PING, len(PING), rec.cb, None), -errno.EINVAL, "closed handle")
    expect_eq(call(LIB.hosi_cancel, c, 1), -errno.EINVAL, "cancel closed handle")
    call(LIB.hosi_close, c)              # twice: harmless
    call(LIB.hosi_close, 0xdead)         # never given: harmless
    call(LIB.hosi_close, None)
    expect_eq(rec.events, {}, "nothing was answered")


def t_duplicate_id():                    # T-02
    g, rec = Gated(), Rec()
    c = g.open()
    expect_eq(submit(c, rec, 7, PING), 0, "first")
    expect_eq(submit(c, rec, 7, PING), -errno.EINVAL, "same id while it waits")
    g.release()
    rec.once([7])
    expect_eq(rec.status(7), ERR_INTERNAL, "the start failed, so every waiting request is told so")
    expect(b":open-exception" in rec.body(7), rec.body(7))
    expect_eq(submit(c, rec, 7, PING), 0, "the id is free once answered")
    rec.wait([7])
    call(LIB.hosi_close, c)


def t_limit():                           # T-07
    g, rec = Gated(), Rec()
    c = g.open()
    ids = list(range(1, LIMIT + 1))
    for i in ids:
        expect_eq(submit(c, rec, i, PING), 0, f"submit {i}")
    expect_eq(submit(c, rec, LIMIT + 1, PING), -errno.EBUSY, "one more than the limit")
    expect_eq(call(LIB.hosi_cancel, c, 1), 0, "cancel one")
    rec.wait([1])
    expect_eq(rec.status(1), ERR_CANCELLED, "cancelled")
    expect_eq(submit(c, rec, LIMIT + 1, PING), 0, "room again after a cancel")
    g.release()
    rest = ids[1:] + [LIMIT + 1]
    rec.once(rest, settle=0.3)
    expect_eq(collections.Counter(rec.status(i) for i in rest), {ERR_INTERNAL: len(rest)}, "the rest are told the start failed")
    expect_eq(len(rec.events[1]), 1, "the cancelled one is not answered again")
    call(LIB.hosi_close, c)


def t_cancel_pending():
    g, rec = Gated(), Rec()
    c = g.open()
    for i in (1, 2, 3):
        expect_eq(submit(c, rec, i, PING), 0, "submit")
    expect_eq(call(LIB.hosi_cancel, c, 2), 0, "cancel")
    rec.wait([2])
    expect_eq(rec.status(2), ERR_CANCELLED, "status")
    expect(b":cancelled" in rec.body(2), rec.body(2))
    expect_eq(call(LIB.hosi_cancel, c, 2), 0, "cancel again")
    expect_eq(call(LIB.hosi_cancel, c, 99), 0, "cancel an id nobody used")
    g.release()
    rec.once([1, 2, 3])
    expect_eq(rec.status(2), ERR_CANCELLED, "still cancelled, answered once")
    call(LIB.hosi_close, c)


def t_close_answers_and_waits():          # T-08, T-14
    g, rec = Gated(), Rec(slow_for=2, slow_for_s=0.4)
    c = g.open()
    for i in (1, 2, 3):
        expect_eq(submit(c, rec, i, PING), 0, "submit")
    threading.Timer(0.3, g.release).start()    # close waits for the start; let the start end
    t0 = time.time()
    call(LIB.hosi_close, c)
    took = time.time() - t0
    expect({1, 2, 3} <= rec.finished, f"close returned while a callback was still running (finished: {rec.finished})")
    expect(took >= 0.3, f"close did not wait for the start ({took:.2f}s)")
    for i in (1, 2, 3):
        expect_eq(len(rec.events[i]), 1, f"answers of {i}")
        expect_eq(rec.status(i), ERR_CLOSED, f"status of {i}")
        expect(b":closed" in rec.body(i), rec.body(i))
    n = sum(len(v) for v in rec.events.values())
    time.sleep(0.3)
    expect_eq(sum(len(v) for v in rec.events.values()), n, "callbacks after close returned")


def t_connections_are_apart():
    rec1, rec2, a, b = Rec(), Rec(), unattached(), unattached()
    expect_eq(submit(a, rec1, 1, PING), 0, "a")
    expect_eq(submit(b, rec2, 1, PING), 0, "b: the same id on another connection is fine")
    rec1.once([1])
    rec2.once([1])
    call(LIB.hosi_close, a)
    expect_eq(submit(b, rec2, 2, PING), 0, "b still works after a is closed")
    rec2.once([1, 2])
    call(LIB.hosi_close, b)


def t_sigint_stays_with_the_host():
    got = "nothing"
    try:
        os.kill(os.getpid(), signal.SIGINT)
        time.sleep(0.3)
    except KeyboardInterrupt:
        got = "KeyboardInterrupt"
    expect_eq(got, "KeyboardInterrupt", "SIGINT must still reach Python (--install-signal-handlers=no)")


# ---------------------------------------------------------------------------------------------
# after shutdown

def t_shutdown_with_waiting():           # T-08, T-09, T-14 at the end of the runtime
    g, rec = Gated(), Rec(slow_for=3, slow_for_s=0.3)
    c = g.open()
    for i in (1, 2, 3):
        expect_eq(submit(c, rec, i, PING), 0, "submit")
    threading.Timer(0.3, g.release).start()
    call(LIB.hosi_runtime_shutdown)
    expect({1, 2, 3} <= rec.finished, f"shutdown returned while a callback was still running ({rec.finished})")
    expect_eq([rec.status(i) for i in (1, 2, 3)], [ERR_CLOSED] * 3, "every waiting request is told closed")
    SHUT["rec"], SHUT["c"] = rec, c


SHUT = {}


def t_after_shutdown():
    rec = Rec()
    expect_eq(call(LIB.hosi_open, b"{}", None), None, "open after shutdown")
    expect_eq(call(LIB.hosi_submit, SHUT["c"], 9, PING, len(PING), rec.cb, None), -errno.EPIPE, "submit after shutdown")
    expect_eq(call(LIB.hosi_cancel, SHUT["c"], 9), -errno.EPIPE, "cancel after shutdown")
    call(LIB.hosi_close, SHUT["c"])      # harmless
    expect_eq(LIB.hosi_runtime_init(), -errno.EPERM, "init after shutdown is refused, not a crash")
    call(LIB.hosi_runtime_shutdown)      # twice: harmless
    call(LIB.hosi_thread_done)
    expect_eq(LIB.hosi_abi_version(), header_version(), "abi_version")
    expect_eq(rec.events, {}, "nothing was answered")


# ---------------------------------------------------------------------------------------------

def run(tests):
    failed = 0
    for fn in tests:
        name = fn.__name__[2:].replace("_", " ")
        try:
            fn()
            print(f"  ok    {name}", flush=True)
        except Exception:                      # noqa: BLE001
            failed += 1
            print(f"  FAIL  {name}\n{traceback.format_exc()}", flush=True)
        finally:
            for g in GATES:                    # whatever the test did, let every start end
                g.release()
    return failed


if __name__ == "__main__":
    failed = run([t_before_init])
    expect_eq(LIB.hosi_runtime_init(), 0, "init")
    failed += run([
        t_init_twice, t_ping, t_abi_op, t_not_the_contract, t_uri_not_utf8, t_uri_not_the_shape,
        t_no_database_refuses, t_request_is_copied, t_never_on_the_calling_thread,
        t_cancel_meets_finish_from_threads, t_many_threads_submit, t_bad_handles,
        t_duplicate_id, t_limit, t_cancel_pending, t_close_answers_and_waits,
        t_connections_are_apart, t_sigint_stays_with_the_host,
        t_shutdown_with_waiting,
    ])
    failed += run([t_after_shutdown])
    for g in GATES:
        shutil.rmtree(g.dir, ignore_errors=True)
    print("all passed" if not failed else f"{failed} failed", flush=True)
    sys.exit(1 if failed else 0)
