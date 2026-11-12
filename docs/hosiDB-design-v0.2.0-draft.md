# hosiDB design document (v0.2.0-draft)

Nov 11, 2026 · @ひなた

## Position and decisions

hosiDB is an embedded database engine. It does not hold on to the past, and it handles "now" lightly.

- A snapshot is a **frozen chunk of events**. A chunk holds only Create and Delete. Nobody changes a chunk. The system only adds.
- The **core material** (core) is "now": the fold of the chunks. The system can always build it again from the chunks.
- Delete is a freeze, and it keeps the history. To remove data for real, you use a separate operation, Erase. Erase invalidates a range or a bitmap, and garbage collection (GC) removes the data physically.
- The system decides whether to cool (freeze) a chunk from the way people use it, the capacity, and the free disk space. The unit that it watches is a group of "kind × month".
- Each job has its own tool. Local point lookups use an MPHF. S3 skipping uses a Binary Fuse filter.
- Distribution is not built now. Three margins stay open so that you can add it later.
- The system publishes daily numbers, so that an administrator can decide when to start to think about capacity.

The snapshot has two parts: the "chunks of events" and the "core material". Each chunk has an `mphf.idx`, which also holds the key check. Metrics have two kinds: for the local administrator, and anonymized metrics that people share.

## Pillars of the design

When in doubt, return to these five directions.

- **Run quietly.** Freezing and cleaning run a little at a time, when the system is idle. They do not disturb reads.
- **Do not change; add.** Nobody changes a chunk. To change data, write a new chunk and a new manifest, and swap them.
- **Fast for now; the past only when needed.** Try the fast path first. Open the deep path only when needed.
- **Freeze before you remove.** Delete keeps the history. Erase, which removes data for real, is a separate operation.
- **Receive at your own pace.** Future sync is a pull: the receiver pulls. The sender does not push.

## Overall shape

Events enter the tail log, get sealed into chunks, and move to the right as they cool. The core material is "now": the fold of those chunks. The system can always build it again.

Events cool in this order: tail log, hot chunk, cold chunk, S3. The system builds the core material by folding the chunks.

| Term | Meaning | Place |
| --- | --- | --- |
| Tail log (Tail) | The entrance for writes. It adds the events that are not yet sealed into a chunk, in order. | local |
| Hot chunk | A sealed, immutable chunk of events. It has an MPHF for point lookups. | local |
| Cold chunk | A frozen chunk. It is for scans. The system searches it with filters and zone maps. | local or S3 |
| S3 archive | Old chunks and pruned detail logs. | S3 |
| Core material (Core) | "Now": the fold of the chunks. It is derived, and the system can build it again. | local |
| manifest | Lists the chunks and the invalidation records that make up "the world now". It has a hash signature. | local |
| Invalidation record | The ID ranges and bitmaps of Erased data. The system does not change the chunks. It filters at read time. | sidecar of each chunk |

## Data model and ID

The system expands events into EAV triples and sorts them by kind, creation month, and ID. It weaves JSON-LD meaning and Matrix-style events into this shape.

- **Event kind**: Create and Delete. There is no Update. The first idea: a newer Create for the same entity wins (open).
- **ID**: an internal ID with the creation time inside, sortable by time. The ID alone tells you the month, so an ID lookup can also skip with the zone map.
- **IRIs from outside**: a dictionary maps them to internal IDs at import. Attribute names also go in a dictionary.
- **Order**: [kind] → [creation month YYYY-MM] → [ID]. The system does not sort by update date, because the place would move at each update.
- **JSON-LD details**: the placement of `@list` order, blank nodes, and language tags is decided in Phase 0 (open).

## Write path

A write goes into the tail log. The system seals the tail log into chunks in batches. A read looks at the core material and the unsealed tail log together, so "now" never stays old until the next batch.

- **Tail log**: adds events in order. It has a sequence number that shows the order.
- **Seal**: at a set size or age, the system turns the tail log into a hot chunk. It writes the chunk and a new manifest, and then it swaps the manifest.
- **Read**: the answer from the core material gets the tail-log part on top. If a Delete is there, the system treats it as a freeze.
- **If the system stops**: until the manifest swap, the old world stays. After the restart, the system seals again from where the tail log continues.
- **Batch**: writing in groups lowers the CPU and I/O load.

## Read path

The fast path answers "now". The system goes down to the deep path only when it needs to.

A question by ID goes to the fast path. Only questions about the past, or questions that stream out, go down to S3.

- **Fast path**: for an ID lookup, the MPHF returns a number, and the system compares the key in that place. If the key matches, the system checks the invalidation record, and if the key is not invalidated, it answers. A search by attribute and value uses block-sorted order and a sparse index.
- **Deep path**: it opens only for the past, and for streaming to an external search engine. First it skips whole chunks with the zone map in the manifest and with the chunk filter. For the remaining chunks, it gets only the footer with a Range GET, and it narrows further with the block filter.
- **Why the MPHF is not enough**: an MPHF returns some number even for a key that is not in the set. Always compare the key (or a fingerprint).
- **Streaming**: when the system streams out, it does not write locally. It only passes the data through a pipeline.

## Files and formats

One chunk is one directory and is self-contained. The formats follow the ideas of Parquet, Iceberg, and HDT. The file names are provisional.

| File | Content | Hot chunk / cold chunk |
| --- | --- | --- |
| `chunk.hosi` | Dictionary-encoded triples, sorted by block, with statistics in the footer | both (a cold chunk has bigger blocks and stronger compression) |
| `mphf.idx` | The MPHF, and the fingerprints for the key check | hot chunks only |
| `filter.bin` | A Binary Fuse filter, for a first guess about a key | always for a cold chunk; optional for a hot chunk |
| `erase.bin` | The invalidation record (ID ranges and a Roaring bitmap) | only the chunks that need it |
| `manifest` | The list of chunks, hashes, zone maps, and filter summaries | one for the world |

- The manifest holds statistics, so the system skips whole chunks before it gets a footer (the two-step approach of Iceberg).
- Invalidation follows the idea of the Iceberg v3 deletion vector (a Roaring bitmap in a sidecar). Ranges are added for bulk operations.
- The MPHF for point lookups is PtrHash, PTHash, or RecSplit. The key check is always done.
- The filter of a cold chunk is a Binary Fuse filter. Ribbon is considered when the size of the manifest becomes a problem.
- The triple layout follows HDT: a dictionary and the triples are separate. Roaring and Elias-Fano make ID sets and sequences small.

## Freezing

The system freezes cold chunks together in a cleaning run. It looks at the way people use the data, the capacity, and the free disk space. The unit is a group of kind × month.

The decision is made for each group of kind × month, only in a cleaning run.

- **Coldness**: the time since the last random access, no writes, and the ratio of random to sequential access. The read count is a count in which old reads fade.
- **Capacity pressure**: the less free space, the looser the coldness rule, and the earlier the freeze. But if there is no room to rewrite, the system goes on from the smallest chunk.
- **Free disk capacity**: if fast-path reads are slower than usual, the system steps back and waits.
- **What freezing changes**: the order becomes scan-friendly, with bigger blocks and stronger compression. The MPHF is removed, and only the filter and the zone map stay.
- **Thaw**: when access grows, the system copies the chunk into a new hot chunk. Nobody changes a chunk. To avoid back-and-forth, the thaw rule is a little stricter than the freeze rule, or there is a grace period.
- **How to record**: access counts are kept in memory and written together to a separate file. They are not written inside an immutable chunk.
- **First version**: only "no random access and no writes for a set time". The sequential ratio, and the capacity and free-space rules, come later.

## Delete and Erase

Delete is a freeze and keeps the history. To remove data for real, use the separate operation Erase.

|  | Delete | Erase |
| --- | --- | --- |
| Meaning | A freeze that says "not visible from here" | Remove physically |
| Mechanism | Add one event | Invalidate a range or a bitmap; rewrite in the cleaning run |
| History | Stays | Gone |
| Visible in "now" | No | No |

- **Invalidation record**: the system does not change the chunk. It adds "the ranges of removed IDs" and "a bitmap of single IDs" in a sidecar. The record holds only IDs, no content. A range suits a bulk job such as "everything of this user". A bitmap suits single deletes.
- **Read order**: filter → MPHF → key check → invalidation check. Until the cleaning run, the filter and the MPHF still react to a removed key, but the last check rejects it.
- **Cleaning (GC)**: it rewrites only the chunks that overlap an invalidated range. The unit is kind × month, so the touched area is small. It writes the new chunk and the manifest, swaps them, and then drops the old ones. It also builds the filter and the MPHF again, only for the rewritten chunks.
- **When you want removal at once**: there is a way to run the cleaning by hand, now.
- **Note on S3**: if bucket versioning is on, old versions stay. The Erase is complete only after the old versions are deleted.

## Cleaning (maintenance)

One cleaning run includes freezing, Erase, and filter rebuilds. One run a day is an upper limit, not a fixed schedule.

1. Look at coldness, capacity, and free space, and choose the chunks to touch in this run.
2. Write the chunks to freeze, and the chunks that overlap an Erase range, as new chunks.
3. Build the filters and the MPHFs again, only for the rewritten chunks.
4. Write a new manifest and swap it in one step.
5. Drop the old chunks. For S3, count the old versions too.

- **Run when idle**: the system watches the delay of fast-path reads and waits if reads are slow. It does not use OS statistics.
- **When capacity is at risk**: it runs at a reduced speed and does not wait.
- **If the system stops**: before step 4, the old world stays. A new run gives the same result.
- **Network**: the upload to S3 is slowed down in the same way.

## Metrics and capacity outlook

The system publishes only numbers, every day, so that an administrator can decide when to start to think about capacity. Decisions and notifications belong to the administrator's tools. The metrics hold only sizes and counts. They hold no content and no IDs.

| Number | Meaning |
| --- | --- |
| Breakdown by place | Core material, hot chunks, cold chunks, tail log, invalidation records, cleaning scratch space |
| Growth rate | What grew by import, what shrank by freezing and GC, and the net |
| Real remaining space | Free disk space minus the margin that cleaning needs |
| Recovery forecast | What is cold but not frozen, and what can move to S3 |
| S3 size and growth rate | Counted in the same way as local data |
| Freeze progress | The share of cold units that are frozen |

- **Margin**: the space to rewrite the biggest chunk. Cleaning can become impossible before the disk is full.
- **Outlook**: "at this speed, how many days until the real remaining space is zero", with a cautious estimate and a normal estimate.
- **Notification target**: earlier than the number of days that the administrator needs to react (a setting).
- **How often to measure**: about once an hour, and at each cleaning or freeze. The system keeps about 90 days of daily records, so the slope is visible even with weekly waves.
- **Output**: JSON first.
- **Separate from shared metrics**: anonymized shared metrics (fast-to-deep ratio, pruning rate, skip rate, and so on) are a separate flow outside the engine. They are off by default. Each node chooses after it sees a preview, one month later.

## Margin for distribution

Distribution is not built now. Keep these three rules from the start, so that you can add it later.

- **Keep the chunk hashes in the manifest**: you identify a chunk by its content, and you can exchange only the chunks that differ.
- **Unique, time-sortable IDs**: they do not collide across nodes.
- **Do not change chunks**: sync becomes only "receive new chunks".

The plan for later:

- **Pull**: `PullTicket` is a pair of "a manifest hash + send me from here on".
- **Interaction with pruning**: if the system also drops the delete records (Delete and Erase), a sync from an old node brings back removed data. Set a pruning horizon. A node older than the horizon takes a full snapshot again, not a diff.
- **Candidates**: a content-addressed tree (Prolly Tree or Merkle Search Tree) exchanges only the parts that differ.

## Roadmap

There are six phases. There is no time estimate. Each phase has a sign that it is done.

| Phase | Work | Sign of completion |
| --- | --- | --- |
| Phase 0 | Write about 10 queries that you really want to run. Decide ID, Update, and JSON-LD. | You can say which index answers each of the 10 queries. |
| Phase 1 | Chunks, tail log, manifest, core material. Point lookup by MPHF (with key check). | Write, seal, restart, and get the same answers. |
| Phase 2 | S3 and the deep path. Zone maps, Binary Fuse filters, Range GET. | A question about the past skips the chunks that it does not need to read. |
| Phase 3 | Freezing, Erase, cleaning. The first coldness rule, invalidation records, GC. | After cleaning, an Erased ID is physically gone. |
| Phase 4 | Metrics and capacity outlook. Daily JSON, real remaining space, days left. | An administrator can decide when to start to think, from the JSON alone. |
| Phase 5 | Sharing of anonymized metrics (off by default, chosen after one month), and a screen prototype for the "thank you" contribution display. | The shared content is visible in a preview, and the node can choose. |

## Open points

- **Update**: the first idea is that a newer Create for the same entity wins.
- **JSON-LD**: how to place `@list` order, blank nodes, and language tags in triples.
- **ID scheme**: the exact shape of the time-sortable ID, and the dictionary for external IRIs.
- **Query shape**: how much Datalog to support. The join implementation, and the attribute and value indexes.
- **MPHF choice**: PtrHash, PTHash, or RecSplit.
- **Implementation language**, and how to use it from each language.
- **Compression method.**
- **Default values of the freezing thresholds**, and the way to estimate the outlook.
- **The size and age at which the tail log is sealed.**
