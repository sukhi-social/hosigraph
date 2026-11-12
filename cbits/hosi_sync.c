/* hosigraph: tiny POSIX helpers so we can make files and directories durable.
 * Haskell's Handle has hFlush (user space -> OS) but no fsync (OS -> disk). */
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

/* fsync a regular file by path.  Returns 0 on success, -1 (errno set) on failure. */
int hosi_fsync_path(const char *path) {
  int fd = open(path, O_RDONLY);
  if (fd < 0) return -1;
  int r = fsync(fd);
  int saved = errno;
  close(fd);
  errno = saved;
  return r;
}

/* fsync a directory so that creates / renames inside it survive a crash. */
int hosi_fsync_dir(const char *path) {
  int fd = open(path, O_RDONLY | O_DIRECTORY);
  if (fd < 0) return -1;
  int r = fsync(fd);
  int saved = errno;
  close(fd);
  errno = saved;
  return r;
}
