#include "HsConfig.h"
#include <sys/sendfile.h>

ssize_t lnxsendfile(int out_fd, int in_fd, off_t *offset, size_t count)
{
  return sendfile(out_fd, in_fd, offset, count);
}

