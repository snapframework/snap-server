#include <errno.h>

void set_errno(int e) {
  errno = e;
}
