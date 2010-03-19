#include <sys/socket.h>
#include <sys/time.h>


void set_linger(int fd) {
    struct linger linger;

    /* five seconds of linger */
    linger.l_onoff = 1;
    linger.l_linger = 5;

    setsockopt(fd, SOL_SOCKET, SO_LINGER, &linger, sizeof(linger));
}


void set_fd_timeout(int fd) {
    struct timeval timeout;
    timeout.tv_sec = 10;
    timeout.tv_usec = 0;

    setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof(timeout));
    setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
}
