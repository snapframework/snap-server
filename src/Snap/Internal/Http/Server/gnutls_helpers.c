#include <gnutls/gnutls.h>
#include <gcrypt.h>
#include <errno.h>
#include <pthread.h>
GCRY_THREAD_OPTION_PTHREAD_IMPL;

/* See http://www.gnu.org/software/gnutls/manual/html_node/Multi_002dthreaded-applications.html */

static int threading_init = 0;
          
void gnutls_set_threading_helper()
{
    if (!threading_init) {
        gcry_control (GCRYCTL_SET_THREAD_CBS, &gcry_threads_pthread);
        threading_init = 1;
    }
}
