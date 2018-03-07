/*
  What does this do?

  Haskell cannot directly interrupt a foreign C call.
  All it can do is send a SIGPIPE to the thread executing the C call, and hope that will get
  the C call to return (with some error code possibly), and return to Haskell.

  If it doesn't do that, then the C code will continue to run.

  AFAICT Duktape does no signal handling. By default, the SIGPIPE handler installed by GHC does nothing.

  This installs a SIGPIPE handler that works as an exception mechanism, similar to try/catch.
  This then enables GHC to interrupt Duktape when an async exception is raised.
*/
#ifdef __APPLE__
  #define _XOPEN_SOURCE
#endif

#include <stdio.h>
#include <stdlib.h>
#include <ucontext.h>
#include <setjmp.h>
#include <pthread.h>
#include <signal.h>
#include <errno.h>


static __thread ucontext_t ctx;
static __thread int ctx_saved = 0;
static __thread volatile int ret;
static __thread struct sigaction old_sa;

/* static ucontext_t ctx; */
/* static int ctx_saved = 0; */
/* static volatile int ret; */
/* static struct sigaction old_sa; */

void sigpipe_handle(int x);
void install_handler(void);
int enable_interrupt(void);
void disable_interrupt(void);

void sigpipe_handle(int x) {
  ret = 1;
  if(ctx_saved)
    setcontext(&ctx);
}

void install_handler(void) {
  struct sigaction sa;
  memset(&sa, 0, sizeof(struct sigaction));
  sigemptyset(&sa.sa_mask);
  sa.sa_handler = sigpipe_handle;
  (void) sigaction(SIGPIPE, &sa, NULL);
  ret = 0;
}

int enable_interrupt(void) {
  install_handler();
  getcontext(&ctx);
  ctx_saved = 1;
  return ret;
}

void disable_interrupt(void) {
  getcontext(&ctx);
  (void) sigaction(SIGPIPE, &old_sa, NULL);
}

void force_interrupt(void) {
  setcontext(&ctx);
}

void duktape_fatal_handler(void* udata, const char* msg) {
  fprintf(stderr, "** FATAL error in duktape: %s", msg);
  force_interrupt();
}
