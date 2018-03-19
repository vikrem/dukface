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

#include <errno.h>
#include <pthread.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ucontext.h>

#include "duktape.h"


//static struct sigaction old_sa;
static __thread int threadTimeout = 0;

void sigpipe_handle(int x);
void install_handler(void);

void sigpipe_handle(int x) {
  threadTimeout = 1; // Set
}

void install_handler(void) {
  threadTimeout = 0; // Reset
  struct sigaction sa;
  memset(&sa, 0, sizeof(struct sigaction));
  sigemptyset(&sa.sa_mask);
  sa.sa_handler = sigpipe_handle;
  (void) sigaction(SIGPIPE, &sa, NULL);
}

void duktape_fatal_handler(void* udata, const char* msg) {
  fprintf(stderr, "\n** FATAL error in Duktape: %s. Forcing interrupt.\n", msg);
  fprintf(stderr, "\n** The application cannot continue. Halting \n");
  __builtin_trap();
  for(;;) {}
}

duk_bool_t dukTimeoutCheck(void* udata) {
  return threadTimeout;
  //__builtin_trap();
}
