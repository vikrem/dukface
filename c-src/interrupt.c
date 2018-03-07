#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <pthread.h>
#include <signal.h>
#include <errno.h>

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

static __thread jmp_buf jmp;
static __thread struct sigaction old_sa;

void sigpipe_handle(int x);
void install_handler(void);
int enable_interrupt(void);
void disable_interrupt(void);

void sigpipe_handle(int x) {
  (void) longjmp(jmp, 1);
}

void install_handler(void) {
  struct sigaction sa;
  sa.sa_handler = sigpipe_handle;
  sigemptyset(&sa.sa_mask);
  (void) sigaction(SIGPIPE, &sa, NULL);
}

int enable_interrupt(void) {
  install_handler();
  return setjmp(jmp);
}

void disable_interrupt(void) {
  (void) sigaction(SIGPIPE, &old_sa, NULL);
}
