#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

#include <signal.h>

static jmp_buf g_exc_handle;
static struct sigaction g_old_sa;

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

void sigpipe_handle(int x) {
  /* fprintf(stderr, "Got signal %d\n", x); */
  /* fflush(stderr); */
  (void) sigaction(SIGPIPE, &g_old_sa, NULL);
  longjmp(g_exc_handle, 1);
}

void install_handler(void) {
  struct sigaction sa;
  sa.sa_handler = sigpipe_handle;
  sigemptyset(&sa.sa_mask);
  (void) sigaction(SIGPIPE, &sa, &g_old_sa);
}

int interrupted(void) {
  return setjmp(g_exc_handle);
}
