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


static __thread ucontext_t ctx;
static __thread volatile int ret;
static __thread volatile int int_enabled = 0;
static struct sigaction old_sa;

void sigpipe_handle(int x);
void install_handler(void);
int enable_interrupt(void);
void disable_interrupt(void);

void sigpipe_handle(int x) {
  ret = 1;
  if (setcontext(&ctx) == -1) {fprintf(stderr, "Setcontext failed!\n");}
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
  int_enabled = 1;
  if (getcontext(&ctx) == -1) {fprintf(stderr, "Getcontext failed!\n");}
  //fprintf(stderr, "Interrupting with return: %d\n", ret);
  return ret;
}

void disable_interrupt(void) {
  ret = 0;
  int_enabled = 0;
  if (getcontext(&ctx) == -1) {fprintf(stderr, "Getcontext failed!\n");}
  (void) sigaction(SIGPIPE, &old_sa, NULL);
}

void force_interrupt(void) {
  ret = 1;
  if (!int_enabled) {fprintf(stderr, "Interrupts disabled! Can't safely return.\n");}
  if (setcontext(&ctx) == -1) {fprintf(stderr, "Setcontext failed!\n");}
}

void duktape_fatal_handler(void* udata, const char* msg) {
  // fprintf(stderr, "\n** FATAL error in Duktape: %s. Forcing interrupt.\n", msg);
  force_interrupt();
}
