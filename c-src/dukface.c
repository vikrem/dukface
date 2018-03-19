#include "duktape.h"
#include <stdio.h>


static void dump_stack(duk_context *ctx);

duk_int_t dh_safe_json_encode(duk_context *ctx);
static duk_ret_t wrapper(duk_context *ctx, void *udata);
// Tries to json encode topmost argument on the stack
// 1 on success, with json encoding on the top of the stack
// If it fails, it returns 0 and the error is on the top of the stack
duk_int_t dh_safe_json_encode(duk_context *ctx) {

  duk_bool_t can_enc = (duk_is_object_coercible(ctx, -1) || duk_is_null(ctx, -1)) && !duk_is_undefined(ctx, -1);
  duk_int_t ret = duk_safe_call(ctx, wrapper, NULL, 1, 1);
  if (can_enc && ret == DUK_EXEC_SUCCESS) {
    return 1;
  } else {
    // place an error
    duk_dup_top(ctx);
    const char *val = duk_safe_to_string(ctx, -1);
    duk_pop(ctx);
    duk_push_error_object(ctx, DUK_ERR_EVAL_ERROR, "Can't json-encode value: %s", val);
    return 0;
  }
}

static duk_ret_t wrapper(duk_context *ctx, void *udata) {
  duk_dup_top(ctx);
  duk_json_encode(ctx, -1);

  return 1;
}

static void dump_stack(duk_context *ctx) {
  duk_push_context_dump(ctx);
  const char* dbg = duk_safe_to_string(ctx, -1);
  fprintf(stderr, "%s\n", dbg);
  fflush(stderr);
  duk_pop(ctx);
}
