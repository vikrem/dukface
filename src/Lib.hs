{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( someFunc
    ) where

import qualified Language.C.Inline as C

C.include "duktape.h"

someFunc :: IO ()
someFunc = print =<< [C.block| int {
                         duk_context *ctx = duk_create_heap_default();
                         duk_eval_string(ctx, "40 + 22");
                         int i = duk_get_int(ctx, -1);
                         duk_destroy_heap(ctx);
            return i;
            }
            |]
