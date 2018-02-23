{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( someFunc
    ) where

import qualified Language.C.Inline as C

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Foreign.ForeignPtr
import Foreign.Ptr
import Control.Concurrent.MVar
import Data.Monoid

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx)
C.include "duktape.h"

data DuktapeHeap
type DuktapeCtx = Ptr DuktapeHeap

data Duk a = MkDuk { unDuk :: DuktapeCtx -> IO a }

someFunc :: IO ()
someFunc = print =<< [C.block| int {
                         duk_context *ctx = duk_create_heap_default();
                         duk_eval_string(ctx, "40 + 22");
                         int i = duk_get_int(ctx, -1);
                         duk_destroy_heap(ctx);
            return i;
            }
            |]
cleanup :: Ptr DuktapeHeap -> IO ()
cleanup ctx = [C.exp| void { duk_destroy_heap($(void* ctx')) }|]
  where
   ctx' = castPtr ctx

runDuk :: Duk a -> IO a
runDuk dk = do
  ptr <- [C.exp| void* { duk_create_heap_default() } |]
  (ctx :: ForeignPtr DuktapeHeap) <- castForeignPtr <$> newForeignPtr_ ptr
  finalizer <- $(C.mkFunPtr [t|Ptr DuktapeHeap -> IO ()|]) cleanup
  addForeignPtrFinalizer finalizer ctx
  withForeignPtr ctx $ unDuk dk


execJS :: Duk C.CUInt
execJS = MkDuk $ \ctx ->
  let ctx' = castPtr ctx in
  [C.block| unsigned int {
    duk_eval_string($(void* ctx'), "return 42;");
    return duk_get_uint($(void* ctx'), -1);
  }|]
