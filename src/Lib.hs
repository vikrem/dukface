{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib where
    -- ( someFunc
    -- ) where

import qualified Language.C.Inline as C

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.Concurrent
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Control.Concurrent.MVar
import Data.Monoid

import Control.Monad.State
import Control.Monad.State.Class
import Control.Exception.Safe
import Control.Concurrent hiding (throwTo)
import Control.Concurrent.Async

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx)
C.include "duktape.h"

data DuktapeHeap
type DuktapeCtx = Ptr DuktapeHeap

data DukType =
    DukNone
  | DukUndefined
  | DukNull
  | DukBoolean
  | DukNumber
  | DukString
  | DukObject
  | DukBuffer
  | DukPointer
  | DukLightFunc

data DukEnv = DukEnv { ctx :: DuktapeCtx }

type Duk a = StateT DukEnv IO a

cleanup :: Ptr DuktapeHeap -> IO ()
cleanup ctx = [C.exp| void { duk_destroy_heap($(void* ctx')) }|]
  where
   ctx' = castPtr ctx

runDuk :: Duk a -> IO a
runDuk dk = do
  victim <- myThreadId
  fatalHandler <- $(C.mkFunPtr [t|Ptr () -> CString -> IO ()|]) $ captureExc victim
  (ctx :: DuktapeCtx) <- castPtr <$> [C.block| void* {
      return duk_create_heap(NULL, NULL, NULL, 0, $(void (*fatalHandler)(void*, const char*)) );
    }|]
  (ret, _) <- runStateT dk $ DukEnv ctx
  cleanup ctx
  freeHaskellFunPtr fatalHandler
  return ret
  where
    captureExc :: ThreadId -> a -> CString -> IO ()
    captureExc target _ msg = (peekCString msg >>= throwString) `catchAny` \(e :: SomeException) -> throwTo target e

execJS :: T.Text -> Duk String
execJS code = do
  ctx' <- castPtr <$> gets ctx
  let bs = T.encodeUtf8 code
  pArr <- lift $ [C.block| const char* {
    duk_peval_lstring($(void* ctx'), $bs-ptr:bs, $bs-len:bs);
    return duk_to_string($(void* ctx'), -1);
  }|]
  lift $ peekCString pArr
