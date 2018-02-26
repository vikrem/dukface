{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

import GHC.TypeLits
import Data.Proxy

import Data.Aeson
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HMS

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx)
C.include "duktape.h"

data DuktapeHeap
type DuktapeCtx = Ptr DuktapeHeap

data DukType =
    DukNone
  | DukUndefined
  | DukNull
  | DukJSON Value
  -- | DukBuffer
  -- | DukPointer
  -- | DukLightFunc

data DukEnv = DukEnv { ctx :: DuktapeCtx, main :: ThreadId, gc :: IO () }

type Duk a = StateT DukEnv IO a

cleanup :: DuktapeCtx -> IO ()
cleanup ctx = [C.exp| void { duk_destroy_heap($(void* ctx')) }|]
  where
   ctx' = castPtr ctx

runDuk :: Duk a -> IO a
runDuk dk = do
  me <- myThreadId
  (ret, newE) <- bracket
    ((castPtr <$> [C.exp| void* {duk_create_heap_default()}|]) :: IO DuktapeCtx)
    cleanup
    (\ctx -> runStateT dk $ DukEnv ctx me (pure ()))
  gc newE
  return ret

execJS :: T.Text -> Duk String
execJS code = do
  ctx' <- castPtr <$> gets ctx
  let bs = T.encodeUtf8 code
  pArr <- lift $ [C.block| const char* {
    duk_peval_lstring($(void* ctx'), $bs-ptr:bs, $bs-len:bs);
    return duk_to_string($(void* ctx'), -1);
  }|]
  lift $ peekCString pArr

injectFunc :: (DuktapeCtx -> IO C.CInt) -> T.Text -> Duk ()
injectFunc fn name = do
  ctx' <- castPtr <$> gets ctx
  let name' = T.encodeUtf8 name
  wrappedFunc <- liftIO $ $(C.mkFunPtr [t|Ptr () -> IO C.CInt|]) $ fn . castPtr
  liftIO $ [C.block| void {
    duk_push_global_object($(void* ctx'));
    duk_push_c_function($(void* ctx'), $(int (*wrappedFunc)(void*)), 0);
    duk_put_prop_lstring($(void* ctx'), -2, $bs-ptr:name', $bs-len:name');
  }|]
  oldGc <- gets gc
  modify $ \e -> e {gc = freeHaskellFunPtr wrappedFunc >> oldGc}
  -- TODO clean this ^
  pure ()

class Dukkable f where
  type NumArgs f :: Nat
  getArgs :: KnownNat (NumArgs f) => f -> Integer
  getArgs _ = natVal (Proxy :: Proxy (NumArgs f))

instance Dukkable () where
  type NumArgs () = 0

instance (Dukkable r) => Dukkable (a -> r) where
  type NumArgs (a -> r) = 1 + NumArgs r

pushVal :: Value -> Duk ()
pushVal Null = (castPtr <$> gets ctx) >>= \ctx -> liftIO [C.exp|void { duk_push_null($(void* ctx))} |]
pushVal (Bool True) = (castPtr <$> gets ctx) >>= \ctx -> liftIO [C.exp|void { duk_push_boolean($(void* ctx), 1)} |]
pushVal (Bool False) = (castPtr <$> gets ctx) >>= \ctx -> liftIO [C.exp|void { duk_push_boolean($(void* ctx), 0)} |]
pushVal (Number n) = let n' = realToFrac n in (castPtr <$> gets ctx) >>= \ctx -> liftIO [C.exp|void { duk_push_number($(void* ctx), $(double n'))} |]
pushVal (String s) = let s' = T.encodeUtf8 s in (castPtr <$> gets ctx) >>= \ctx -> liftIO [C.exp|void { duk_push_lstring($(void* ctx), $bs-ptr:s', $bs-len:s')} |]
pushVal (Array v) = do
  ctx' <- castPtr <$> gets ctx
  root <- liftIO [C.exp|int { duk_push_array($(void* ctx')) }|]
  let pushElem e i = pushVal e >> liftIO [C.exp|void { duk_put_prop_index($(void* ctx'), $(int root), $(int i)) }|]
  V.zipWithM_ pushElem v (V.enumFromN 0 $ V.length v)
pushVal (Object o) = do
  ctx' <- castPtr <$> gets ctx
  root <- liftIO [C.exp|int { duk_push_object($(void* ctx')) }|]
  forM_ (HMS.toList o) $ \(k, v) -> do
    let key = T.encodeUtf8 k
    pushVal v
    liftIO $ [C.exp|void { duk_put_prop_lstring($(void* ctx'), $(int root), $bs-ptr:key, $bs-len:key) }|]
