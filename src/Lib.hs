{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where
    -- ( someFunc
    -- ) where

import qualified Language.C.Inline as C

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
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
data JSCallback -- TODO

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

type family Arity a :: Nat where
  Arity (r -> a) = 1 + Arity a
  Arity a = 0

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

injectFunc :: Dukkable f => f -> T.Text -> Duk ()
injectFunc fn name = do
  ctx' <- castPtr <$> gets ctx
  let name' = T.encodeUtf8 name
  wrappedFunc <- liftIO $ $(C.mkFunPtr [t|Ptr () -> IO C.CInt|]) $ entry fn . castPtr
  let numArgs = fromInteger $ getArgs fn
  liftIO $ [C.block| void {
    duk_push_global_object($(void* ctx'));
    duk_push_c_function($(void* ctx'), (int(*)(duk_context*))$(int (*wrappedFunc)(void*)), $(int numArgs));
    duk_put_prop_lstring($(void* ctx'), -2, $bs-ptr:name', $bs-len:name');
  }|]
  oldGc <- gets gc
  modify $ \e -> e {gc = freeHaskellFunPtr wrappedFunc >> oldGc}
  pure ()

class KnownNat (Arity f) => Dukkable f where
  getArgs :: KnownNat (Arity f) => f -> Integer
  getArgs _ = natVal (Proxy :: Proxy (Arity f))
  entry :: f -> DuktapeCtx -> IO C.CInt

instance Dukkable () where
  entry _ ctx = pushVal ctx Null >> return 1

instance {-# OVERLAPS #-} (ToJSON v, KnownNat (Arity v)) => Dukkable v where
  entry val ctx = pushVal ctx (toJSON val) >> return 1

instance (FromJSON a, KnownNat (Arity (a -> v)), Dukkable v) => Dukkable (a -> v) where
  entry f ctx = do
    let ctx' = castPtr ctx
    str <- join $ BS.packCString <$> [C.block|const char* {
              const char* ret = duk_json_encode($(void* ctx'), -1);
              duk_pop($(void* ctx'));
              return ret;
    }|]
    case decode (BSL.fromStrict str) of
       Nothing -> return $ [C.pure| int {DUK_RET_TYPE_ERROR}|]
       Just val -> entry (f val) ctx

-- instance (Dukkable r) => Dukkable (a -> r) where
--   entry _ = return 0

pushVal :: DuktapeCtx -> Value -> IO ()
pushVal ctx Null = let ctx' = castPtr ctx in [C.exp|void { duk_push_null($(void* ctx'))} |]
pushVal ctx (Bool True) = let ctx' = castPtr ctx in [C.exp|void { duk_push_boolean($(void* ctx'), 1)} |]
pushVal ctx (Bool False) = let ctx' = castPtr ctx in [C.exp|void { duk_push_boolean($(void* ctx'), 0)} |]
pushVal ctx (Number n) = [C.exp|void { duk_push_number($(void* ctx'), $(double n'))} |]
  where
    ctx' = castPtr ctx
    n' = realToFrac n
pushVal ctx (String s) = [C.exp|void { duk_push_lstring($(void* ctx'), $bs-ptr:s', $bs-len:s')} |]
  where
    s' = T.encodeUtf8 s
    ctx' = castPtr ctx
pushVal ctx (Array v) = do
  let ctx' = castPtr ctx
  root <- [C.exp|int { duk_push_array($(void* ctx')) }|]
  let pushElem e i = pushVal ctx e >> [C.exp|void { duk_put_prop_index($(void* ctx'), $(int root), $(int i)) }|]
  V.zipWithM_ pushElem v (V.enumFromN 0 $ V.length v)
pushVal ctx (Object o) = do
  let ctx' = castPtr ctx
  root <- [C.exp|int { duk_push_object($(void* ctx')) }|]
  forM_ (HMS.toList o) $ \(k, v) -> do
    let key = T.encodeUtf8 k
    pushVal ctx v
    [C.exp|void { duk_put_prop_lstring($(void* ctx'), $(int root), $bs-ptr:key, $bs-len:key) }|]

