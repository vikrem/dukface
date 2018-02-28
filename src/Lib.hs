{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InterruptibleFFI #-}

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
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Exception.Safe
import Control.Concurrent hiding (throwTo)
import Control.Concurrent.Async

import Control.Concurrent.MVar

import GHC.TypeLits
import Data.Proxy
import System.Random

import Data.Aeson
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HMS

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx)
C.include "duktape.h"

data DuktapeHeap
type DuktapeCtx = Ptr DuktapeHeap

newtype JSCallback a = MkJSCallback { jsCallbackKey :: T.Text } deriving Show

data DukType =
    DukNone
  | DukUndefined
  | DukNull
  | DukJSON Value
  -- | DukBuffer
  -- | DukPointer
  -- | DukLightFunc

data DukEnv = DukEnv { ctx :: DuktapeCtx, main :: ThreadId, gc :: IO (), exc :: MVar SomeException  }
data DukCallEnv = DukCallEnv { dceCtx :: DuktapeCtx, dceExc :: MVar SomeException }

type Duk a = StateT DukEnv IO a
type DukCall a = ReaderT DukCallEnv IO a

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
  excBox <- newEmptyMVar
  (ret, newE) <- bracket
    ((castPtr <$> [C.exp| void* {duk_create_heap_default()}|]) :: IO DuktapeCtx)
    cleanup
    (\ctx -> runStateT dk $ DukEnv ctx me (pure ()) excBox)
  gc newE
  tryReadMVar excBox >>= \case
    Just e -> throwM e >> return ret
    Nothing -> return ret

execJS :: FromJSON v => T.Text -> Duk v
execJS code = do
  ctx' <- castPtr <$> gets ctx
  let bs = T.encodeUtf8 code
  errCode <- lift $ [C.exp| int {duk_peval_lstring($(void* ctx'), $bs-ptr:bs, $bs-len:bs)}|]
  lift $ when (errCode /= 0) $ do
    err <- [C.exp| const char* { duk_safe_to_string($(void* ctx'), -1) }|]
    peekCString err >>= throwString
  pArr <- lift $ [C.block| const char* {
    duk_peval_lstring($(void* ctx'), $bs-ptr:bs, $bs-len:bs);
    return duk_safe_to_string($(void* ctx'), -1);
  }|]
  s <- liftIO $ BS.packCString pArr
  case eitherDecode $ BSL.fromStrict s of
    Left err -> throwString err
    Right v -> return v

injectFunc :: Dukkable f => f -> T.Text -> Duk ()
injectFunc fn name = do
  ctx <- gets ctx
  let ctx' = castPtr ctx
  excBox <- gets exc
  let name' = T.encodeUtf8 name
  let runFunc = runReaderT (entry fn) (DukCallEnv ctx excBox) `catchAny` \(e :: SomeException) -> do
        void $ tryPutMVar excBox e
        return $ [C.pure| int {DUK_RET_EVAL_ERROR}|]
  wrappedFunc <- liftIO $ $(C.mkFunPtr [t|Ptr () -> IO C.CInt|]) $ const runFunc
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
  entry :: f -> DukCall C.CInt

instance Dukkable () where
  entry _ = do { ctx <- asks dceCtx; liftIO $ pushVal ctx Null; return 1}

instance {-# OVERLAPS #-} (ToJSON v, KnownNat (Arity v)) => Dukkable v where
  entry val = do { ctx <- asks dceCtx; liftIO $ pushVal ctx (toJSON val); return 1 }

instance (FromJSON a, KnownNat (Arity (a -> v)), Dukkable v) => Dukkable (a -> v) where
  entry f = do
    ctx' <- castPtr <$> asks dceCtx
    str <- liftIO $ join $ BS.packCString <$> [C.block|const char* {
              const char* ret = duk_json_encode($(void* ctx'), -1);
              duk_pop($(void* ctx'));
              return ret;
    }|]
    case decode (BSL.fromStrict str) of
       Nothing -> return $ [C.pure| int {DUK_RET_TYPE_ERROR}|]
       Just val -> entry (f val)

instance {-# OVERLAPS #-} (KnownNat (Arity ((JSCallback a) -> v)), Dukkable v) => Dukkable (JSCallback a -> v) where
  entry f = do
    ctx' <- castPtr <$> asks dceCtx
    rand_str <- liftIO $ T.encodeUtf8 . T.pack . show <$> getStdRandom (randomR (1 :: Integer, 99999999))
    isFunc <- liftIO [C.exp|int { duk_is_function($(void* ctx'), -1) } |]
    case isFunc of
      1 -> do
        liftIO $ [C.exp| void { duk_put_global_lstring($(void* ctx'), $bs-ptr:rand_str, $bs-len:rand_str) }|]
        let cb = MkJSCallback . T.decodeUtf8 $ rand_str
        liftIO $ print cb
        entry (f cb)
      _ -> return $ [C.pure| int {DUK_RET_TYPE_ERROR}|]

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

