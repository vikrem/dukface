{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Lib where
    -- ( someFunc
    -- ) where

import qualified Language.C.Inline.Interruptible as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline as C (context, include, mkFunPtr)
import qualified Foreign.C.Types as C

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.Ptr
import Foreign.C.String
import Data.Monoid

import Control.Monad.State
import Control.Monad.Reader
import Control.Exception.Safe
import Control.Concurrent hiding (throwTo)
import Control.Concurrent.Async

import GHC.TypeLits
import Data.Proxy
import System.Random

import Data.Aeson
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HMS

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx)
C.include "duktape.h"
C.include "interrupt.h"

data DuktapeHeap
type DuktapeCtx = Ptr DuktapeHeap

newtype JSCallback a = MkJSCallback { jsCallbackKey :: T.Text } deriving Show

-- Top level Duk execution env
data DukEnv = DukEnv {
  ctx :: DuktapeCtx,
  mainThread :: ThreadId,
  gc :: IO (),
  exc :: MVar SomeException,
  tasks :: MVar (Task ()),
  workers :: MVar Int
  }

-- Environment for HS callbacks and scheduled tasks
data DukCallEnv = DukCallEnv {
  dceMain :: ThreadId,
  dceCtx :: DuktapeCtx,
  dceExc :: MVar SomeException,
  dceTasks :: MVar (Task ()),
  dceWorkers :: MVar Int
  }

data JSCall = JSCall
-- Duk a executes a script and returns the last value of the last task, a
type Duk a = StateT DukEnv IO a

-- DukCall a is a Haskell function that returns a value of a to JS
type DukCall a = ReaderT DukCallEnv IO a
-- Also used for re-entry tasks
type Task a = DukCall a

type family Arity a :: Nat where
  Arity (r -> a) = 1 + Arity a
  Arity a = 0

cleanup :: DuktapeCtx -> IO ()
cleanup ctx = [C.exp| void { duk_destroy_heap($(void* ctx')) }|]
  where
   ctx' = castPtr ctx

-- Promote a nested JS execution action to an action in the JS main thread
dukLift :: DukCall a -> Duk a
dukLift dc = do
  main <- gets mainThread
  ctx <- gets ctx
  excBox <- gets exc
  tasks <- gets tasks
  workers <- gets workers
  liftIO $ runReaderT dc $ DukCallEnv main ctx excBox tasks workers

runDuk :: Duk a -> IO a
runDuk dk = do
  me <- myThreadId
  excBox <- newEmptyMVar
  taskQ <- newEmptyMVar
  numWorkers <- newMVar 0
  (ret, newE) <- bracket
    (castPtr <$> [C.block| void* {
        return duk_create_heap(NULL, NULL, NULL, 0, &duktape_fatal_handler);
      }|])
    (cleanup)
    (\ctx -> do
        retme <- runStateT dk $ DukEnv ctx me (pure ()) excBox taskQ numWorkers
        let loop = do
              works <- readMVar numWorkers
              case works of
                0 -> pure ()
                _ -> do
                  t <- takeMVar taskQ
                  runReaderT t $ DukCallEnv me ctx excBox taskQ numWorkers
                  void $ liftIO $ modifyMVar_ numWorkers (\n -> return $! (n - 1))
                  loop
        loop
        return retme
    ) `catch` \(e :: SomeException) -> do
        tryReadMVar excBox >>= \case
          Just e' -> throwM e' >> throwM e
          Nothing -> throwM e
  gc newE
  tryReadMVar excBox >>= \case
    Just e -> throwM e >> return ret
    Nothing -> return ret

execJS :: FromJSON v => T.Text -> DukCall v
execJS code = do
  ctx' <- castPtr <$> asks dceCtx
  let bs = T.encodeUtf8 code
  errCode <- lift $ [C.block| int {
    install_handler();
    volatile int ret = enable_interrupt();
    if(!ret)
    {
      duk_eval_lstring($(void* ctx'), $bs-ptr:bs, $bs-len:bs);
    } else {
      return 1;
    }
    return 0;
  }|]
  lift $ when (errCode /= 0) $ do
    err <- [C.exp| const char* { duk_safe_to_string($(void* ctx'), -1) }|]
    peekCString err >>= throwString
  pArr <- lift $ [C.exp| const char* {duk_json_encode($(void* ctx'), -1)}|]
  s <- liftIO $ BS.packCString pArr
  case eitherDecode $ BSL.fromStrict s of
    Left err -> throwString $ err ++ " # trying to parse: " ++ (T.unpack . T.decodeUtf8 $ s)
    Right v -> return v

injectFunc :: Dukkable f => f -> [T.Text] -> T.Text -> Duk ()
injectFunc fn path name = do
  ctx <- gets ctx
  let ctx' = castPtr ctx
  main <- gets mainThread
  excBox <- gets exc
  taskQ <- gets tasks
  numWorkers <- gets workers
  let runFunc = runReaderT (entry fn) (DukCallEnv main ctx excBox taskQ numWorkers) `catchDeep` \(e :: SomeException) -> do
        void $ putMVar excBox e
        return $ [C.pure| int {DUK_RET_EVAL_ERROR}|]
  wrappedFunc <- liftIO $ $(C.mkFunPtr [t|Ptr () -> IO C.CInt|]) $ const runFunc
  let numArgs = fromInteger $ getArgs fn
  let name' = T.encodeUtf8 name
  liftIO $ [C.exp| void {duk_push_global_object($(void* ctx'))}|]
  liftIO $ walk ctx' path
  liftIO $ [C.block| void {
    duk_push_c_function($(void* ctx'), (int(*)(duk_context*))$(int (*wrappedFunc)(void*)), $(int numArgs));
    duk_put_prop_lstring($(void* ctx'), -2, $bs-ptr:name', $bs-len:name');
  }|]
  oldGc <- gets gc
  modify $ \e -> e {gc = freeHaskellFunPtr wrappedFunc >> oldGc}
  pure ()
  where
    walk ctx paths = forM_ paths $ \prop -> do
      let enc = T.encodeUtf8 prop
      code <- [C.exp| int { duk_get_prop_lstring($(void* ctx), -1, $bs-ptr:enc, $bs-len:enc) }|]
      when (code == 0) $ throwString $ T.unpack $ "Couldn't traverse property " <> prop

addEvent :: DukCall (DukCall ()) -> DukCall ()
addEvent dc = do
  env <- ask
  tasks <- asks dceTasks
  excBox <- asks dceExc
  numworkers <- asks dceWorkers
  void $ liftIO $ modifyMVar_ numworkers (\n -> return $! n + 1)
  void $ liftIO $ async $ (do
      ret <- runReaderT dc env
      void $ putMVar tasks ret
                          )
    `catchAny` \(e :: SomeException) -> void $ tryPutMVar excBox e

class KnownNat (Arity f) => Dukkable f where
  getArgs :: KnownNat (Arity f) => f -> Integer
  getArgs _ = natVal (Proxy :: Proxy (Arity f))
  entry :: f -> DukCall C.CInt

instance {-# OVERLAPS #-} (ToJSON v, KnownNat (Arity v)) => Dukkable (DukCall v) where
  entry act = do
    val <- act
    ctx <- asks dceCtx
    liftIO $ pushVal ctx (toJSON val)
    return 1

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
        entry (f cb)
      _ -> return $ [C.pure| int {DUK_RET_TYPE_ERROR}|]

class (KnownNat (Arity a)) => DukCallable a where
  evalCallback :: JSCallback a -> C.CInt -> DukCall () -> CBExpand a
  makeCall :: JSCallback a -> C.CInt -> DukCall C.CInt
  makeCall (MkJSCallback name) arity = do
    ctx <- castPtr <$> asks dceCtx
    let name' = T.encodeUtf8 name
    -- Stack order:
    -- <- Bottom , func , arg1, arg2, argn | TOP
    liftIO $ [C.block| int {
                        duk_get_global_lstring($(void* ctx), $bs-ptr:name', $bs-len:name');
                        duk_insert($(void *ctx), -$(int arity) - 1);
                        duk_ret_t ret = duk_pcall($(void* ctx), $(int arity));
                        duk_push_null($(void* ctx));
                        duk_put_global_lstring($(void* ctx), $bs-ptr:name', $bs-len:name');
                        return ret;
      } |]

type family CBExpand a where
  CBExpand (a -> b) = a -> CBExpand b
  CBExpand a = DukCall a

instance {-# OVERLAPPABLE #-} (FromJSON a, KnownNat (Arity a), CBExpand a ~ DukCall a) => DukCallable a where
  evalCallback cb ar accum = do
    accum
    void $ makeCall cb ar
    ctx' <- castPtr <$> asks dceCtx
    pArr <- lift $ [C.block| const char* {
      return duk_safe_to_string($(void* ctx'), -1);
    }|]
    s <- liftIO $ BS.packCString pArr
    case eitherDecode $ BSL.fromStrict s of
      Left err -> throwString err
      Right v -> return v

instance (ToJSON b, DukCallable a, KnownNat (Arity (b -> a))) => DukCallable (b -> a) where
  evalCallback (MkJSCallback name) ar accum b = evalCallback nextCb ar accum'
    where
      accum' = do
        accum
        ctx <- asks dceCtx
        liftIO $ pushVal ctx $ toJSON b
      nextCb = MkJSCallback name :: JSCallback a

evalCallback' :: forall a. (KnownNat (Arity a), DukCallable a) => JSCallback a -> CBExpand a
evalCallback' cb = evalCallback cb (fromInteger $ natVal p) (pure ())
  where p = Proxy :: Proxy (Arity a)

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

