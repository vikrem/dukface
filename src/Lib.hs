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
import qualified Data.Text.IO as T.IO
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
C.include "interrupt.h"

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
  ctx <- gets ctx
  excBox <- gets exc
  tasks <- gets tasks
  workers <- gets workers
  liftIO $ runReaderT dc $ DukCallEnv ctx excBox tasks workers

runDuk :: Duk a -> IO a
runDuk dk = do
  me <- myThreadId
  excBox <- newEmptyMVar
  taskQ <- newEmptyMVar
  numWorkers <- newMVar 0
  fatalHandler <- $(C.mkFunPtr [t|Ptr () -> CString -> IO ()|]) $ fatalErr me
  --async $ threadDelay 3000000 >> print "killing" >> killThread me
  (ret, newE) <- bracket
    (castPtr <$> [C.block| void* {
        install_handler();
        return duk_create_heap(NULL, NULL, NULL, 0, $(void (*fatalHandler)(void*, const char*)) );
      }|])
    (\ptr -> cleanup ptr >> freeHaskellFunPtr fatalHandler)
    (\ctx -> do
        retme <- runStateT dk $ DukEnv ctx me (pure ()) excBox taskQ numWorkers
        let loop = do
              works <- readMVar numWorkers
              case works of
                0 -> pure ()
                _ -> do
                  t <- takeMVar taskQ
                  runReaderT t $ DukCallEnv ctx excBox taskQ numWorkers
                  void $ liftIO $ modifyMVar_ numWorkers (\n -> return $! (n - 1))
                  loop
        loop
        return retme
    )
  gc newE
  tryReadMVar excBox >>= \case
    Just e -> throwM e >> return ret
    Nothing -> return ret
  where
    fatalErr :: ThreadId -> a -> CString -> IO ()
    --fatalErr victim _ msg = (peekCString msg >>= throwString) `catch` \(e :: SomeException) -> throwTo victim e
    --fatalErr victim _ msg = (peekCString msg >>= throwString) `catch` \(e :: SomeException) -> throwTo victim e
    -- TODO: get rethrow to work
    fatalErr _ _ msg = peekCString msg >>= \m -> T.IO.putStrLn $ "*** FATAL ERR IN DUKTAPE: " <> T.pack m

execJS :: FromJSON v => T.Text -> DukCall v
execJS code = do
  ctx' <- castPtr <$> asks dceCtx
  let bs = T.encodeUtf8 code
  errCode <- lift $ [C.block| int {
                        if(!interrupted())
                          return duk_peval_lstring($(void* ctx'), $bs-ptr:bs, $bs-len:bs);
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

injectFunc :: Dukkable f => f -> T.Text -> Duk ()
injectFunc fn name = do
  ctx <- gets ctx
  let ctx' = castPtr ctx
  excBox <- gets exc
  taskQ <- gets tasks
  numWorkers <- gets workers
  let name' = T.encodeUtf8 name
  let runFunc = runReaderT (entry fn) (DukCallEnv ctx excBox taskQ numWorkers) `catchAny` \(e :: SomeException) -> do
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


addEvent :: DukCall (DukCall ()) -> DukCall ()
addEvent dc = do
  env <- ask
  tasks <- asks dceTasks
  excBox <- asks dceExc
  numworkers <- asks dceWorkers
  void $ liftIO $ modifyMVar_ numworkers (\n -> do
                                          return $! n + 1)
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

