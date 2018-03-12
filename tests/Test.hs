{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}

import Protolude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import Test.SmallCheck.Series
import Data.Aeson.Types hiding (Series)
import Data.Aeson hiding (Series)

import Control.Exception.Safe
import Control.Concurrent.Async

import Data.String
import Data.IORef

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Dukface
import Data.Scientific
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V

import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "All Tests"
  [
      testGroup "Monad laws"
    [
      monadLaw
    ]
    , testGroup "Object identity"
    [ testRefl $ Proxy @Int
    , testRefl $ Proxy @Double
    , testRefl $ Proxy @Value
    , testRefl $ Proxy @T.Text
    , testRefl $ Proxy @String
    ]
    , testGroup "Callbacks"
    [ testCase "Single-arg callback" singleCallback
    , testCase "Triple-arg callback" tripleCallback
    , testCase "Namespaced callback" namedCallback
    , testCase "Namespaced callback with missing namespace throws" namedCallbackExc
    , testCase "Callback to Haskell from Haskell via JS" hsCallback
    , testCase "Callback to JS from Haskell worker" asyncCallback
    , testCase "Nested JS calls to Haskell worker" asyncNestedCallback
    , testCase "Multiple concurrent Haskell workers" multipleWorkers
    ]
    , testGroup "Exception handling and termination"
    [ testCase "Can kill a long-running JS process" canKill
    , testCase "Can kill multiple concurrent long-running JS processes" canKillMany
    , testCase "Can capture the exception of a Haskell callback" canCatchCallbackExc
    , testCase "Can capture the exceptions of concurrent Haskell callbacks" canCatchManyCallbackExc
    , testCase "Can capture the exceptions of Haskell workers" canCatchWorkerExc
    , testCase "Can capture the exceptions of concurrent Haskell workers" canCatchManyWorkerExc
    , testCase "Can catch unrepresentable data exceptions" canCatchBadcoercion
    , testCase "Can catch errors from JS" canCatchSourceErr
    ]
    , testGroup "Haskell workers"
    [ testCase "Can use Haskell worker for setTimeout" setTimeoutTest
    ]
  ]

instance Monad m => Serial m T.Text where
  series = T.pack <$> (series :: Series m String)

instance (Eq k, Hashable k, Serial m k, Serial m v) => Serial m (HMS.HashMap k v) where
  series = do
    key <- series
    value <- series
    let element = return (key,value)
    d <- getDepth
    ls <- listM d element
    return $ HMS.fromList ls

instance Serial m a => Serial m (V.Vector a) where
  series = do
    value <- series
    d <- getDepth
    let value' = return value
    ls <- listM d value'
    return $ V.fromList ls

instance Monad m => Serial m Scientific where
  series = cons2 scientific

instance Monad m => Serial m Value

type CanGenRefl a = (KnownNat (Arity a), Typeable a, Eq a, FromJSON a, ToJSON a, Serial IO a, Show a)

testRefl :: forall a. CanGenRefl a => Proxy a -> TestTree
testRefl Proxy = testProperty (typeName ++ "s can pass between HS and JS") $ typeRefl $ Proxy @a
  where
    typeName = show $ typeRep $ Proxy @a

typeRefl :: forall a. CanGenRefl a => Proxy a -> Property IO
typeRefl _ = forAll $ \(x :: a) -> monadic $ do
    let jsSerial = (T.decodeUtf8 . BSL.toStrict . encode $ x)
    let evalStr = "(" <> jsSerial <> ")"
    let hsFunc = return x :: DukCall a
    let callStr = "(f())"
    direct_ref <- runDuk $ dukLift $ execJS evalStr :: IO a
    direct_call <- runDuk $ do { injectFunc hsFunc [] "f"; dukLift $ execJS callStr} :: IO a
    return $ and $ fmap (== x) [direct_ref, direct_call]

monadLaw :: TestTree
monadLaw = testProperty "monad return law" $
  -- Eh, Int is good enough
  forAll $ \(x :: Int) -> monadic $ do
    v <- runDuk $ return x
    return $ v == x

singleCallback :: Assertion
singleCallback = do
  ref <- newIORef 0
  let js = "function getVal(){ return 5; } f(getVal);"
  _ <- runDuk $ do { injectFunc (hsFunc ref) [] "f"; dukLift $ execJS js } :: IO Int
  v <- readIORef ref
  v @?= 5
  where
    hsFunc :: IORef Int -> JSCallback Int -> DukCall Int
    hsFunc ref cb = evalCallback cb >>= \v -> liftIO (writeIORef ref v) >> return v

tripleCallback :: Assertion
tripleCallback = do
  ref <- newIORef 0
  let js = "function getVal(a, b, c){ return a * b * c; } f(getVal);"
  _ <- runDuk $ do { injectFunc (hsFunc ref) [] "f"; dukLift $ execJS js } :: IO Int
  v <- readIORef ref
  v @?= 27
  where
    hsFunc :: IORef Int -> JSCallback (Int -> Int -> Int -> Int) -> DukCall Int
    hsFunc ref cb = evalCallback cb 3 3 3 >>= \v -> liftIO (writeIORef ref v) >> return v

namedCallback :: Assertion
namedCallback = do
  ref <- newIORef 0
  let js = "function getVal(a, b, c){ return a * b * c; } foo.bar.baz.f(getVal);"
  let setup = "foo = {}; foo.bar = {}; foo.bar.baz = {};"
  _ <- runDuk $ do {
    _ <- dukLift $ execJS setup :: Duk Value;
    injectFunc (hsFunc ref) ["foo", "bar", "baz"] "f";
    dukLift $ execJS js } :: IO Int
  v <- readIORef ref
  v @?= 27
  where
    hsFunc :: IORef Int -> JSCallback (Int -> Int -> Int -> Int) -> DukCall Int
    hsFunc ref cb = evalCallback cb 3 3 3 >>= \v -> liftIO (writeIORef ref v) >> return v

namedCallbackExc :: Assertion
namedCallbackExc =
  runDuk (injectFunc hsFunc ["foo", "bar", "baz"] "f") `catchDeep` \(e :: SomeException) -> do
    let err = "Not the exception we were looking for!"
    "foo" `T.isInfixOf` T.pack (displayException e) @? err
  where
    hsFunc :: DukCall Int
    hsFunc = return 5

hsCallback :: Assertion
hsCallback = do
  ref <- newIORef 0
  let js = "f(g);"
  _ <- runDuk $ do { injectFunc (f ref) [] "f"; injectFunc g [] "g"; dukLift $ execJS js } :: IO Int
  v <- readIORef ref
  v @?= 10
  where
    f :: IORef Int -> JSCallback (Int -> Int) -> DukCall Int
    f ref cb = evalCallback cb 5 >>= \v -> liftIO (writeIORef ref v) >> return v
    g :: Int -> DukCall Int
    g = return . (*) 2

asyncCallback :: Assertion
asyncCallback = do
  ref <- newIORef 0
  let js = "function getVal(){ return 11; } f(getVal);"
  _ <- runDuk $ do { injectFunc (hsFunc ref) [] "f"; dukLift $ execJS js } :: IO ()
  readIORef ref >>= (@=?) 11
  _ <- runDuk $ do { injectFunc (hsFunc' ref) [] "f"; dukLift $ execJS js } :: IO ()
  readIORef ref >>= (@=?) 11
  where
    hsFunc :: IORef Int -> JSCallback Int -> DukCall ()
    -- run the callback then return a value later
    hsFunc ref cb = evalCallback cb >>= \v -> addEvent $ return $ void $ liftIO (writeIORef ref v)
    -- return a value later that runs the callback and returns a value
    hsFunc' :: IORef Int -> JSCallback Int -> DukCall ()
    hsFunc' ref cb = addEvent $ return $ do
      v <- evalCallback cb
      liftIO $ writeIORef ref v
      return ()

asyncNestedCallback :: Assertion
asyncNestedCallback = do
  ref <- newIORef 0
  let letters = T.singleton <$> ['a'..'z']
  let injections = sequence $ injectFunc (hsFunc ref) [] <$> letters
  let js = nest letters
  _ <- runDuk $ do { void injections; dukLift $ execJS js } :: IO ()
  readIORef ref >>= (@=?) 26
  where
    hsFunc :: IORef Int -> DukCall ()
    -- run the callback then return a value later
    hsFunc ref = addEvent $ return $ void $ liftIO (modifyIORef' ref (1 + ))
    nest [] = ""
    nest (x:xs) = x <> "(" <> nest xs <> ")"

multipleWorkers :: Assertion
multipleWorkers = do
  refs <- replicateM 26 (newIORef 0)
  let letters = T.singleton <$> ['a'..'z']
  let injections = zipWithM (\c ref -> injectFunc (hsFunc ref) [] c) letters refs
  let js = mconcat $ (<> "();") <$> letters
  _ <- runDuk $ do { void injections; dukLift $ execJS js } :: IO ()
  sequence_ $ (readIORef >=> (@=?) 1) <$> refs
  where
    hsFunc :: IORef Int -> DukCall ()
    hsFunc ref = addEvent $ return $ void $ liftIO (modifyIORef' ref (1 + ))

canKill :: Assertion
canKill = do
  let js = "for(;;){}"
  (t :: Async ()) <- async $ runDuk $ dukLift $ execJS js
  kill <- async $ threadDelay 1000000 >> cancel t
  wait kill

canKillMany :: Assertion
canKillMany = do
  let js = "for(;;){}"
  (thds :: [Async ()]) <- replicateM 30 $ async $ runDuk $ dukLift $ execJS js
  kill <- async $ threadDelay 1000000 >> sequence_ (cancel <$> thds)
  wait kill

canCatchCallbackExc :: Assertion
canCatchCallbackExc = do
  death <- newIORef False
  _ :: () <- (runDuk $ do { injectFunc hsFunc [] "f"; dukLift $ execJS "f();" }) `catchDeep` \(e :: SomeException) -> do
    let err = "Not the exception we were looking for!"
    "wrry" `T.isInfixOf` T.pack (displayException e) @? err
    "asplode" `T.isInfixOf` T.pack (displayException e) @? err
    writeIORef death True
  readIORef death >>= (@=?) True
  where
    hsFunc :: DukCall ()
    hsFunc = throwString "wrry does this asplode" >> pure ()

canCatchManyCallbackExc :: Assertion
canCatchManyCallbackExc = replicateConcurrently_ 100 canCatchCallbackExc

canCatchWorkerExc :: Assertion
canCatchWorkerExc = do
  death <- newIORef False
  _ :: () <- (runDuk $ do { injectFunc hsFunc [] "f"; dukLift $ execJS "function g(){}; f();" }) `catchDeep` \(e :: SomeException) -> do
    let err = "Not the exception we were looking for!"
    "wrry" `T.isInfixOf` T.pack (displayException e) @? err
    "asplode" `T.isInfixOf` T.pack (displayException e) @? err
    writeIORef death True
  readIORef death >>= (@=?) True
  where
    hsFunc :: DukCall ()
    hsFunc = addEvent $ return $ throwString "wrry does this asplode"

canCatchManyWorkerExc :: Assertion
canCatchManyWorkerExc = replicateConcurrently_ 100 canCatchWorkerExc

setTimeoutTest :: Assertion
setTimeoutTest = do
  act <- newIORef False
  _ :: () <- (runDuk $ do { injectFunc delay [] "f"; injectFunc (final act) [] "g"; dukLift $ execJS "f(g);" })
  readIORef act >>= (@=?) True
  where
    delay :: JSCallback Int -> DukCall ()
    delay cb = addEvent ( do { liftIO (threadDelay 1000000); return $ void $ evalCallback cb })
    final :: IORef Bool -> DukCall Int
    final ref = void (liftIO $ writeIORef ref True) >> pure 0

canCatchBadcoercion :: Assertion
canCatchBadcoercion = do
  let js = "undefined"
  (runDuk $ dukLift $ execJS js) `catchDeep` \(e :: SomeException) -> do
    let err = "Not the exception we were looking for!"
    "coercible" `T.isInfixOf` T.pack (displayException e) @? (err <> " : " <> displayException e)

canCatchSourceErr :: Assertion
canCatchSourceErr = do
  death <- newIORef False
  let js = "y this explod tho"
  runDuk $ dukLift $ execJS js `catchDeep` \(_ :: SomeException) -> liftIO $ writeIORef death True
  readIORef death >>= (@=?) True
