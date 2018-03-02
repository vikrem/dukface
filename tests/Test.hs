{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import Test.SmallCheck.Series
import Data.Aeson.Types hiding (Series)
import Data.Aeson hiding (Series)

import Data.Typeable
import Data.Monoid

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Proxy
import Lib
import GHC.Generics
import Data.Scientific
import Data.Hashable
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V

import qualified Data.ByteString as BS
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
    ]
  ]

instance Monad m => Serial m T.Text where
  series = T.pack <$> (series :: Series m String)

instance (Eq k, Hashable k, Serial m k, Serial m v) => Serial m (HMS.HashMap k v) where
  series = do
    key <- series
    value <- series
    let elem = return (key,value)
    d <- getDepth
    ls <- listM d elem
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

type CanGenRefl a = (Typeable a, Eq a, FromJSON a, ToJSON a, Serial IO a, Show a)

testRefl :: forall a. CanGenRefl a => Proxy a -> TestTree
testRefl Proxy = testProperty (typeName ++ "s can pass between HS and JS") $ typeRefl $ Proxy @a
  where
    typeName = show $ typeRep $ Proxy @a

typeRefl :: forall a. CanGenRefl a => Proxy a -> Property IO
typeRefl _ = forAll $ \(x :: a) -> monadic $ do
    let jsSerial = (T.decodeUtf8 . BSL.toStrict . encode $ x)
    let evalStr = "(" <> jsSerial <> ")"
    val <- runDuk $ dukLift $ execJS evalStr :: IO a
    return $ val == x

monadLaw :: TestTree
monadLaw = testProperty "monad return law" $
  -- Eh, Int is good enough
  forAll $ \(x :: Int) -> monadic $ do
    v <- runDuk $ return x
    return $ v == x
