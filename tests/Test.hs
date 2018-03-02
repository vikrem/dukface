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

import Data.Typeable

import qualified Data.Text as T

import Data.Proxy
import Lib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "All Tests"
  [
    testGroup "Object identity"
    [ testRefl $ Proxy @Int
    , testRefl $ Proxy @Double
    , testRefl $ Proxy @T.Text
    ]
  ]

instance Monad m => Serial m T.Text where
  series = T.pack <$> (series :: Series m String)

type CanGenRefl a = (Typeable a, Eq a, FromJSON a, Serial IO a, Show a)

testRefl :: forall a. CanGenRefl a => Proxy a -> TestTree
testRefl Proxy = testProperty (typeName ++ "s can pass between HS and JS") $ typeRefl $ Proxy @a
  where
    typeName = show $ typeRep $ Proxy @a

typeRefl :: forall a. CanGenRefl a => Proxy a -> Property IO
typeRefl _ = forAll $ \(x :: a) -> monadic $ do
    val <- runDuk $ dukLift $ execJS (T.pack . show $ x) :: IO a
    return $ val == x

monadLaw :: TestTree
monadLaw = testProperty "monad return law" $
  -- Eh, Int is good enough
  forAll $ \(x :: Int) -> monadic $ do
    v <- runDuk $ return x
    return $ v == x
