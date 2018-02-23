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

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx)
C.include "duktape.h"

data DuktapeHeap
type DuktapeCtx = Ptr DuktapeHeap

data Duk a = MkDuk { unDuk :: DuktapeCtx -> IO a } deriving (Functor)

instance Applicative Duk where
  pure a = MkDuk $ \_ -> return a
  (MkDuk a) <*> (MkDuk b) = MkDuk $ \ctx -> do { f <- a ctx; f <$> b ctx}

instance Monad Duk where
  return = pure
  (MkDuk v) >>= f = MkDuk $ \ctx -> do { a <- v ctx; unDuk (f a) ctx }

cleanup :: Ptr DuktapeHeap -> IO ()
cleanup ctx = [C.exp| void { duk_destroy_heap($(void* ctx')) }|]
  where
   ctx' = castPtr ctx

runDuk :: Duk a -> IO a
runDuk dk = do
  (ctx :: DuktapeCtx) <- castPtr <$> [C.exp| void* { duk_create_heap_default() } |]
  ret <- unDuk dk ctx
  cleanup ctx
  return ret

execJS :: T.Text -> Duk String
execJS code = MkDuk $ \ctx -> do
  let ctx' = castPtr ctx
  let bs = T.encodeUtf8 code
  pArr <- [C.block| const char* {
    duk_peval_lstring($(void* ctx'), $bs-ptr:bs, $bs-len:bs);
    return duk_to_string($(void* ctx'), -1);
  }|]
  peekCString pArr
