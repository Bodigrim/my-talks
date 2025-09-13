#!/usr/bin/env cabal
{- cabal:
build-depends: base, primitive, text
-}

module Lib where

import Control.Monad.ST
import Data.Primitive.ByteArray
import Data.Text.Internal
import Prelude hiding ((++))

data Buffer = Buffer ByteArray Int

(++) :: Buffer -> Text -> Buffer
Buffer arr used ++ Text srcArr srcOff srcLen = runST $ do
  let unused = sizeofByteArray arr - used
  if unused >= srcLen then do
    mutArr <- unsafeThawByteArray arr
    copyByteArray mutArr used srcArr srcOff srcLen
    arr' <- unsafeFreezeByteArray mutArr
    pure $ Buffer arr' (used + srcLen)
  else do
    mutArr <- newByteArray ((used + srcLen) * 2)
    copyByteArray mutArr 0 arr 0 used
    copyByteArray mutArr used srcArr srcOff srcLen
    arr' <- unsafeFreezeByteArray mutArr
    pure $ Buffer arr' (used + srcLen)

data MutableBuffer s = MutableBuffer (MutableByteArray s) Int

(+++) :: MutableBuffer s -> Text -> ST s (MutableBuffer s)
MutableBuffer mutArr used +++ Text srcArr srcOff srcLen = do
  size <- getSizeofMutableByteArray mutArr
  let unused = size - used
  if unused >= srcLen then do
    copyByteArray mutArr used srcArr srcOff srcLen
    pure $ MutableBuffer mutArr (used + srcLen)
  else do
    mutArr' <- resizeMutableByteArray mutArr ((used + srcLen) * 2)
    copyByteArray mutArr' used srcArr srcOff srcLen
    pure $ MutableBuffer mutArr' (used + srcLen)
