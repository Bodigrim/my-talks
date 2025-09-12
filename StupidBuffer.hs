#!/usr/bin/env cabal
{- cabal:
build-depends: base, primitive, text
-}

module Lib where

import Control.Monad.ST
import Data.Primitive.ByteArray
import Data.Text.Internal

data Builder = Builder ByteArray Int

append :: Builder -> Text -> Builder
append (Builder arr used) (Text srcArr srcOff srcLen) = runST $ do
  let unused = sizeofByteArray arr - used
  if unused >= srcLen then do
    mutArr <- unsafeThawByteArray arr
    arr' <- unsafeFreezeByteArray mutArr
    pure $ Builder arr' (used + srcLen)
  else do
    mutArr <- newByteArray ((used + srcLen) * 2)
    copyByteArray mutArr 0 arr 0 used
    copyByteArray mutArr used srcArr srcOff srcLen
    arr' <- unsafeFreezeByteArray mutArr
    pure $ Builder arr' (used + srcLen)
