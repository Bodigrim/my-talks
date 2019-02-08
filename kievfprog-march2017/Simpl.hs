{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Simpl where

import GHC.TypeLits
import Data.String

newtype Counter (range :: Nat) = Counter String
  deriving (IsString)

data Reference (arity :: [Nat]) where
  V :: String -> Reference xs
  (:!) :: Reference (x : xs) -> Counter x -> Reference xs

(!) :: Reference (x : xs) -> Counter x -> Reference xs
(!) = (:!)

instance IsString (Reference xs) where
  fromString = V

data Expr a where
  Ref :: Reference '[] -> Expr Double
  Num :: Double -> Expr Double
  (:+) :: Expr Double -> Expr Double -> Expr Double
  (:*) :: Expr Double -> Expr Double -> Expr Double
  (:^) :: Expr Double -> Expr Double -> Expr Double
  (:<) :: Expr Double -> Expr Double -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

infixl 6 :+
infixl 7 :*
infixl 8 :^
infix  4 :<

instance Num (Expr Double) where
  (+) = (:+)
  (*) = (:*)
  negate x = x :* (-1)
  abs x = If (x :< 0) (negate x) x
  signum x = If (x :< 0) (-1) (If (0 :< x) 1 0)
  fromInteger = Num . fromInteger

(&&) :: Expr Bool -> Expr Bool -> Expr Bool
a && b = If a b a
infixr 3 &&

(||) :: Expr Bool -> Expr Bool -> Expr Bool
a || b = If a a b
infixr 2 ||

data Stmt where
  (:=) :: Reference '[] -> Expr Double -> Stmt
  For :: Counter x -> [Stmt] -> Stmt
infix 0 :=

sumOfIncreasingSquares :: [Stmt]
sumOfIncreasingSquares =
  [ For i
    [ a!i := Ref c
    , c := Ref c + 1
    ]
  , For i
    [ b!i := Ref (a!i) :^ 2
    ]
  , ret     := 0
  , For i
    [ ret   := Ref ret + Ref (b!i)
    ]
  ]
  where
    a = "a"
    b = "b"
    c = "c"
    ret = "ret"
    i = "i"

-- -- partial compilation: supply some data, optimize, then either generate code, or supply some more data
-- -- Muchnik algorithm

-- eval :: Map Identifier Value -> Program -> Double
-- eval = undefined
