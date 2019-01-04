{-# LANGUAGE DataKinds  #-}

module Lib where

import           Prelude hiding                         (Ordering(..))

import qualified Data.Vector.Storable.Sized as SVec

import           Numeric.LinearProgramming.Problem


example :: Problem 3 3 1
example = Maximise (SVec.fromTuple (5, -1, -1))
    `SuchThat` LEQ (SVec.fromTuple (3, -1, -1)) (-1)
    `SuchThat` LEQ (SVec.fromTuple (1,  2, -1)) (-2)
    `SuchThat` LEQ (SVec.fromTuple (2,  1,  0))   2
    `SuchThat` EQU (SVec.fromTuple (1,  1,  0))   1

