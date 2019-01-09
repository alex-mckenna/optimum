{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}

module Main where

import qualified Data.Vector.Storable.Sized as SVec

import           Numeric.Optimization.Problem
import           Numeric.Optimization.Solver
import           Numeric.Optimization.Trace
import           Numeric.Optimization.TwoPhase


example1 :: Problem 3 3 3 4
example1 = maximize (SVec.fromTuple ( 5, -1, -1))
    `suchThat` geq (SVec.fromTuple (-3,  1,  1)) 1
    `suchThat` geq (SVec.fromTuple (-1, -2,  1)) 2
    `suchThat` leq (SVec.fromTuple ( 2,  1,  0)) 2
    `suchThat` equ (SVec.fromTuple ( 1,  1,  0)) 1


example2 :: Problem 3 0 2 2
example2 = minimize (SVec.fromTuple (-2, -3, -4))
    `suchThat` equ  (SVec.fromTuple ( 3,  2,  1)) 10
    `suchThat` equ  (SVec.fromTuple ( 2,  5,  3)) 15


main :: IO ()
main =
    print $ solveWith twoPhase example1

