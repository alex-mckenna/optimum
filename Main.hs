{-# LANGUAGE DataKinds          #-}
-- {-# LANGUAGE FlexibleContexts   #-}

module Main where

import Numeric.Optimization.Problem
import Numeric.Optimization.Solver
import Numeric.Optimization.Trace
import Numeric.Optimization.TwoPhase


example1a :: Problem 'Max 3 3 3 4
example1a = maximize ( 5, -1, -1)
    `suchThat`   geq (-3,  1,  1) 1
    `suchThat`   geq (-1, -2,  1) 2
    `suchThat`   leq ( 2,  1,  0) 2
    `suchThat`   equ ( 1,  1,  0) 1


example1b :: Problem 'Min 3 3 3 4
example1b = minimize (-5,  1,  1)
    `suchThat`   geq (-3,  1,  1) 1
    `suchThat`   geq (-1, -2,  1) 2
    `suchThat`   leq ( 2,  1,  0) 2
    `suchThat`   equ ( 1,  1,  0) 1


example2 :: Problem 'Max 3 0 2 2
example2 = maximize (2, 3, 4)
    `suchThat`  equ (3, 2, 1) 10
    `suchThat`  equ (2, 5, 3) 15


main :: IO ()
main =
    print $ solveWith (traceSolver . twoPhase) example1b

