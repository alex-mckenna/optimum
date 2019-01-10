{-# LANGUAGE PatternSynonyms #-}

module Numeric.Optimization
    ( -- * Problems
      Problem
    , Direction(..)
    , maximize
    , minimize
    , suchThat
    , leq
    , geq
    , equ
    , pattern Maximize
    , pattern Minimize
    , pattern SuchThat
    , pattern (:<)
    , pattern (:>)
    , pattern (:=)
      -- * Solvers
    , Solver(..)
    , solveWith
    ) where

import Numeric.Optimization.Problem
import Numeric.Optimization.Solver

