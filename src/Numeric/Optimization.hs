{-# LANGUAGE PatternSynonyms #-}

module Numeric.Optimization
    ( -- * Problems
      Problem
    , maximize
    , suchThat
    , leq
    , geq
    , equ
    , pattern Maximize
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

