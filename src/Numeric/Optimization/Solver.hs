{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE TypeFamilies       #-}

module Numeric.Optimization.Solver
    ( -- * Solver
      Solver(..)
    , IsSolver
    , solveWith
    ) where

import Numeric.Optimization.Problem         (Problem)
import Numeric.Optimization.Solver.Class


type IsSolver a =
    (Solver a, CanSolve a)


solveWith
    :: (IsSolver method)
    => (Problem d v s a c -> method)
    -> Problem d v s a c
    -> Either (Stop method) (Vars method)
solveWith mkSolver problem =
    go $ mkSolver problem
  where
    go state
        | isOptimal state   = pure $ toResult state
        | otherwise         = either Left go $ step state

