{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE TypeFamilies       #-}

module Numeric.Optimization.Solver
    ( -- * Solver Class
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
    => (Problem v s a -> method)
    -> Problem v s a
    -> Either (Stop method) (Vars method)
solveWith mkSolver problem =
    go $ mkSolver problem
  where
    go state
        | isOptimal state   = pure $ toResult state
        | otherwise         = either Left go $ step state

