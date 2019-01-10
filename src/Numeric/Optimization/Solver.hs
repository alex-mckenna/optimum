{-# LANGUAGE TypeFamilies #-}

module Numeric.Optimization.Solver
    ( -- * Solver
      Solver(..)
    , solveWith
    ) where

import Numeric.Optimization.Problem (Problem)


-- For a type to be a solver, it needs only support three functions. One to
-- check if the current state is optimal, one to extract a result from a state
-- and one to try and step to the next state. While this may seem limited, it
-- should work for both native and foreign solvers.
--
-- Foreign solvers will likely define step as being a routine which performs
-- many steps in one go, returning a value containing the final state / error.
--
class Solver method where
    type Error  method
    type Result method

    isOptimal   :: method -> Bool
    toResult    :: method -> Result method
    step        :: method -> Either (Error method) method


-- Solve an optimization problem using the method provided. If the
-- optimization finds an optimal state, the result from that state is
-- returned. Otherwise, the error which occured while solving is returned.
--
solveWith
    :: (Solver method)
    => (Problem d v s a c -> method)
    -> Problem d v s a c
    -> Either (Error method) (Result method)
solveWith mkSolver problem =
    go $ mkSolver problem
  where
    go state
        | isOptimal state   = pure $ toResult state
        | otherwise         = either Left go $ step state

