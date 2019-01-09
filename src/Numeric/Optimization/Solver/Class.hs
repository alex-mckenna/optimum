{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeFamilies       #-}

module Numeric.Optimization.Solver.Class where

import Data.Kind    (Constraint, Type)


class Solver method where
    type CanSolve method    :: Constraint
    type Stop method        :: Type
    type Vars method        :: Type

    isOptimal   :: method -> Bool
    toResult    :: method -> Vars method
    step        :: method -> Either (Stop method) method

