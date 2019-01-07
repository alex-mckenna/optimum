{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Optimization.Trace where

import Debug.Trace

import Numeric.Optimization.Solver.Class


type IsTrace a =
    (Solver a, Show a, Show (Stop a), Show (Vars a))


newtype Trace a = Trace { getSolver :: a }
    deriving (Show)


instance (IsTrace a) => Solver (Trace a) where
    type CanSolve (Trace a) = CanSolve a
    type Stop     (Trace a) = Stop a
    type Vars     (Trace a) = Vars a

    isOptimal   =
        trace "isOptimal = " . traceShowId . isOptimal . getSolver

    toResult    =
        toResult . getSolver

    step        =
        trace "step = " . traceShowId . fmap Trace . step . getSolver


traceSolver :: (IsTrace a) => a -> Trace a
traceSolver = Trace

