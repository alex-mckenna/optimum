{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Optimization.Trace where

import Debug.Trace

import Numeric.Optimization.Solver  (Solver(..))


type IsTrace a =
    (Solver a, Show a, Show (Error a), Show (Result a))


-- The tracing solver wraps another solver, and inserts calls to trace when
-- the underlying solver checks for optimality or takes a step. This is for
-- people who want to
--
--   * see what happens when a problem is solved
--   * debug the implementation of a particular solver easily
--
newtype Trace a = Trace { getSolver :: a }

instance (Show a) => Show (Trace a) where
    show = show . getSolver


instance (IsTrace a) => Solver (Trace a) where
    type Error  (Trace a) = Error a
    type Result (Trace a) = Result a

    isOptimal   =
        trace "isOptimal = " . traceShowId . isOptimal . getSolver

    toResult    =
        toResult . getSolver

    step x      = case step $ getSolver x of
        Left err -> Left . trace "step = " $ traceShowId err
        Right st -> Right . Trace . trace "step = " $ traceShowId st


traceSolver :: (IsTrace a) => a -> Trace a
traceSolver = Trace

