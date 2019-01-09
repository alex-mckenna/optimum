{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Numeric.Optimization.Solver.Class where

import Control.Monad.Except     (ExceptT, liftEither)
import Data.Functor.Identity    (Identity)
import Data.Kind                (Constraint, Type)


class Solver method where
    type CanSolve method    :: Constraint
    type Stop method        :: Type
    type Vars method        :: Type

    isOptimal   :: method -> Bool
    toResult    :: method -> Vars method
    step        :: method -> Either (Stop method) method


class (Monad m) => MonadSolver m method | method -> m where
    type StopM m method :: Type
    type VarsM m method :: Type

    isOptimalM  :: method -> m Bool
    toResultM   :: method -> m (VarsM m method)
    stepM       :: method -> ExceptT (StopM m method) m method


instance (Solver method) => MonadSolver Identity method where
    type StopM Identity method = Stop method
    type VarsM Identity method = Vars method

    isOptimalM  = pure . isOptimal
    toResultM   = pure . toResult
    stepM       = liftEither . step

