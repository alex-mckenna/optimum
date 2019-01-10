{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Optimization.TwoPhase
    ( -- * Two-Phase Simplex
      TwoPhase
    , TwoPhaseError
    , TwoPhaseResult
    , twoPhase
    ) where

import Data.Finite                              (Finite)

import Numeric.Optimization.Problem             (Problem, Direction(..))
import Numeric.Optimization.Solver              (Solver(..))
import Numeric.Optimization.TwoPhase.Tableau
import Numeric.Optimization.TwoPhase.Types


type IsTwoPhase v s a c =
    (IsTableau 'PhaseI v s a c, IsTableau 'PhaseII v s a c)


data TwoPhase d v s a c
    = TableauI  (Tableau 'PhaseI d v s a c)
    | TableauII (Tableau 'PhaseII d v s a c)


instance (IsTwoPhase v s a c) => Show (TwoPhase d v s a c) where
    show (TableauI  x) = show x
    show (TableauII x) = show x


instance (IsTwoPhase v s a c) => Solver (TwoPhase 'Max v s a c) where
    type Error  (TwoPhase 'Max v s a c) = TwoPhaseError
    type Result (TwoPhase 'Max v s a c) = TwoPhaseResult 'Max v

    isOptimal   = twoPhaseOptimal maximizeOptimal
    step        = twoPhaseStep maximizeEntering
    toResult    = twoPhaseResult


instance (IsTwoPhase v s a c) => Solver (TwoPhase 'Min v s a c) where
    type Error  (TwoPhase 'Min v s a c) = TwoPhaseError
    type Result (TwoPhase 'Min v s a c) = TwoPhaseResult 'Min v

    isOptimal   = twoPhaseOptimal minimizeOptimal
    step        = twoPhaseStep minimizeEntering
    toResult    = twoPhaseResult


-- Construct a solver for the given problem that uses the Two-Phase Simplex
-- method. This method first solves an artificial problem to find an initial
-- basic feasible solution (or identify infeasibility), then solves the initial
-- problem using the obtained solution.
--
twoPhase :: (IsTwoPhase v s a c)
    => Problem d v s a c -> TwoPhase d v s a c
twoPhase = TableauI . mkPhaseI


-- When checking for optimality in the solver, the Phase I tableau must never
-- be reported as optimal. This is because it is not guaranteed to find an
-- optimal solution to the problem being optimized, only an initial BFS.
--
twoPhaseOptimal
    :: (IsTwoPhase v s a c)
    => (Tableau 'PhaseII d v s a c -> Bool)
    -> TwoPhase d v s a c
    -> Bool
twoPhaseOptimal _ (TableauI  _) = False
twoPhaseOptimal f (TableauII x) = f x


twoPhaseResult
    :: (IsTwoPhase v s a c)
    => TwoPhase d v s a c
    -> TwoPhaseResult d v
twoPhaseResult (TableauI  x)  = tableauResult x
twoPhaseResult (TableauII x)  = tableauResult x


-- When stepping the solver, it is important to convert between phases when
-- appropriate. This means that if the tableau is in phase I, it must check
-- whether the current state is infeasible (in which case optimization stops)
-- or optimal (in which case the optimization changes to phase II).
--
twoPhaseStep
    :: (IsTwoPhase v s a c)
    => (Tableau 'PhaseII d v s a c
        -> Either TwoPhaseError (Finite (Cols 'PhaseII v s a)))
    -> TwoPhase d v s a c
    -> Either TwoPhaseError (TwoPhase d v s a c)
twoPhaseStep f state = case state of
    TableauI  x
        | tableauInfeasible x   -> Left Infeasible
        | phaseIOptimal x       -> Right . TableauII $ mkPhaseII x
        | otherwise             -> TableauI  <$> stepI x

    TableauII x                 -> TableauII <$> stepII x
  where
    stepI   = tableauStep phaseIEntering
    stepII  = tableauStep f

