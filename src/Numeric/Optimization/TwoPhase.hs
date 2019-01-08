{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Optimization.TwoPhase
    ( -- * Two-Phase Simplex
      TwoPhase
    , TwoPhaseStop
    , TwoPhaseVars
    , twoPhase
    ) where

import Data.Bifunctor                           (bimap)

import Numeric.Optimization.Problem             (Problem)
import Numeric.Optimization.Solver.Class        (Solver(..))
import Numeric.Optimization.TwoPhase.Tableau
import Numeric.Optimization.TwoPhase.Types


type IsTwoPhase v s a c =
    (IsTableau 'PhaseI v s a c, IsTableau 'PhaseII v s a c)


data TwoPhase v s a c
    = TableauI  (Tableau 'PhaseI v s a c)
    | TableauII (Tableau 'PhaseII v s a c)


instance (IsTwoPhase v s a c) => Show (TwoPhase v s a c) where
    show (TableauI  x) = show x
    show (TableauII x) = show x


instance (IsTwoPhase v s a c) => Solver (TwoPhase v s a c) where
    type CanSolve (TwoPhase v s a c) = IsTwoPhase v s a c
    type Stop     (TwoPhase v s a c) = TwoPhaseStop
    type Vars     (TwoPhase v s a c) = TwoPhaseVars v

    isOptimal   = twoPhaseOptimal
    toResult    = twoPhaseResult
    step        = twoPhaseStep


-- Construct a solver for the given problem that uses the Two-Phase Simplex
-- method. This method first solves an artificial problem to find an initial
-- basic feasible solution (or identify infeasibility), then solves the initial
-- problem using the obtained solution.
--
twoPhase :: (IsTwoPhase v s a c)
    => Problem v s a c -> TwoPhase v s a c
twoPhase = TableauI . mkPhaseI


-- When checking for optimality in the solver, the Phase I tableau must never
-- be reported as optimal. This is because it is not guaranteed to find an
-- optimal solution to the problem being optimized, only an initial BFS.
--
twoPhaseOptimal :: (IsTwoPhase v s a c)
    => TwoPhase v s a c -> Bool
twoPhaseOptimal (TableauI  _) = False
twoPhaseOptimal (TableauII x) = tableauOptimal x


twoPhaseResult :: (IsTwoPhase v s a c)
    => TwoPhase v s a c -> TwoPhaseVars v
twoPhaseResult (TableauI  x)  = tableauVars x
twoPhaseResult (TableauII x)  = tableauVars x


twoPhaseStep :: (IsTwoPhase v s a c)
    => TwoPhase v s a c
    -> Either TwoPhaseStop (TwoPhase v s a c)
twoPhaseStep state = case state of
    TableauI  x
        | tableauOptimal x  -> fmap TableauII . stepII $ mkPhaseII x
        | otherwise         -> bimap (const Infeasible) TableauI  $ stepI x

    TableauII x             -> fmap TableauII $ stepII x
  where
    stepI   = tableauStep (\n -> isDecision n || isSlack n)
    stepII  = tableauStep (not . isSpecial)

