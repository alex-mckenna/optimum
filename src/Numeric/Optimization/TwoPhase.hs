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
    , twoPhase
    ) where


import Numeric.Optimization.Problem            (Problem)
import Numeric.Optimization.Solver.Class       (Solver(..))
import Numeric.Optimization.TwoPhase.Tableau
import Numeric.Optimization.TwoPhase.Types


type IsTwoPhase v s a =
    (IsTableau 'PhaseI v s a, IsTableau 'PhaseII v s a)


data TwoPhase v s a
    = TableauI  (Tableau 'PhaseI v s a)
    | TableauII (Tableau 'PhaseII v s a)


instance (IsTwoPhase v s a) => Show (TwoPhase v s a) where
    show (TableauI  x) = show x
    show (TableauII x) = show x


instance (IsTwoPhase v s a) => Solver (TwoPhase v s a) where
    type Stop (TwoPhase v s a) = TwoPhaseStop
    type Vars (TwoPhase v s a) = TwoPhaseVars v

    isOptimal   = twoPhaseOptimal
    toResult    = twoPhaseResult
    step        = twoPhaseStep


-- Construct a solver for the given problem that uses the Two-Phase Simplex
-- method. This method first solves an artificial problem to find an initial
-- basic feasible solution (or identify infeasibility), then solves the initial
-- problem using the obtained solution.
--
twoPhase :: (IsTwoPhase v s a)
    => Problem v s a -> TwoPhase v s a
twoPhase = TableauI . mkPhaseI


-- When checking for optimality in the solver, the Phase I tableau must never
-- be reported as optimal. This is because it is not guaranteed to find an
-- optimal solution to the problem being optimized, only an initial BFS.
--
twoPhaseOptimal :: (IsTwoPhase v s a)
    => TwoPhase v s a -> Bool
twoPhaseOptimal (TableauI  _) = False
twoPhaseOptimal (TableauII x) = tableauOptimal x


twoPhaseResult :: (IsTwoPhase v s a)
    => TwoPhase v s a -> TwoPhaseVars v
twoPhaseResult (TableauI  x)  = tableauVars x
twoPhaseResult (TableauII x)  = tableauVars x


twoPhaseStep :: (IsTwoPhase v s a)
    => TwoPhase v s a
    -> Either TwoPhaseStop (TwoPhase v s a)
twoPhaseStep state = case state of
    TableauI  x
        | tableauOptimal x  -> fmap TableauII . stepII $ mkPhaseII x
        | otherwise         -> fmap TableauI  $ stepI x

    TableauII x             -> fmap TableauII $ stepII x
  where
    stepI   = tableauStep (\n -> not $ isSpecial n)
    stepII  = tableauStep (\n -> isDecision n || isSlack n)

