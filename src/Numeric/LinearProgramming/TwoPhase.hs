{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.LinearProgramming.TwoPhase
    ( -- * Two-Phase Simplex
      TwoPhase
    , twoPhase
    ) where


import Numeric.LinearProgramming.Problem                (Problem)
import Numeric.LinearProgramming.Solver.Class
import Numeric.LinearProgramming.TwoPhase.Tableau
import Numeric.LinearProgramming.TwoPhase.Types


type IsTwoPhase v s a =
    (IsTableau 'PhaseI v s a, IsTableau 'PhaseII v s a)


data TwoPhase v s a
    = TableauI  (Tableau 'PhaseI v s a)
    | TableauII (Tableau 'PhaseII v s a)


instance (IsTwoPhase v s a) => Show (TwoPhase v s a) where
    show (TableauI  x) = show x
    show (TableauII x) = show x


instance (IsTwoPhase v s a) => Solver (TwoPhase v s a) where
    type Error  (TwoPhase v s a)  = TwoPhaseError
    type Result (TwoPhase v s a)  = TwoPhaseResult v

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
    => TwoPhase v s a -> TwoPhaseResult v
twoPhaseResult (TableauI  x)  = tableauResult x
twoPhaseResult (TableauII x)  = tableauResult x


twoPhaseStep :: (IsTwoPhase v s a)
    => TwoPhase v s a
    -> Either TwoPhaseError (TwoPhase v s a)
twoPhaseStep state = case state of
    TableauI  x
        | tableauOptimal x  -> fmap TableauII . stepII $ mkPhaseII x
        | otherwise         -> fmap TableauI  $ stepI x

    TableauII x             -> fmap TableauII $ stepII x
  where
    stepI   = tableauStep (\n -> not $ isSpecial n)
    stepII  = tableauStep (\n -> isDecision n || isSlack n)

