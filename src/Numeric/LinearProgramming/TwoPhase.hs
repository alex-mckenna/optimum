{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.LinearProgramming.TwoPhase
    ( -- * Two-Phase Simplex
      TwoPhase
    , twoPhase
    ) where


import Numeric.LinearProgramming.Problem                (Problem)
import Numeric.LinearProgramming.Solver.Class
import Numeric.LinearProgramming.Solver.Types
import Numeric.LinearProgramming.TwoPhase.Tableau
import Numeric.LinearProgramming.TwoPhase.Phase
import Numeric.LinearProgramming.TwoPhase.VarName


type IsTwoPhase v s a =
    (IsTableau 'PhaseI v s a, IsTableau 'PhaseII v s a)


data TwoPhase v s a
    = TableauI  (Tableau 'PhaseI  v s a)
    | TableauII (Tableau 'PhaseII v s a)
    deriving (Show)


instance (IsTwoPhase v s a) => CanSolve (TwoPhase v s a) where
    step        = twoPhaseStep
    dictionary  = twoPhaseDictionary
    isOptimal   = twoPhaseOptimal


-- Construct a solver for the given problem that uses the Two-Phase Simplex
-- method. This method first solves an artificial problem to find an initial
-- basic feasible solution (or identify infeasibility), then solves the initial
-- problem using the obtained solution.
--
twoPhase :: (IsTwoPhase v s a) => Problem v s a -> TwoPhase v s a
twoPhase = TableauI . mkPhaseI


-- When checking for optimality in the solver, the Phase I tableau must never
-- be reported as optimal. This is because it is not guaranteed to find an
-- optimal solution to the problem being optimized, only an initial BFS.
--
twoPhaseOptimal :: (IsTwoPhase v s a)
    => TwoPhase v s a -> Bool
twoPhaseOptimal (TableauI  _) = False
twoPhaseOptimal (TableauII x) = tableauOptimal x


twoPhaseDictionary :: (IsTwoPhase v s a)
    => TwoPhase v s a -> Dictionary
twoPhaseDictionary (TableauI  x)  = tableauDictionary x
twoPhaseDictionary (TableauII x)  = tableauDictionary x


twoPhaseStep :: (IsTwoPhase v s a)
    => TwoPhase v s a -> Either SolveError (TwoPhase v s a)
twoPhaseStep state = case state of
    TableauI  x
        | tableauOptimal x  -> fmap TableauII . stepII $ mkPhaseII x
        | otherwise         -> fmap TableauI  $ stepI x

    TableauII x             -> fmap TableauII $ stepII x
  where
    stepI   = tableauStep (\n -> not $ isSpecial n)
    stepII  = tableauStep (\n -> isDecision n || isSlack n)

