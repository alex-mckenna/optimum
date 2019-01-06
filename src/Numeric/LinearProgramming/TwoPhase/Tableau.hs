{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeOperators          #-}

module Numeric.LinearProgramming.TwoPhase.Tableau
    ( -- * Tableaus
      Tableau(..)
    , IsTableau
    , mkPhaseI
    , mkPhaseII
      -- * Operations on Tableaus
    , tableauStep
    , tableauDictionary
    , tableauOptimal
    ) where

import           Debug.Trace
import           GHC.TypeLits
import qualified Numeric.LinearAlgebra as LA
import           Numeric.LinearAlgebra.Static as LS

import           Numeric.LinearProgramming.Problem
import           Numeric.LinearProgramming.Solver.Types
import           Numeric.LinearProgramming.TwoPhase.Builder
import           Numeric.LinearProgramming.TwoPhase.Phase
import           Numeric.LinearProgramming.TwoPhase.Pivot
import           Numeric.LinearProgramming.TwoPhase.VarMap
import           Numeric.LinearProgramming.TwoPhase.VarName


type IsTableau p v s a =
    ( KnownNat v
    , KnownNat s
    , KnownNat a
    , KnownNat (s + a)
    , KnownNat (((2 + v) + (s + a)) + 2)
    , KnownNat (Rows p s a)
    , KnownNat (Cols p v s a)
    )


data Tableau :: Phase -> Nat -> Nat -> Nat -> * where
    Tableau :: (IsTableau p v s a)
        => VarMap p (Rows p s a) (Cols p v s a)
        -> L (Rows p s a) (Cols p v s a)
        -> Tableau p v s a


instance Show (Tableau p v s a) where
    show (Tableau _ table) = LA.dispf 1 $ LS.unwrap table


mkPhaseI
    :: forall v s a.
        (IsTableau 'PhaseI v s a)
    => Problem v s a
    -> Tableau 'PhaseI v s a
mkPhaseI problem =
    case toTableau $ mkBuilder problem of
        Just tableau    -> tableau
        Nothing         -> error "mkPhaseI: Failed to create tableau"
  where
    toTableau b = Tableau <$> toVarMap b <*> toMatrix b


mkPhaseII :: Tableau 'PhaseI v s a -> Tableau 'PhaseII v s a
mkPhaseII = undefined


tableauStep
    :: (VarName -> Bool)
    -> Tableau p v s a
    -> Either SolveError (Tableau p v s a)
tableauStep f (Tableau vs x) = do
    enter <- tryOr Optimal $ enteringFrom colChoices x
    leave <- tryOr Unbounded $ leavingFrom enter rowChoices x

    let cell = (leave, enter)
    trace "pivot = " <$> traceShowM cell

    pure $ Tableau (updateRow cell vs) (pivot cell x)
  where
    tryOr e     = maybe (Left e) Right

    colChoices  = findColumns f vs
    rowChoices  = allRows vs


tableauDictionary :: (IsTableau p v s a) => Tableau p v s a -> Dictionary
tableauDictionary (Tableau vs x) = undefined


-- A tableau is at it's optimal solution if all coefficients
-- in the objective row are non-negative.
--
tableauOptimal :: (IsTableau p v s a) => Tableau p v s a -> Bool
tableauOptimal (Tableau vs x) =
    allCells (>= 0) coeffCells x
  where
    coeffCells  = (0,) <$> findColumns isCoeff vs
    isCoeff n   = isDecision n || isSlack n

