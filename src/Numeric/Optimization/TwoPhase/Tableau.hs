{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Optimization.TwoPhase.Tableau
    ( -- * Tableaus
      Tableau(..)
    , IsTableau
    , mkPhaseI
    , mkPhaseII
      -- * Operations on Tableaus
    , tableauOptimal
    , tableauVars
    , tableauStep
    ) where

import           Data.Finite                            (Finite)
import qualified Data.List as List
import           Data.Maybe
import           Data.Ord                               (comparing)
import qualified Data.Vector.Sized as Vec
import           GHC.TypeLits
import qualified Numeric.LinearAlgebra as LA
import           Numeric.LinearAlgebra.Static           (L)
import qualified Numeric.LinearAlgebra.Static as LS

import           Numeric.Optimization.Problem
import           Numeric.Optimization.TwoPhase.Build
import           Numeric.Optimization.TwoPhase.Pivot
import           Numeric.Optimization.TwoPhase.Types
import           Numeric.Optimization.TwoPhase.VarMap


type IsTableau p v s a c =
    ( KnownNat v
    , KnownNat s
    , KnownNat a
    , KnownNat c
    , KnownNat (v + 1)
    , KnownNat (Rows p c)
    , KnownNat (Cols p v s a)
    , IsBuilder v s a c
    )


data Tableau (p :: Phase) (v :: Nat) (s :: Nat) (a :: Nat) (c :: Nat)
    = Tableau
        { varMap :: VarMap p (Rows p c) (Cols p v s a)
        , table  :: L (Rows p c) (Cols p v s a)
        }

instance (IsTableau p v s a c) => Show (Tableau p v s a c) where
    show = LA.dispf 1 . LS.unwrap . table


-- Constructing Tableaus

mkPhaseI
    :: (IsTableau 'PhaseI v s a c)
    => Problem v s a c
    -> Tableau 'PhaseI v s a c
mkPhaseI problem =
    Tableau (mkVarMap problem) (build problem)


mkPhaseII :: Tableau 'PhaseI v s a c -> Tableau 'PhaseII v s a c
mkPhaseII = undefined


-- Checking Optimality

-- A tableau is at it's optimal solution if all coefficients in the objective
-- row are non-negative. This includes slack and artificial columns - but
-- not special columns (scale, RHS).
--
tableauOptimal
    :: (IsTableau p v s a c)
    => Tableau p v s a c
    -> Bool
tableauOptimal (Tableau vs x) =
    allCells (>= 0) coeffCells x
  where
    coeffCells  = [(0, j) | j <- findIndicesColumns isCoeff vs]
    isCoeff     = not . isSpecial


-- Extracting Results

readValue
    :: (IsTableau p v s a c)
    => VarName
    -> Tableau p v s a c
    -> Double
readValue n (Tableau vs x)
    | Vec.elem n (columnVars vs) =
        let mIx = (,) <$> elemIndexRows n vs <*> elemIndexColumns RHS vs
        in  fromMaybe 0 $ fmap (`index` x) mIx

    | otherwise                  =
        error "readValue: Variable not in tableau"


tableauVars
    :: (IsTableau p v s a c)
    => Tableau p v s a c
    -> TwoPhaseVars v
tableauVars t@(Tableau vs x) =
    TwoPhaseVars . Vec.zip names $ Vec.map toValue names
  where
    toValue i   = readValue i t / index (0, 0) x
    names       = fromJust . Vec.fromList $ findColumns isVar vs

    isVar n     = isDecision n || n == currentObj
    currentObj  = indexColumns 0 vs


-- Stepping

enteringVar
    :: (IsTableau p v s a c, cols ~ Cols p v s a)
    => (VarName -> Bool)
    -> Tableau p v s a c
    -> Either TwoPhaseStop (Finite cols)
enteringVar canEnter (Tableau vs x) =
    case filter isViable columns of
        [] -> Left NoEntering
        xs -> Right . fst $ List.minimumBy (comparing snd) xs
  where
    columns         = Vec.toList . Vec.indexed $ columnVars vs
    isViable (j, n) = index (0, j) x < 0 && canEnter n


leavingVar
    :: (IsTableau p v s a c, cols ~ Cols p v s a, rows ~ Rows p c)
    => Finite cols
    -> Tableau p v s a c
    -> Either TwoPhaseStop (Finite rows)
leavingVar enter (Tableau vs x) =
    case fmap toRatio $ filter isViable rows of
        [] -> Left Unbounded
        xs -> Right . fst $ List.minimumBy (comparing snd) xs
  where
    rows            = Vec.toList . Vec.indexed $ rowVars vs
    rhsColumn       = fromJust $ elemIndexColumns RHS vs

    toRatio (i, _)  =
        let val = index (i, enter) x
            rhs = index (i, rhsColumn) x
        in  (i, rhs / val)

    isViable (i, _) =
        let val = index (i, enter) x
            rhs = index (i, rhsColumn) x
        in  val /= 0 && signum val == signum rhs


tableauStep
    :: (IsTableau p v s a c)
    => (VarName -> Bool)
    -> Tableau p v s a c
    -> Either TwoPhaseStop (Tableau p v s a c)
tableauStep f t@(Tableau vs x) = do
    enter <- enteringVar f t
    leave <- leavingVar enter t

    let cell = (leave, enter)

    pure $ Tableau (updateRow cell vs) (pivot cell x)

