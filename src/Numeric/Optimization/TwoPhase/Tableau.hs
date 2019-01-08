{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Optimization.TwoPhase.Tableau
    ( -- * Tableaus
      Tableau(..)
    , IsTableau
    , mkPhaseI
    , mkPhaseII
    , canEnterPhaseI
    , canEnterPhaseII
      -- * Operations on Tableaus
    , tableauOptimalPhaseI
    , tableauOptimalPhaseII
    , tableauVars
    , tableauStep
      -- * TODO: DELETE THESE
    , p2Columns
    ) where

import           Data.Finite                            (Finite)
import qualified Data.List as List
import           Data.Maybe
import           Data.Ord                               (comparing)
import qualified Data.Vector.Sized as Vec
import           GHC.TypeLits
import           Numeric.LinearAlgebra                  (Extractor(..), idxs, (??))
import qualified Numeric.LinearAlgebra as LA
import           Numeric.LinearAlgebra.Static           (L)
import qualified Numeric.LinearAlgebra.Static as LS
import           Unsafe.Coerce

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

mkPhaseI :: (IsTableau 'PhaseI v s a c)
    => Problem v s a c -> Tableau 'PhaseI v s a c
mkPhaseI problem =
    Tableau (mkVarMap problem) (build problem)


mkPhaseII :: forall v s a c. (IsTableau 'PhaseI v s a c, IsTableau 'PhaseII v s a c)
    => Tableau 'PhaseI v s a c -> Tableau 'PhaseII v s a c
mkPhaseII (Tableau vs x) =
    Tableau (VarMap newRows newColumns) newMatrix
  where
    newRows     = Vec.tail . unsafeCoerce $ rowVars vs

    newColumns  = fromJust . Vec.fromList . filter isPhaseII . Vec.toList $ columnVars vs

    newMatrix   =
        let newElems = LS.unwrap x ?? (Drop 1, Pos (idxs indices))
        in  fromJust $ LS.create newElems

    indices     = fmap fromIntegral $ findIndicesColumns isPhaseII vs
    isPhaseII n = n /= Objective PhaseI && not (isArtificial n)


p2Columns
    :: forall v s a c cols.
        ( IsTableau 'PhaseI v s a c
        , IsTableau 'PhaseII v s a c
        , KnownNat cols
        , cols ~ Cols 'PhaseII v s a
        )
    => Tableau 'PhaseI v s a c
    -> Vec.Vector cols VarName
p2Columns (Tableau vs _) =
    Vec.backpermute (columnVars vs) indices
  where
    indices     = fromJust . Vec.fromListN @cols . fmap fromIntegral $ findIndicesColumns isPhaseII vs
    isPhaseII n = n /= Objective PhaseI && not (isArtificial n)


-- Checking Optimality


tableauOptimalPhaseI :: (IsTableau 'PhaseI v s a c)
    => Tableau 'PhaseI v s a c -> Bool
tableauOptimalPhaseI (Tableau vs x) =
    allCells (<= 0) coeffCells x
  where
    coeffCells  = [(0, j) | j <- findIndicesColumns isCoeff vs]
    isCoeff     = not . isSpecial


tableauOptimalPhaseII :: (IsTableau 'PhaseII v s a c)
    => Tableau 'PhaseII v s a c -> Bool
tableauOptimalPhaseII (Tableau vs x) =
    allCells (>= 0) coeffCells x
  where
    coeffCells  = [(0, j) | j <- findIndicesColumns isCoeff vs]
    isCoeff     = not . isSpecial


-- Extracting Results

readValue :: (IsTableau p v s a c)
    => VarName -> Tableau p v s a c -> Double
readValue n (Tableau vs x)
    | Vec.elem n (columnVars vs) =
        let mIx = (,) <$> elemIndexRows n vs <*> elemIndexColumns RHS vs
        in  fromMaybe 0 $ fmap (`index` x) mIx

    | otherwise                  =
        error "readValue: Variable not in tableau"


tableauVars :: (IsTableau p v s a c)
    => Tableau p v s a c -> TwoPhaseVars v
tableauVars t@(Tableau vs x) =
    TwoPhaseVars . Vec.zip names $ Vec.map toValue names
  where
    toValue i   = readValue i t / index (0, 0) x
    names       = fromJust . Vec.fromList $ findColumns isVar vs

    isVar n     = isDecision n || n == currentObj
    currentObj  = indexColumns 0 vs


-- Stepping

canEnterPhaseI
    :: (IsTableau 'PhaseI v s a c)
    => (Finite (Cols 'PhaseI v s a), VarName)
    -> Tableau 'PhaseI v s a c
    -> Bool
canEnterPhaseI (j, n) (Tableau _ x) =
    canEnterVar && isPositive
  where
    canEnterVar = isDecision n || isSlack n
    isPositive  = index (0, j) x > 0


canEnterPhaseII
    :: (IsTableau 'PhaseII v s a c)
    => (Finite (Cols 'PhaseII v s a), VarName)
    -> Tableau 'PhaseII v s a c
    -> Bool
canEnterPhaseII (j, n) (Tableau _ x) =
    canEnterVar && isNegative
  where
    canEnterVar = not $ isSpecial n
    isNegative  = index (0, j) x < 0


enteringVar
    :: (IsTableau p v s a c, cols ~ Cols p v s a)
    => ((Finite cols, VarName) -> Tableau p v s a c -> Bool)
    -> Tableau p v s a c
    -> Either TwoPhaseStop (Finite cols)
enteringVar canEnter t@(Tableau vs _) =
    case filter (`canEnter` t) columns of
        [] -> Left NoEntering
        xs -> Right . fst $ List.minimumBy (comparing snd) xs
  where
    columns = Vec.toList . Vec.indexed $ columnVars vs


leavingVar :: (IsTableau p v s a c, cols ~ Cols p v s a, rows ~ Rows p c)
    => Finite cols -> Tableau p v s a c -> Either TwoPhaseStop (Finite rows)
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
            var = indexRows i vs
        in  val > 0 && rhs /= 0 && not (isSpecial var)


tableauStep
    :: (IsTableau p v s a c, cols ~ Cols p v s a)
    => ((Finite cols, VarName) -> Tableau p v s a c -> Bool)
    -> Tableau p v s a c
    -> Either TwoPhaseStop (Tableau p v s a c)
tableauStep f t@(Tableau vs x) = do
    enter <- enteringVar f t
    leave <- leavingVar enter t

    let cell = (leave, enter)

    pure $ Tableau (updateRow cell vs) (pivot cell x)

