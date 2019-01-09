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
    , tableauInfeasible
    , tableauOptimalPhaseI
    , tableauOptimalPhaseII
    , tableauVars
    , tableauStep
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
import           Text.Printf
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
    , KnownNat (Rows p c)
    , KnownNat (Cols p v s a)
    )


data Tableau (p :: Phase) (v :: Nat) (s :: Nat) (a :: Nat) (c :: Nat)
    = Tableau
        { varMap :: VarMap p (Rows p c) (Cols p v s a)
        , table  :: L (Rows p c) (Cols p v s a)
        }


instance (IsTableau p v s a c) => Show (Tableau p v s a c) where
    show (Tableau vs x) = unlines (cols : rows)
      where
        cols    = concatMap (printf "%8s" . show) . Vec.toList $ columnVars vs
        rows    = zipWith (printf "%s   %s") rowData rowName

        rowData = lines . LA.format "" (printf "%8.2f") $ LS.unwrap x
        rowName = fmap show . Vec.toList $ rowVars vs


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

    newColumns  =
        let is = fromJust $ Vec.fromList indices
        in  Vec.backpermute (columnVars vs) is

    newMatrix   =
        let newElems = LS.unwrap x ?? (Drop 1, Pos (idxs indices))
        in  fromJust $ LS.create newElems

    indices     = fmap fromIntegral $ findIndicesColumns isPhaseII vs
    isPhaseII n = n /= Objective PhaseI && not (isArtificial n)


-- Checking Optimality


tableauInfeasible :: (IsTableau 'PhaseI v s a c)
    => Tableau 'PhaseI v s a c
    -> Bool
tableauInfeasible t
    | tableauOptimalPhaseI t    = readValue RHS t /= 0
    | otherwise                 = False


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
readValue n (Tableau vs x) =
    case (valRowIx, valColIx, rhsColIx) of
        (Just i,  Just j, Just r) -> index (i, r) x / index (i, j) x
        (Nothing, Just _, Just _) -> 0
        _                         -> error "readValue: Variable not in tableau"
  where
    valRowIx    = elemIndexRows n vs
    valColIx    = elemIndexColumns n vs
    rhsColIx    = elemIndexColumns RHS vs


tableauVars :: (IsTableau p v s a c)
    => Tableau p v s a c -> TwoPhaseVars v
tableauVars t@(Tableau vs _) =
    TwoPhaseVars . Vec.zip names $ Vec.map toValue names
  where
    toValue i   = readValue i t
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

