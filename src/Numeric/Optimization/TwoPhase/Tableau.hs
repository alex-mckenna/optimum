{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Optimization.TwoPhase.Tableau
    ( -- * Tableaus
      Tableau
    , IsTableau
    , mkPhaseI
    , mkPhaseII
    , maximizeEntering
    , maximizeOptimal
    , minimizeEntering
    , minimizeOptimal
    , phaseIEntering
    , phaseIOptimal
    , tableauInfeasible
    , tableauLeaving
    , tableauResult
    , tableauStep
    ) where

import           Data.Finite                                    (Finite)
import qualified Data.List as List
import           Data.Maybe                                     (fromJust)
import           Data.Ord                                       (comparing)
import qualified Data.Vector.Sized as Vec
import qualified Data.Vector.Storable.Sized as SVec
import           GHC.TypeLits
import           Numeric.LinearAlgebra                          (Extractor(..), (??))
import qualified Numeric.LinearAlgebra as LA
import           Numeric.LinearAlgebra.Static                   (L)
import qualified Numeric.LinearAlgebra.Static as LS
import qualified Numeric.LinearAlgebra.Static.Vector as LS
import           Text.Printf                                    (printf)
import           Unsafe.Coerce                                  (unsafeCoerce)

import           Numeric.Optimization.Problem
import           Numeric.Optimization.TwoPhase.Tableau.Build
import           Numeric.Optimization.TwoPhase.Tableau.Pivot
import           Numeric.Optimization.TwoPhase.Tableau.VarMap
import           Numeric.Optimization.TwoPhase.Types


type IsTableau p v s a c =
    ( KnownNat v
    , KnownNat s
    , KnownNat a
    , KnownNat c
    , KnownNat (Rows p c)
    , KnownNat (Cols p v s a)
    )


data Tableau (p :: Phase) (d :: Direction) v s a c =
    Tableau
        { varMap :: VarMap (Rows p c) (Cols p v s a)
        , table  :: L (Rows p c) (Cols p v s a)
        }


instance (IsTableau p v s a c) => Show (Tableau p d v s a c) where
    show (Tableau vs x) = unlines (cols : rows)
      where
        cols        = concatMap (printf "%8s" . show)
            . Vec.toList $ columnVars vs

        rows        = zipWith (printf "%s   %s") rowVals rowNames
        rowVals     = lines . LA.format "" (printf "%8.2f") $ LS.unwrap x
        rowNames    = Vec.toList . Vec.map show $ rowVars vs


mkPhaseI
    :: (IsTableau 'PhaseI v s a c)
    => Problem d v s a c
    -> Tableau 'PhaseI d v s a c
mkPhaseI x =
    Tableau (mkVarMap x) (build x)


mkPhaseII
    :: forall d v s a c.
        ( IsTableau 'PhaseI  v s a c
        , IsTableau 'PhaseII v s a c
        )
    => Tableau 'PhaseI  d v s a c
    -> Tableau 'PhaseII d v s a c
mkPhaseII (Tableau vs x) =
    Tableau (VarMap newRows newColumns) newMatrix
  where
    newRows     = Vec.tail . unsafeCoerce $ rowVars vs

    newColumns  =
        let is = fromJust $ Vec.fromList indices
        in  Vec.backpermute (columnVars vs) is

    newMatrix   =
        let xs = LS.unwrap x ?? (Drop 1, Pos (LA.idxs indices))
        in  fromJust $ LS.create xs

    indices     = fromIntegral <$> findIndicesColumns isPhaseII vs
    isPhaseII n = n /= Objective PhaseI && not (isArtificial n)


-- Read the value associated with a variable in the tableau. If the variable is
-- in the basis (has a row in the tableau), it's value is the value in the RHS
-- of that row divided by the value in the variable's column in that row. If
-- the variable is not in the basis it has a value of 0 (non-basic).
--
readValue
    :: (IsTableau p v s a c)
    => VarName
    -> Tableau p d v s a c
    -> Double
readValue n (Tableau vs x) =
    case (valRowIx, valColIx, rhsColIx) of
        (Just i,  Just j, Just r) -> index (i, r) x / index (i, j) x
        (Nothing, Just _, Just _) -> 0
        _                         -> error "readValue: Variable not in tableau"
  where
    valRowIx    = elemIndexRows n vs
    valColIx    = elemIndexColumns n vs
    rhsColIx    = elemIndexColumns RHS vs


-- Read the value associated with the current objective and each decision
-- variable, and store them in the Vars type for this solver. This is what is
-- returned when optimization succeeds.
--
tableauResult
    :: (IsTableau p v s a c)
    => Tableau p d v s a c
    -> TwoPhaseResult d v
tableauResult x =
    TwoPhaseResult . Vec.zip names $ Vec.map toValue names
  where
    toValue i   = readValue i x
    names       = fromJust . Vec.fromList . findColumns isVar $ varMap x

    isVar n     =
        let objective = indexColumns 0 (varMap x)
        in  n == objective || isDecision n


-- A phase I tableau is infeasible if the value of the objective function is
-- greater than 0. This means that at least one artificial variable has to be
-- non-zero, so there can be no feasible BFS to start phase II from.
--
tableauInfeasible
    :: (IsTableau 'PhaseI v s a c)
    => Tableau 'PhaseI d v s a c
    -> Bool
tableauInfeasible x
    | phaseIOptimal x   = readValue (Objective PhaseI) x /= 0
    | otherwise         = False


-- A tableau is optimal if the given test passes for each cell corresponding to
-- a variable (everything apart from columns for objectives and the RHS column)
-- in the current objective.
--
tableauOptimal
    :: (IsTableau p v s a c)
    => (Double -> Bool)
    -> Tableau p d v s a c
    -> Bool
tableauOptimal f (Tableau vs x) =
    SVec.and . SVec.imap optimal . LS.rVec . head . Vec.toList $ LS.lRows x
  where
    coeffs                  = findIndicesColumns isCoeff vs
    isCoeff                 = not . isSpecial

    optimal i a
        | i `elem` coeffs   = f a
        | otherwise         = True


-- A phase I problem is always a minimization. This is a separate function
-- from 'minimizeOptimal' as the problem being solved in phase II may not be
-- a minimization problem.
--
phaseIOptimal
    :: (IsTableau 'PhaseI v s a c)
    => Tableau 'PhaseI d v s a c
    -> Bool
phaseIOptimal =
    tableauOptimal (<= 0)


-- A phase II problem which is a maximization is optimal when all coefficients
-- in the objective function are non-negative.
--
maximizeOptimal
    :: (IsTableau 'PhaseII v s a c)
    => Tableau 'PhaseII 'Max v s a c
    -> Bool
maximizeOptimal =
    tableauOptimal (>= 0)


-- A phase I problem which is a minimization is optimal when all coefficients
-- in the objective function are non-negative.
--
minimizeOptimal
    :: (IsTableau 'PhaseII v s a c)
    => Tableau 'PhaseII 'Min v s a c
    -> Bool
minimizeOptimal =
    tableauOptimal (<= 0)


-- The entering variable is the lowest indexed variable where the value in the
-- objective column passes a test, and the name of the variable passes another
-- test. The tests change depending on the phase, and whether the optimization
-- is a maximization or a minimization.
--
enteringVar
    :: (IsTableau p v s a c)
    => (Finite (Cols p v s a) -> Bool)
    -> (VarName -> Bool)
    -> Tableau p d v s a c
    -> Either TwoPhaseError (Finite (Cols p v s a))
enteringVar validCol validVar (Tableau vs _) =
    case filter canEnter columns of
        [] -> Left NoEntering
        xs -> Right . fst $ List.minimumBy (comparing snd) xs
  where
    canEnter (j, n) = validCol j && validVar n
    columns         = Vec.toList . Vec.indexed $ columnVars vs


-- In phase I, the entering variable must be a positive value, as the problem
-- is a minimization. Artificial variables are still in the table at this
-- point, so the valid variable test must make sure artificial variables cannot
-- enter the basis (as they should be non-basic).
--
phaseIEntering
    :: (IsTableau 'PhaseI v s a c)
    => Tableau 'PhaseI d v s a c
    -> Either TwoPhaseError (Finite (Cols 'PhaseI v s a))
phaseIEntering x =
    enteringVar validCol validVar x
  where
    validCol j = index (0, j) (table x) > 0
    validVar n = isDecision n || isSlack n


-- When maximizing, the variable to enter the basis is the lowest indexed
-- variable with a negative coefficient in the objective function row.
--
maximizeEntering
    :: (IsTableau 'PhaseII v s a c)
    => Tableau 'PhaseII 'Max v s a c
    -> Either TwoPhaseError (Finite (Cols 'PhaseII v s a))
maximizeEntering x =
    enteringVar validCol validVar x
  where
    validCol j = index (0, j) (table x) < 0
    validVar   = not . isSpecial


-- When minimizing, the variable to enter the basis is the lowest indexed
-- variable with a positive coefficient in the objective function row.
--
minimizeEntering
    :: (IsTableau 'PhaseII v s a c)
    => Tableau 'PhaseII 'Min v s a c
    -> Either TwoPhaseError (Finite (Cols 'PhaseII v s a))
minimizeEntering x =
    enteringVar validCol validVar x
  where
    validCol j = index (0, j) (table x) > 0
    validVar   = not . isSpecial


-- The variable to leave the basis is the lowest indexed variable which has
-- the minimum ratio between it's coefficient and the RHS value. A variable
-- with a coefficient of 0 is not allowed to leave (as the ratio is infinite).
-- This function is the same regardless of the direction of optimization.
--
tableauLeaving
    :: (IsTableau p v s a c)
    => Finite (Cols p v s a)
    -> Tableau p d v s a c
    -> Either TwoPhaseError (Finite (Rows p c))
tableauLeaving enter (Tableau vs x) =
    case toRatio <$> filter canLeave rows of
        [] -> Left Unbounded
        xs -> Right . fst $ List.minimumBy (comparing snd) xs
  where
    rows            = Vec.toList . Vec.indexed $ rowVars vs
    rhsColumn       = fromJust $ elemIndexColumns RHS vs

    toRatio (i, _)  =
        let val = index (i, enter) x
            rhs = index (i, rhsColumn) x
        in  (i, rhs / val)

    canLeave (i, n) =
        let val = index (i, enter) x
            rhs = index (i, rhsColumn) x
        in  val > 0 && rhs /= 0 && not (isSpecial n)


tableauStep
    :: (IsTableau p v s a c)
    => (Tableau p d v s a c
        -> Either TwoPhaseError (Finite (Cols p v s a)))
    -> Tableau p d v s a c
    -> Either TwoPhaseError (Tableau p d v s a c)
tableauStep tableauEntering t@(Tableau vs x) = do
    enter <- tableauEntering t
    leave <- tableauLeaving enter t

    let cell = (leave, enter)
    pure $ Tableau (updateRow cell vs) (pivot cell x)

