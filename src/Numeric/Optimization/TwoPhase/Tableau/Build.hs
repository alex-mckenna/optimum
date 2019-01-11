{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Numeric.Optimization.TwoPhase.Tableau.Build
    ( IsBuilder
    , mkTable
    ) where

import           Prelude hiding                             ((++))

import qualified Data.List as List
import           Data.Maybe                                 (fromJust)
import qualified Data.Vector.Sized as Vec
import           Data.Vector.Storable.Sized                 (Vector, (++))
import qualified Data.Vector.Storable.Sized as SVec
import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static               (L)
import qualified Numeric.LinearAlgebra.Static as LS
import qualified Numeric.LinearAlgebra.Static.Vector as LS

import           Numeric.Optimization.Problem
import           Numeric.Optimization.TwoPhase.Types


type IsBuilder v s a c =
    ( KnownNat v
    , KnownNat s
    , KnownNat a
    , KnownNat c
    )


data Builder (v :: Nat) (s :: Nat) (a :: Nat) (c :: Nat) = Builder
    { slackSupply       :: [Vector s Double]
    , artificialSupply  :: [Vector a Double]
    , rowsBuilt         :: [Vector (Cols 'PhaseI v s a) Double]
    , adjustmentsBuilt  :: [Vector (Cols 'PhaseI v s a) Double]
    }


mkBuilder :: (IsBuilder v s a c) => Builder v s a c
mkBuilder = Builder slackI artificialI [] []
  where
    slackI      = reverse . fmap LS.rVec . LS.toRows $ LS.eye
    artificialI = reverse . fmap LS.rVec . LS.toRows $ LS.eye


buildPhaseI
    :: forall v s a c.
        (IsBuilder v s a c)
    => Builder v s a c
    -> Builder v s a c
buildPhaseI acc = acc
    { rowsBuilt = row : rowsBuilt acc }
  where
    row = SVec.snoc (os ++ xs ++ ss ++ as) 0
    os  = SVec.fromTuple (1, 0)
    xs  = SVec.replicate @v 0
    ss  = SVec.replicate @s 0
    as  = SVec.replicate @a (-1)


buildPhaseII
    :: forall v s a c.
        (IsBuilder v s a c)
    => Builder v s a c
    -> Coeffs v
    -> Builder v s a c
buildPhaseII acc xs' = acc
    { rowsBuilt = row : rowsBuilt acc }
  where
    row = SVec.snoc (os ++ xs ++ ss ++ as) 0
    os  = SVec.fromTuple (0, 1)
    xs  = SVec.map negate xs'
    ss  = SVec.replicate @s 0
    as  = SVec.replicate @a 0


buildObjective :: (IsBuilder v s a c)
    => Builder v s a c -> Coeffs v -> Builder v s a c
buildObjective acc = buildPhaseI . buildPhaseII acc


buildLEQ
    :: forall v s a c.
        (IsBuilder v s a c)
    => Builder v s a c
    -> Coeffs v
    -> Double
    -> Builder v s a c
buildLEQ acc xs rhs = acc
    { slackSupply   = tail (slackSupply acc)
    , rowsBuilt     = row : rowsBuilt acc
    }
  where
    row = SVec.snoc (os ++ xs ++ ss ++ as) rhs
    os  = SVec.fromTuple (0, 0)
    ss  = head (slackSupply acc)
    as  = SVec.replicate @a 0


buildGEQ
    :: forall v s a c.
        (IsBuilder v s a c)
    => Builder v s a c
    -> Coeffs v
    -> Double
    -> Builder v s a c
buildGEQ acc xs' rhs' = acc
    { slackSupply       = tail (slackSupply acc)
    , artificialSupply  = tail (artificialSupply acc)
    , rowsBuilt         = row : rowsBuilt acc
    }
  where
    row = SVec.snoc (os ++ xs ++ ss ++ as) rhs
    os  = SVec.fromTuple (0, 0)
    xs  = SVec.map negate xs'
    ss  = head (slackSupply acc)
    as  = head (artificialSupply acc)
    rhs = negate rhs'


buildEQU
    :: forall v s a c.
        (IsBuilder v s a c)
    => Builder v s a c
    -> Coeffs v
    -> Double
    -> Builder v s a c
buildEQU acc xs rhs = acc
    { artificialSupply  = tail (artificialSupply acc)
    , rowsBuilt         = row : rowsBuilt acc
    , adjustmentsBuilt  = row : adjustmentsBuilt acc
    }
  where
    row = SVec.snoc (os ++ xs ++ ss ++ as) rhs
    os  = SVec.fromTuple (0, 0)
    ss  = SVec.replicate @s 0
    as  = head (artificialSupply acc)


buildConstraint :: (IsBuilder v s a c)
    => Builder v s a c -> Constraint v x y -> Builder v s a c
buildConstraint acc (xs :< y) = buildLEQ acc xs y
buildConstraint acc (xs :> y) = buildGEQ acc xs y
buildConstraint acc (xs := y) = buildEQU acc xs y


toMatrix :: forall v s a c. (IsBuilder v s a c)
    => Builder v s a c -> L (Rows 'PhaseI c) (Cols 'PhaseI v s a)
toMatrix acc =
    LS.rowsL . fromJust . Vec.fromList $ fmap LS.vecR adjusted
  where
    rows    = rowsBuilt acc
    adjusts = adjustmentsBuilt acc

    adjusted = case adjusts of
        [] -> rows
        xs ->
            let oldObjP1  = head rows
                newObjP1  = List.foldl1' (SVec.zipWith (+)) (oldObjP1 : xs)
            in  newObjP1 : tail rows


mkTable :: forall d v s a c. (IsBuilder v s a c)
    => Problem d v s a c -> L (Rows 'PhaseI c) (Cols 'PhaseI v s a)
mkTable = toMatrix . go mkBuilder
  where
    go :: Builder v s a c -> Problem d v x y z -> Builder v s a c
    go acc (Maximize xs)    = buildObjective acc xs
    go acc (Minimize xs)    = buildObjective acc xs
    go acc (SuchThat p c)   = go (buildConstraint acc c) p

