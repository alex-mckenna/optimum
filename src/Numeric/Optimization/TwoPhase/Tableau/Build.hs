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
    , build
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


data Builder (v :: Nat) (s :: Nat) (a :: Nat) (c :: Nat) =
    Builder
        { slackSupply       :: [Vector s Double]
        , artificialSupply  :: [Vector a Double]
        , coeffsBuilt       :: [Vector (2 + v) Double]
        , slackBuilt        :: [Vector s Double]
        , artificialBuilt   :: [Vector a Double]
        , rhsBuilt          :: [Double]
        }
    deriving (Show)


mkBuilder :: forall v s a c. (IsBuilder v s a c) => Builder v s a c
mkBuilder = Builder slackI artificialI [] [] [] []
  where
    slackI      = reverse . fmap LS.rVec . LS.toRows $ LS.eye @s
    artificialI = reverse . fmap LS.rVec . LS.toRows $ LS.eye @a


buildPhaseI :: forall v s a c. (IsBuilder v s a c)
    => Builder v s a c -> Builder v s a c
buildPhaseI acc@Builder{..} = acc
    { coeffsBuilt       = coeffs : coeffsBuilt
    , slackBuilt        = slack : slackBuilt
    , artificialBuilt   = artificial : artificialBuilt
    , rhsBuilt          = rhs : rhsBuilt
    }
  where
    coeffs      = SVec.fromTuple (1, 0) ++ SVec.replicate @v 0
    slack       = SVec.replicate 0
    artificial  = SVec.replicate (-1)
    rhs         = 0


buildPhaseII :: (IsBuilder v s a c)
    => Builder v s a c -> Coeffs v -> Builder v s a c
buildPhaseII acc@Builder{..} xs = acc
    { coeffsBuilt       = coeffs : coeffsBuilt
    , slackBuilt        = slack : slackBuilt
    , artificialBuilt   = artificial : artificialBuilt
    , rhsBuilt          = rhs : rhsBuilt
    }
  where
    coeffs      = SVec.fromTuple (0, 1) ++ SVec.map negate xs
    slack       = SVec.replicate 0
    artificial  = SVec.replicate 0
    rhs         = 0


buildObjective :: (IsBuilder v s a c)
    => Builder v s a c -> Coeffs v -> Builder v s a c
buildObjective acc = buildPhaseI . buildPhaseII acc


buildLEQ :: (IsBuilder v s a c)
    => Builder v s a c -> Coeffs v -> Double -> Builder v s a c
buildLEQ acc@Builder{..} xs rhs = acc
    { slackSupply       = tail slackSupply
    , coeffsBuilt       = coeffs : coeffsBuilt
    , slackBuilt        = slack : slackBuilt
    , artificialBuilt   = artificial : artificialBuilt
    , rhsBuilt          = rhs : rhsBuilt
    }
  where
    coeffs      = SVec.replicate 0 ++ xs
    slack       = head slackSupply
    artificial  = SVec.replicate 0


-- TODO: Redefine in terms of buildLEQ
buildGEQ :: (IsBuilder v s a c)
    => Builder v s a c -> Coeffs v -> Double -> Builder v s a c
buildGEQ acc@Builder{..} xs rhs = acc
    { slackSupply       = tail slackSupply
    , artificialSupply  = tail artificialSupply
    , coeffsBuilt       = coeffs : coeffsBuilt
    , slackBuilt        = slack : slackBuilt
    , artificialBuilt   = artificial : artificialBuilt
    , rhsBuilt          = negate rhs : rhsBuilt
    }
  where
    coeffs      = SVec.replicate 0 ++ SVec.map negate xs
    slack       = head slackSupply
    artificial  = head artificialSupply


buildEQU :: (IsBuilder v s a c)
    => Builder v s a c -> Coeffs v -> Double -> Builder v s a c
buildEQU acc@Builder{..} xs rhs = acc
    { artificialSupply  = tail artificialSupply
    , coeffsBuilt       = coeffs : coeffsBuilt
    , slackBuilt        = slack : slackBuilt
    , artificialBuilt   = artificial : artificialBuilt
    , rhsBuilt          = rhs : rhsBuilt
    }
  where
    coeffs      = SVec.replicate 0 ++ xs
    slack       = SVec.replicate 0
    artificial  = head artificialSupply


buildConstraint :: (IsBuilder v s a c)
    => Builder v s a c -> Constraint v x y -> Builder v s a c
buildConstraint acc (xs :< y) = buildLEQ acc xs y
buildConstraint acc (xs :> y) = buildGEQ acc xs y
buildConstraint acc (xs := y) = buildEQU acc xs y


toMatrix :: forall v s a c. (IsBuilder v s a c)
    => Builder v s a c -> L (Rows 'PhaseI c) (Cols 'PhaseI v s a)
toMatrix Builder{..} =
    LS.rowsL . fromJust . Vec.fromList
        . fmap LS.vecR $ adjustedObj : tail rows
  where
    rows            = fmap fst rowsAndAdjusts
    adjustedObj     = SVec.zipWith (+) (head rows) adjustment
    adjustment      = List.foldl1' (SVec.zipWith (+))
        $ fmap snd rowsAndAdjusts

    rowsAndAdjusts  = List.zipWith4 toRowAdjust
        coeffsBuilt slackBuilt artificialBuilt rhsBuilt

    toRowAdjust cs ss as y
        | useRow    = (row, row)
        | otherwise = (row, SVec.replicate 0)
      where
        row     = SVec.snoc (cs ++ ss ++ as) y
        useRow  = SVec.index cs 0 == 0 && SVec.any (== 1) as


build :: forall d v s a c. (IsBuilder v s a c)
    => Problem d v s a c -> L (Rows 'PhaseI c) (Cols 'PhaseI v s a)
build = toMatrix . go mkBuilder
  where
    go :: Builder v s a c -> Problem d v x y z -> Builder v s a c
    go acc (Maximize xs)    = buildObjective acc xs
    go acc (Minimize xs)    = buildObjective acc xs
    go acc (SuchThat p c)   = go (buildConstraint acc c) p

