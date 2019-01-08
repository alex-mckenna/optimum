{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}

module Numeric.Optimization.TwoPhase.VarMap
    ( -- * Variable Maps
      VarMap(..)
    , IsVarMap
    , mkVarMap
      -- * Columns
    , indexColumns
    , findColumns
    , findIndicesColumns
    , elemIndexColumns
      -- * Rows
    , indexRows
    , elemIndexRows
    , updateRow
    ) where

import           Prelude hiding                         ((++))

import           Data.Finite                            (Finite)
import qualified Data.List as List
import           Data.Maybe                             (fromJust)
import           Data.Proxy
import           Data.Vector.Sized                      (Vector, (++), (//))
import qualified Data.Vector.Sized as Vec
import           GHC.TypeLits

import           Numeric.Optimization.Problem
import           Numeric.Optimization.TwoPhase.Types


type IsVarMap rows cols =
    (KnownNat rows, KnownNat cols)


data VarMap (p :: Phase) (rows :: Nat) (cols :: Nat)
    = VarMap
        { rowVars    :: Vector rows VarName
        , columnVars :: Vector cols VarName
        }
    deriving (Eq, Show)


mkVarMap
    :: forall v s a c rows cols.
        ( KnownNat v
        , KnownNat s
        , KnownNat a
        , KnownNat c
        , KnownNat rows
        , KnownNat cols
        , rows ~ Rows 'PhaseI c
        , cols ~ Cols 'PhaseI v s a
        )
    => Problem v s a c
    -> VarMap 'PhaseI rows cols
mkVarMap =
    flip VarMap cols . fromJust . Vec.fromList . go []
  where
    cols        = Vec.snoc (obj ++ decision ++ slack ++ artificial) RHS
    obj         = Vec.fromTuple (Objective PhaseI, Objective PhaseII)
    decision    = Vec.generate @v (Decision . fromIntegral)
    slack       = Vec.generate @s (Slack . fromIntegral)
    artificial  = Vec.generate @a (Artificial . fromIntegral)

    go :: forall x y z. [VarName] -> Problem v x y z -> [VarName]
    go acc (Maximize _)   = Objective PhaseI : Objective PhaseII : acc
    go acc (SuchThat p c) = case c of
        (_:=_)  -> go (lastArtificial : acc) p
        _       -> go (lastSlack : acc) p
      where
        lastSlack       = Slack . fromIntegral . pred . natVal $ Proxy @x
        lastArtificial  = Artificial . fromIntegral . pred . natVal $ Proxy @y


indexColumns
    :: (IsVarMap rows cols)
    => Finite cols
    -> VarMap p rows cols
    -> VarName
indexColumns i x =
    Vec.index (columnVars x) i


findColumns
    :: (IsVarMap rows cols)
    => (VarName -> Bool)
    -> VarMap p rows cols
    -> [VarName]
findColumns f =
    List.filter f . Vec.toList . columnVars


findIndicesColumns
    :: (IsVarMap rows cols)
    => (VarName -> Bool)
    -> VarMap p rows cols
    -> [Finite cols]
findIndicesColumns f =
    fmap fromIntegral . List.findIndices f . Vec.toList . columnVars


elemIndexColumns
    :: (IsVarMap rows cols)
    => VarName
    -> VarMap p rows cols
    -> Maybe (Finite cols)
elemIndexColumns n =
    Vec.elemIndex n . columnVars


indexRows
    :: (IsVarMap rows cols)
    => Finite rows
    -> VarMap p rows cols
    -> VarName
indexRows i x =
    Vec.index (rowVars x) i


elemIndexRows
    :: (IsVarMap rows cols)
    => VarName
    -> VarMap p rows cols
    -> Maybe (Finite rows)
elemIndexRows n =
    Vec.elemIndex n . rowVars


updateRow
    :: (IsVarMap rows cols)
    => (Finite rows, Finite cols)
    -> VarMap p rows cols
    -> VarMap p rows cols
updateRow (i, j) (VarMap r c) =
    VarMap (r // [(i, Vec.index c j)]) c

