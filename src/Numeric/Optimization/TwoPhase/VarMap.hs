{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE KindSignatures     #-}

module Numeric.Optimization.TwoPhase.VarMap
    ( -- * Variable Maps
      VarMap(..)
    , IsVarMap
      -- * Columns
    , allColumns
    , indexColumns
    , findColumns
    , findIndicesColumns
    , elemIndexColumns
      -- * Rows
    , allRows
    , indexRows
    , elemIndexRows
    , updateRow
    ) where

import           Data.Finite                                (Finite)
import qualified Data.List as List
import           Data.Vector.Sized                          (Vector, (//))
import qualified Data.Vector.Sized as Vec
import           GHC.TypeLits                               (KnownNat, Nat)

import           Numeric.Optimization.TwoPhase.Types


type IsVarMap rows cols =
    (KnownNat rows, KnownNat cols)


data VarMap (p :: Phase) (rows :: Nat) (cols :: Nat)
    = VarMap
        { rowVars    :: Vector rows VarName
        , columnVars :: Vector cols VarName
        }
    deriving (Eq, Show)


allColumns
    :: (IsVarMap rows cols)
    => VarMap p rows cols
    -> [Finite cols]
allColumns =
    Vec.toList . Vec.imap const . columnVars


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


allRows
    :: (IsVarMap rows cols)
    => VarMap p rows cols
    -> [Finite rows]
allRows =
    Vec.toList . Vec.imap const . rowVars


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

