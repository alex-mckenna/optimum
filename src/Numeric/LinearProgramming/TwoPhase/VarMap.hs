{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE KindSignatures     #-}

module Numeric.LinearProgramming.TwoPhase.VarMap
    ( -- * Variable Maps
      VarMap(..)
    , IsVarMap
    , updateRow
    , findColumns
    , allRows
    ) where

import           Data.Finite                                (Finite)
import qualified Data.List as List
import           Data.Vector.Sized                          (Vector, (//))
import qualified Data.Vector.Sized as Vec
import           GHC.TypeLits                               (KnownNat, Nat)

import           Numeric.LinearProgramming.TwoPhase.Phase
import           Numeric.LinearProgramming.TwoPhase.VarName


type IsVarMap rows cols =
    (KnownNat rows, KnownNat cols)


data VarMap (p :: Phase) (rows :: Nat) (cols :: Nat)
    = VarMap
        { rowVars :: Vector rows VarName
        , colVars :: Vector cols VarName
        }
    deriving (Eq, Show)


-- Although column variables are fixed in a variable map, the rows
-- change as variables are added or removed from the basis.
--
updateRow
    :: (IsVarMap rows cols)
    => (Finite rows, Finite cols)
    -> VarMap p rows cols
    -> VarMap p rows cols
updateRow (i, j) (VarMap r c) =
    VarMap (r // [(i, Vec.index c j)]) c


findColumns
    :: (IsVarMap rows cols)
    => (VarName -> Bool)
    -> VarMap p rows cols
    -> [Finite cols]
findColumns f (VarMap _ c) =
    fmap fromIntegral . List.findIndices f $ Vec.toList c


allRows
    :: (IsVarMap rows cols)
    => VarMap p rows cols
    -> [Finite rows]
allRows (VarMap r _) =
    Vec.toList $ Vec.imap const r

