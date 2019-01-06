{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}

module Numeric.LinearProgramming.Tableau.VarMap
    ( -- Variable Names
      VarName(..)
    , isSpecial
    , isArtificial
    , isSlack
    , isDecision
      -- Variable Maps
    , VarMap(..)
    , IsVarMap
    , lookupCol
    , lookupRow
    , updateRow
    , findCols
    , findRows
    , colIndex
    , rowIndex
    ) where

import           Data.Finite                                (Finite)
import           Data.Kind                                  (Type)
import qualified Data.List as List
import           Data.Vector.Sized                          (Vector, (//))
import qualified Data.Vector.Sized as Vec
import           GHC.TypeLits                               (Nat, KnownNat)

import           Numeric.LinearProgramming.Tableau.Phase


data VarName
    = Objective Phase
    | Decision Int
    | Slack Int
    | Artificial Int
    | RHS
    deriving (Eq)

instance Show VarName where
    show (Objective PhaseI)     = "w"
    show (Objective PhaseII)    = "z"
    show (Decision i)           = "x" <> show i
    show (Slack i)              = "s" <> show i
    show (Artificial i)         = "a" <> show i
    show RHS                    = "="


isSpecial :: VarName -> Bool
isSpecial Objective{} = True
isSpecial RHS         = True
isSpecial _           = False


isDecision :: VarName -> Bool
isDecision Decision{} = True
isDecision _          = False


isSlack :: VarName -> Bool
isSlack Slack{} = True
isSlack _       = False


isArtificial :: VarName -> Bool
isArtificial Artificial{} = True
isArtificial _            = False


type IsVarMap rows cols =
    (KnownNat rows, KnownNat cols)


data VarMap (p :: Phase) :: Nat -> Nat -> Type where
    VarMap :: (IsVarMap rows cols)
        => Vector rows VarName -> Vector cols VarName -> VarMap p rows cols


lookupCol :: (IsVarMap rows cols)
    => Finite cols -> VarMap p rows cols -> VarName
lookupCol i (VarMap _ cs) =
    Vec.index cs i


lookupRow :: (IsVarMap rows cols)
    => Finite rows -> VarMap p rows cols -> VarName
lookupRow i (VarMap rs _) =
    Vec.index rs i


-- When the variable map is created (by the Builder), all column variables
-- remain fixed. However, the row variables change during optimisation (as
-- rows keep track of which variables are in the basis).
--
-- In order to prevent new variables being introduced into the basis, the
-- variable to enter is found by indexing into the column variables.
--
updateRow :: (IsVarMap rows cols, index ~ (Finite rows, Finite cols))
    => index
    -> VarMap p rows cols
    -> VarMap p rows cols
updateRow (i, j) (VarMap r c) =
    VarMap (r // [(i, Vec.index c j)]) c


findCols :: (IsVarMap rows cols)
    => (VarName -> Bool)
    -> VarMap p rows cols
    -> [Finite cols]
findCols f (VarMap _ c) =
    fmap fromIntegral . List.findIndices f $ Vec.toList c


findRows :: (IsVarMap rows cols)
    => (VarName -> Bool)
    -> VarMap p rows cols
    -> [Finite rows]
findRows f (VarMap r _) =
    fmap fromIntegral . List.findIndices f $ Vec.toList r


colIndex :: (IsVarMap rows cols)
    => VarName
    -> VarMap p rows cols
    -> Maybe (Finite cols)
colIndex n (VarMap _ c) =
    Vec.elemIndex n c


rowIndex :: (IsVarMap rows cols)
    => VarName
    -> VarMap p rows cols
    -> Maybe (Finite rows)
rowIndex n (VarMap r _) =
    Vec.elemIndex n r

