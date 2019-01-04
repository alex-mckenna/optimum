{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Numeric.LinearProgramming.Tableau.VarMap where

import           Data.Finite                (Finite)
import           Data.Kind
import           Data.Vector.Sized          (Vector, (//))
import qualified Data.Vector.Sized as Vec
import           GHC.TypeLits


data VarName
    = ObjectiveP1
    | ObjectiveP2
    | Decision Int
    | Slack Int
    | Artificial Int
    | RHS
    deriving (Eq)

instance Show VarName where
    show ObjectiveP1    = "w"
    show ObjectiveP2    = "z"
    show (Decision i)   = "x" <> show i
    show (Slack i)      = "s" <> show i
    show (Artificial i) = "a" <> show i
    show RHS            = "="


type IsVarMap rows cols =
    (KnownNat rows, KnownNat cols)


data VarMap :: Nat -> Nat -> Type where
    VarMap :: (IsVarMap rows cols)
        => Vector rows VarName -> Vector cols VarName -> VarMap rows cols


updateRow :: (IsVarMap rows cols)
    => Finite rows -> Finite cols -> VarMap rows cols -> VarMap rows cols
updateRow i j (VarMap r c) =
    VarMap (r // [(i, Vec.index c j)]) c

