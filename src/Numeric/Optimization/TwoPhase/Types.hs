{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Optimization.TwoPhase.Types
    ( -- * Phases / Phase-Dependent Types
      Phase(..)
    , Rows
    , Cols
      -- * Variable Names
    , VarName(..)
    , isSpecial
    , isDecision
    , isSlack
    , isArtificial
      -- * Solver Types
    , TwoPhaseStop(..)
    , TwoPhaseVars(..)
    ) where

import GHC.TypeLits                     (Nat, type (+))
import Data.Vector.Sized                (Vector)

import Numeric.Optimization.Problem     (Direction)


-- Phases / Phase-Dependent Types

data Phase
    = PhaseI
    | PhaseII
    deriving (Eq, Ord)

instance Show Phase where
    show PhaseI  = "w"
    show PhaseII = "z"


type family Rows p c :: Nat where
    Rows 'PhaseI  c = 2 + c
    Rows 'PhaseII c = 1 + c


type family Cols p v s a :: Nat where
    Cols 'PhaseI  v s a = 2 + v + s + a + 1
    Cols 'PhaseII v s a = 1 + v + s + 1


-- Variable Names

data VarName
    = Objective Phase
    | Decision Int
    | Slack Int
    | Artificial Int
    | RHS
    deriving (Eq, Ord)

instance Show VarName where
    show (Objective p)          = show p
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


-- Solver Types

data TwoPhaseStop
    = Infeasible
    | Unbounded
    | NoEntering
    deriving (Eq, Show)


newtype TwoPhaseVars (d :: Direction) (v :: Nat) =
    TwoPhaseVars { getVars :: Vector (v + 1) (VarName, Double) }
    deriving (Eq, Show)

