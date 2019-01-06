module Numeric.LinearProgramming.TwoPhase.VarName
    ( -- * Variable Names
      VarName(..)
    , isDecision
    , isSlack
    , isArtificial
    , isSpecial
    ) where

import Numeric.LinearProgramming.TwoPhase.Phase     (Phase(..))


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

