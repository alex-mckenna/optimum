{-# LANGUAGE TypeFamilies #-}

module Numeric.Optimization.Dummy
    ( Dummy
    , dummy
    ) where

import Numeric.Optimization.Problem
import Numeric.Optimization.Solver.Class


newtype Dummy = Dummy { state :: Int }
    deriving (Show)


instance Solver Dummy where
    type CanSolve Dummy = ()
    type Stop     Dummy = ()
    type Vars     Dummy = Int

    isOptimal   = (== 10) . state
    toResult    = state
    step        = pure . Dummy . succ . state


dummy :: Problem v s a -> Dummy
dummy = const (Dummy 0)

