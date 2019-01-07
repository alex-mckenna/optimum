{-# LANGUAGE TypeFamilies #-}

module Numeric.Optimization.DummySolver where

import Numeric.Optimization.Solver.Class


newtype DummySolver = DummySolver { state :: Int }
    deriving (Show)


instance Solver DummySolver where
    type Stop DummySolver = ()
    type Vars DummySolver = Int

    isOptimal   = (== 10) . state
    toResult    = state
    step        = pure . DummySolver . succ . state


mkDummy :: DummySolver
mkDummy = DummySolver 0

