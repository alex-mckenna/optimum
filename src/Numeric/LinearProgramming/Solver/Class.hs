{-# LANGUAGE TypeFamilies #-}

module Numeric.LinearProgramming.Solver.Class where


class Solver method where
    type Error method
    type Result method

    isOptimal   :: method -> Bool
    toResult    :: method -> Result method
    step        :: method -> Either (Error method) method

