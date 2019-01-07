{-# LANGUAGE TypeFamilies #-}

module Numeric.Optimization.Solver.Class where


class Solver method where
    type Stop method
    type Vars method

    isOptimal   :: method -> Bool
    toResult    :: method -> Vars method
    step        :: method -> Either (Stop method) method

