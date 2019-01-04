{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeOperators      #-}

module Numeric.LinearProgramming.Problem where

import Data.Kind                    (Type)
import Data.Vector.Storable.Sized   (Vector)
import GHC.TypeLits


type Coeff      = Double
type Coeffs n   = Vector n Coeff


data Constraint (v :: Nat) :: Nat -> Nat -> Type where
    LEQ :: Coeffs v -> Double -> Constraint v 1 0
    GEQ :: Coeffs v -> Double -> Constraint v 1 0
    EQU :: Coeffs v -> Double -> Constraint v 0 1


type IsProblem v s a =
    (KnownNat v, KnownNat s, KnownNat a)


data Problem (v :: Nat) :: Nat -> Nat -> Type where
    Maximise :: (KnownNat v)
        => Coeffs v -> Problem v 0 0

    SuchThat :: (IsProblem v s1 a1, IsProblem v (s1 + s2) (a1 + a2))
        => Problem v s1 a1
        -> Constraint v s2 a2
        -> Problem v (s1 + s2) (a1 + a2)

