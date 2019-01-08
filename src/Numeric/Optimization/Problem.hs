{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}

module Numeric.Optimization.Problem
    ( -- * Linear Optimisation Problems
      Coeff
    , Coeffs
    , Constraint
    , Problem
    , IsProblem
      -- * Smart Constructors
    , maximize
    , minimize
    , suchThat
    , leq
    , geq
    , equ
      -- * Pattern Synonyms
    , pattern Maximize
    , pattern SuchThat
    , pattern (:<)
    , pattern (:>)
    , pattern (:=)
    ) where

import           Data.Kind                          (Type)
import           Data.Vector.Storable.Sized         (Vector)
import qualified Data.Vector.Storable.Sized as SVec
import           GHC.TypeLits                       (KnownNat, Nat, type (+))


type Coeff      = Double
type Coeffs n   = Vector n Coeff


-- Constraints are specified as a vector of coefficients (one for each
-- decision variable) and a scalar RHS value. They are indexed by the number
-- of decision variables, number of slack variables and number of artificial
-- variables.
--
-- As Haskell currently lacks proper support for dependent types, smart
-- constructors are used to ensure that the RHS value for each constraint is
-- a positive number. This is the only way to identify the number of
-- artificial variables statically without needing dependent typing.
--
data Constraint :: Nat -> Nat -> Nat -> Type where
    LEQ :: Coeffs v -> Double -> Constraint v 1 0
    GEQ :: Coeffs v -> Double -> Constraint v 1 1
    EQU :: Coeffs v -> Double -> Constraint v 0 1

{-# COMPLETE (:<), (:>), (:=) #-}

pattern (:<) :: Coeffs v -> Double -> Constraint v s a
pattern xs :< y <- LEQ xs y

pattern (:>) :: Coeffs v -> Double -> Constraint v s a
pattern xs :> y <- GEQ xs y

pattern (:=) :: Coeffs v -> Double -> Constraint v s a
pattern xs := y <- EQU xs y


leq :: Coeffs v -> Double -> Constraint v 1 0
leq xs y
    | y < 0     = error "leq: RHS must be non-negative"
    | otherwise = LEQ xs y


geq :: Coeffs v -> Double -> Constraint v 1 1
geq xs y
    | y < 0     = error "geq: RHS must be non-negative"
    | otherwise = GEQ xs y


equ :: Coeffs v -> Double -> Constraint v 0 1
equ xs y
    | y < 0     = error "equ: RHS must be non-negative"
    | otherwise = EQU xs y


type IsProblem v s a c =
    (KnownNat v, KnownNat s, KnownNat a, KnownNat c)


data Problem :: Nat -> Nat -> Nat -> Nat -> Type where
    Optimize :: (KnownNat v)
        => Coeffs v -> Problem v 0 0 0

    Constrained
        :: (IsProblem v s1 a1 c, IsProblem v (s1 + s2) (a1 + a2) (c + 1))
        => Problem v s1 a1 c
        -> Constraint v s2 a2
        -> Problem v (s1 + s2) (a1 + a2) (c + 1)

{-# COMPLETE Maximize, SuchThat #-}

pattern Maximize
    :: ()
    => (KnownNat v)
    => Coeffs v
    -> Problem v s a c
pattern Maximize xs <- Optimize xs

pattern SuchThat
    :: forall v a b c.
        ()
    => forall s1 a1 s2 a2 c1.
        ( a ~ (s1 + s2)
        , b ~ (a1 + a2)
        , c ~ (c1 + 1)
        , IsProblem v s1 a1 c1
        , IsProblem v a b c
        )
    => Problem v s1 a1 c1
    -> Constraint v s2 a2
    -> Problem v a b c
pattern SuchThat p c <- Constrained p c


maximize :: (KnownNat v) => Coeffs v -> Problem v 0 0 0
maximize = Optimize


minimize :: (KnownNat v) => Coeffs v -> Problem v 0 0 0
minimize = Optimize . SVec.map negate


suchThat
    :: (IsProblem v s1 a1 c, IsProblem v (s1 + s2) (a1 + a2) (c + 1))
    => Problem v s1 a1 c
    -> Constraint v s2 a2
    -> Problem v (s1 + s2) (a1 + a2) (c + 1)
suchThat = Constrained

