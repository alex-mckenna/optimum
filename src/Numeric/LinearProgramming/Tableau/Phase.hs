{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.LinearProgramming.Tableau.Phase where

import GHC.TypeLits (Nat, type (+))


data Phase
    = PhaseI
    | PhaseII


type family Cols (p :: Phase) (v :: Nat) (s :: Nat) (a :: Nat) :: Nat where
    Cols 'PhaseI  v s a = 2 + v + s + a + 2
    Cols 'PhaseII v s a = 1 + v + s + a + 2


type family Rows (p :: Phase) (s :: Nat) (a :: Nat) :: Nat where
    Rows 'PhaseI  s a = 2 + s + a
    Rows 'PhaseII s a = 1 + s + a

