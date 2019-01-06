{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeFamilies       #-}

module Numeric.LinearProgramming.Tableau.Pivot where

import Data.Finite (Finite)
import GHC.TypeLits (KnownNat)
import Numeric.LinearAlgebra ((!))
import Numeric.LinearAlgebra.Static as LS

-- TODO: Submodule cannot import parent
import Numeric.LinearProgramming.Tableau


pivot :: (IsTableau p v s a, rows ~ Rows p s a, cols ~ Cols p v s a)
    => (Finite rows, Finite cols) -> Tableau p v s a -> Tableau p v s a
pivot cell (Tableau vars table) =
    Tableau (updateRow cell vars) (pivotMatrix cell table)


pivotMatrix :: (KnownNat rows, KnownNat cols)
    => (Finite rows, Finite cols) -> L rows cols -> L rows cols
pivotMatrix (x', y') table =
    LS.build (newValue $ LS.unwrap table)
  where
    (x, y)          = (fromIntegral x', fromIntegral y')

    newValue mat i' j'
        | i == x    = mat!i!j / mat!x!y
        | j == y    = 0
        | otherwise = mat!x!y * mat!i!j - mat!x!j * mat!i!y
      where
        (i, j) = (round i', round j')

