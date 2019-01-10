{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

module Numeric.Optimization.TwoPhase.Tableau.Pivot
    ( index
    , allCells
    , pivot
    ) where

import           Data.Finite                        (Finite)
import           GHC.TypeLits                       (KnownNat)
import           Numeric.LinearAlgebra              ((!))
import qualified Numeric.LinearAlgebra as LA        (atIndex)
import           Numeric.LinearAlgebra.Static       (L)
import qualified Numeric.LinearAlgebra.Static as LS


type Index rows cols = (Finite rows, Finite cols)


index :: (KnownNat rows, KnownNat cols)
    => Index rows cols
    -> L rows cols
    -> Double
index (i, j) x =
    LA.atIndex (LS.unwrap x) ix
  where
    ix = (fromIntegral i, fromIntegral j)


allCells :: (KnownNat rows, KnownNat cols)
    => (Double -> Bool)
    -> [Index rows cols]
    -> L rows cols
    -> Bool
allCells f is x =
    all f $ fmap (\i -> index i x) is


pivot :: (KnownNat rows, KnownNat cols)
    => (Finite rows, Finite cols) -> L rows cols -> L rows cols
pivot (x', y') table =
    LS.build (newValue $ LS.unwrap table)
  where
    (x, y)          = (fromIntegral x', fromIntegral y')

    newValue mat i' j'
        | i == x    = mat!i!j / mat!x!y
        | j == y    = 0
        | otherwise = mat!x!y * mat!i!j - mat!x!j * mat!i!y
      where
        (i, j) = (round i', round j')

