{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

module Numeric.Optimization.TwoPhase.Pivot where

import           Data.Finite                        (Finite)
import qualified Data.List as List                  (minimumBy)
import           Data.Maybe                         (listToMaybe)
import           Data.Ord                           (comparing)
import           Data.Proxy
import           GHC.TypeLits                       (KnownNat, natVal)
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


enteringFrom :: (KnownNat rows, KnownNat cols)
    => [Finite cols] -> L rows cols -> Maybe (Finite cols)
enteringFrom cols table =
    listToMaybe $ filter isNegative cols
  where
    isNegative j = index (0, j) table < 0


-- TODO: Will this fail if RHS is zero?
--
leavingFrom :: forall rows cols. (KnownNat rows, KnownNat cols)
    => Finite cols -> [Finite rows] -> L rows cols -> Maybe (Finite rows)
leavingFrom j rows table =
    case filter isPositive rows of
        []  -> Nothing
        xs  -> Just $ List.minimumBy (comparing ratioAt) xs
  where
    endRow          = fromIntegral . pred . natVal $ Proxy @rows
    isPositive i    = index (i, j) table > 0

    ratioAt i
        | rhs == 0  = 1.0e-16 / index (i, j) table
        | otherwise = rhs / index (i, j) table
      where
        rhs = index (endRow, j) table


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

