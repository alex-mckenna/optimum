{-# LANGUAGE DataKinds #-}

-- The "Numeric.LinearAlgebra.Static" module does not have parity for a lot
-- of the functionality in the main module. This module makes up for that by
-- defining some of the missing functions.
--
module Numeric.LinearProgramming.Internal.Matrix where

import Data.Finite                          (Finite)
import GHC.TypeLits
import Numeric.LinearAlgebra                ((!))
import Numeric.LinearAlgebra.Static as LS


-- Index into a sized matrix. This is presumably missing from hmatrix due to
-- the implementaion there being written before -XPolyKinds was added to GHC.
--
index
    :: (KnownNat rows, KnownNat cols)
    => (Finite rows, Finite cols)
    -> L rows cols
    -> Double
index (i, j) x =
    LS.unwrap x ! fromIntegral i ! fromIntegral j

