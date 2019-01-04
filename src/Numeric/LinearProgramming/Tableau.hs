{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}

module Numeric.LinearProgramming.Tableau where

import           GHC.TypeLits
import qualified Numeric.LinearAlgebra as LA
import           Numeric.LinearAlgebra.Static as LS

import           Numeric.LinearProgramming.Problem
import           Numeric.LinearProgramming.Tableau.Builder
import           Numeric.LinearProgramming.Tableau.Phase
import           Numeric.LinearProgramming.Tableau.VarMap


type IsTableau p v s a =
    ( KnownNat v
    , KnownNat s
    , KnownNat a
    , KnownNat (Rows p s a)
    , KnownNat (Cols p v s a)
    )


data Tableau (p :: Phase) (v :: Nat) (s :: Nat) (a :: Nat) where
    Tableau :: (IsTableau p v s a)
        => VarMap p (Rows p s a) (Cols p v s a)
        -> L (Rows p s a) (Cols p v s a)
        -> Tableau p v s a

instance Show (Tableau p v s a) where
    show (Tableau _ table) = LA.dispf 1 $ LS.unwrap table


mkPhaseI :: forall v s a. (IsTableau 'PhaseI v s a)
    => Problem v s a -> Tableau 'PhaseI v s a
mkPhaseI problem =
    case toTableau $ mkBuilder problem of
        Just tableau    -> tableau
        Nothing         -> error "mkPhaseI: Failed to create tableau"
  where
    toTableau b = Tableau <$> toVarMap b <*> toMatrix b

