{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Numeric.LinearProgramming.TwoPhase.Builder
    ( -- Building Tableaus
      mkBuilder
    , toMatrix
    , toVarMap
    ) where

import           Prelude hiding                             ((++))

import qualified Data.List as List
import           Data.Proxy
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Sized as Vec
import           Data.Vector.Storable.Sized                 ((++))
import qualified Data.Vector.Storable.Sized as SVec
import           GHC.TypeLits
import           Numeric.LinearAlgebra as LA hiding         ((<>))
import           Numeric.LinearAlgebra.Static               (L)
import qualified Numeric.LinearAlgebra.Static as LS
import qualified Numeric.LinearAlgebra.Static.Vector as LS

import           Numeric.LinearProgramming.Problem
import           Numeric.LinearProgramming.TwoPhase.Types
import           Numeric.LinearProgramming.TwoPhase.VarMap


type IsBuilder v s a =
    (KnownNat v, KnownNat s, KnownNat a)


data Builder (v :: Nat) (s :: Nat) (a :: Nat) = Builder
    { objectiveP1   :: [Double]
    , objectiveP2   :: [Double]
    , artificial    :: [Double]
    , rhs           :: [Double]
    , coeffs        :: [Coeffs v]
    , rowNames      :: [VarName]
    , colNames      :: [VarName]
    } deriving (Eq, Show)

instance Semigroup (Builder v s a) where
    Builder wx zx ax rx cx rvx cvx <> Builder wy zy ay ry cy rvy cvy = Builder
        { objectiveP1 = wx  <> wy
        , objectiveP2 = zx  <> zy
        , artificial  = ax  <> ay
        , rhs         = rx  <> ry
        , coeffs      = cx  <> cy
        , rowNames    = rvx <> rvy
        , colNames    = cvx <> cvy
        }


singleton
    :: Double
    -> Double
    -> Double
    -> Double
    -> Coeffs v
    -> VarName
    -> VarName
    -> Builder v s a
singleton w z a r c rv cv =
    Builder [w] [z] [a] [r] [c] [rv] [cv]


nextSlack :: forall v s a. (IsBuilder v s a)
    => Builder v s a -> VarName
nextSlack builder =
    case List.find isSlack (colNames builder) of
        Just (Slack i)  -> Slack (i - 1)
        _               -> Slack (numSlack - 1)
  where
    numSlack = fromIntegral . natVal $ Proxy @s


nextArtificial :: Builder v s a -> VarName
nextArtificial builder =
    case List.find isArtificial (colNames builder) of
        Just (Artificial i) -> Artificial (i - 1)
        _                   -> error "nextArtificial: No artificial variables"


buildW :: (IsBuilder v s a)
    => Builder v s a -> Builder v s a
buildW = (singleton 1 0 1 0 xs obj obj <>)
  where
    xs  = SVec.replicate 0
    obj = Objective PhaseI


buildZ :: forall v s a. (IsBuilder v s a)
    => Coeffs v -> Builder v s a -> Builder v s a
buildZ xs acc =
    singleton 0 1 0 0 negXs obj obj
        <> acc { colNames = vars <> colNames acc }
  where
    negXs = SVec.map negate xs
    vars  = Vec.toList $ Vec.generate @v (Decision . fromIntegral)
    obj   = Objective PhaseII


buildObjectives :: (IsBuilder v s a)
    => Coeffs v -> Builder v s a -> Builder v s a
buildObjectives xs = buildW . buildZ xs


buildLEQ :: (IsBuilder v s a)
    => Coeffs v -> Double -> Builder v s a -> Builder v s a
buildLEQ xs y acc =
    singleton 0 0 a y xs n n <> acc
  where
    a = if y < 0 then -1 else 0
    n = nextSlack acc


buildGEQ :: (IsBuilder v s a)
    => Coeffs v -> Double -> Builder v s a -> Builder v s a
buildGEQ xs y acc =
    singleton 0 0 a y xs n n <> acc
  where
    a = if y > 0 then 1 else 0
    n = nextSlack acc


buildEQU :: (IsBuilder v s a)
    => Coeffs v -> Double -> Builder v s a -> Builder v s a
buildEQU xs y acc =
    singleton 0 0 0 y xs n n <> acc
  where
    n = nextArtificial acc


buildConstraint :: (IsBuilder v s a)
    => Constraint v s' a' -> Builder v s a -> Builder v s a
buildConstraint c =
    case c of
        LEQ xs y -> buildLEQ xs y
        GEQ xs y -> buildGEQ xs y
        EQU xs y -> buildEQU xs y


mkBuilder :: forall v s a. (IsBuilder v s a)
    => Problem v s a -> Builder v s a
mkBuilder = go (Builder [] [] [] [] [] [] [Artificial ix, RHS])
  where
    ix = fromIntegral . natVal $ Proxy @a

    go :: Builder v s a -> Problem v s' a' -> Builder v s a
    go acc (Maximise xs)    = buildObjectives xs acc
    go acc (SuchThat p c)   = go (buildConstraint c acc) p


adjustWRow :: [VarName] -> Matrix Double -> Matrix Double
adjustWRow ns mat =
    LA.asRow newRow === LA.dropRows 1 mat
  where
    newRow  = List.foldl' (SV.zipWith (-)) (mat!0) equRows
    equRows = (mat !) <$> List.findIndices isArtificial ns


toMatrix
    :: forall v s a rows cols.
        ( IsBuilder v s a
        , KnownNat (s + a)
        , KnownNat (((2 + v) + (s + a)) + 2)
        , KnownNat rows
        , KnownNat cols
        , rows ~ Rows 'PhaseI s a
        , cols ~ Cols 'PhaseI v s a
        )
    => Builder v s a
    -> Maybe (L rows cols)
toMatrix Builder{..} = do
    slackRows <- mapM (SVec.toSized @(s + a)) $ LA.toRows slackMat
    tableRows <- mapM (LS.exactLength @cols . LS.vecR) $ List.zipWith6
        toTableRow objectiveP1 objectiveP2 coeffs slackRows artificial rhs

    table     <- fmap LS.rowsL $ Vec.fromListN @rows tableRows

    LS.create . adjustWRow rowNames $ LS.unwrap table
  where
    numSlack      = fromIntegral . natVal $ Proxy @s
    numArtificial = fromIntegral . natVal $ Proxy @a

    slackMat      = LA.fromBlocks
        [ [LA.konst 0 (1, numSlack) ||| LA.konst 1 (1, numArtificial)]
        , [0]
        , [LA.ident (numSlack + numArtificial)]
        ]

    toTableRow w z cs ss a r  =
        SVec.fromTuple (w, z) ++ cs ++ ss ++ SVec.fromTuple (a, r)


toVarMap
    ::  ( IsBuilder v s a
        , IsVarMap rows cols
        , rows ~ Rows 'PhaseI s a
        , cols ~ Cols 'PhaseI v s a
        )
    => Builder v s a
    -> Maybe (VarMap 'PhaseI rows cols)
toVarMap Builder{..} =
    VarMap <$> Vec.fromList rowNames <*> Vec.fromList colNames

