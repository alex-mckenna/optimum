{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Numeric.LinearProgramming.Tableau.Builder
    ( mkBuilder
    , toMatrix
    , toVarMap
    ) where

import qualified Data.List as List
import           Data.Proxy
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Sized as Vec
import qualified Data.Vector.Storable.Sized as SVec
import           GHC.TypeLits
import           Numeric.LinearAlgebra as LA hiding         ((<>))
import           Numeric.LinearAlgebra.Static               (L)
import qualified Numeric.LinearAlgebra.Static as LS         (create, unwrap)
import qualified Numeric.LinearAlgebra.Static.Vector as LS

import           Numeric.LinearProgramming.Problem
import           Numeric.LinearProgramming.Tableau.Phase
import           Numeric.LinearProgramming.Tableau.VarMap


type IsBuilder v s a =
    (KnownNat v, KnownNat s, KnownNat a)


data Builder (v :: Nat) (s :: Nat) (a :: Nat) = Builder
    { objectiveP1   :: [Double]
    , objectiveP2   :: [Double]
    , artificial    :: [Double]
    , rhs           :: [Double]
    , coeffs        :: [Coeffs v]
    , rowVars       :: [VarName]
    , colVars       :: [VarName]
    } deriving (Eq, Show)

instance Semigroup (Builder v s a) where
    Builder wx zx ax rx cx rvx cvx <> Builder wy zy ay ry cy rvy cvy = Builder
        { objectiveP1 = wx  <> wy
        , objectiveP2 = zx  <> zy
        , artificial  = ax  <> ay
        , rhs         = rx  <> ry
        , coeffs      = cx  <> cy
        , rowVars     = rvx <> rvy
        , colVars     = cvx <> cvy
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
    case List.find isSlack (colVars builder) of
        Just (Slack i)  -> Slack (i - 1)
        _               -> Slack (numSlack - 1)
  where
    numSlack = fromIntegral . natVal $ Proxy @s

    isSlack (Slack _)   = True
    isSlack _           = False


nextArtificial :: Builder v s a -> VarName
nextArtificial builder =
    case List.find isArtificial (colVars builder) of
        Just (Artificial i) -> Artificial (i - 1)
        _                   -> error "nextArtificial: No artificial variables"
  where
    isArtificial (Artificial _) = True
    isArtificial _              = False


buildW :: (IsBuilder v s a)
    => Builder v s a -> Builder v s a
buildW = (singleton 1 0 1 0 xs ObjectiveP1 ObjectiveP1 <>)
  where
    xs = SVec.replicate 0


buildZ :: forall v s a. (IsBuilder v s a)
    => Coeffs v -> Builder v s a -> Builder v s a
buildZ xs acc =
    singleton 0 1 0 0 negXs ObjectiveP2 ObjectiveP2
        <> acc { colVars = vars <> colVars acc }
  where
    negXs = SVec.map negate xs
    vars  = Vec.toList $ Vec.generate @v (Decision . fromIntegral)


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
adjustWRow rVars mat =
    LA.asRow newRow === LA.dropRows 1 mat
  where
    newRow  = List.foldl' (SV.zipWith (-)) (mat!0) equRows
    equRows = (mat !) <$> List.findIndices isArtificial rVars

    isArtificial (Artificial _) = True
    isArtificial _              = False


toMatrix
    :: forall v s a rows cols.
        ( IsBuilder v s a
        , KnownNat rows
        , KnownNat cols
        , rows ~ Rows 'PhaseI s a
        , cols ~ Cols 'PhaseI v s a
        )
    => Builder v s a -> Maybe (L rows cols)
toMatrix builder = do
    coeff <- fmap toDynMatrix . Vec.fromListN @rows $ coeffs builder
    let mat = w ||| z ||| coeff ||| slack ||| a ||| y

    LS.create $ adjustWRow (rowVars builder) mat
  where
    toDynMatrix   = LS.unwrap . LS.rowsL . fmap LS.vecR

    numSlack      = fromIntegral . natVal $ Proxy @s
    numArtificial = fromIntegral . natVal $ Proxy @a
    w             = LA.matrix 1 $ objectiveP1 builder
    z             = LA.matrix 1 $ objectiveP2 builder
    a             = LA.matrix 1 $ artificial builder
    y             = LA.matrix 1 $ rhs builder

    slack         = LA.konst 0 (1, numSlack) ||| LA.konst 1 (1, numArtificial)
        === LA.konst 0 (1, numSlack + numArtificial)
        === LA.ident (numSlack + numArtificial)


toVarMap
    ::  ( IsBuilder v s a
        , IsVarMap rows cols
        , rows ~ Rows 'PhaseI s a
        , cols ~ Cols 'PhaseI v s a
        )
    => Builder v s a
    -> Maybe (VarMap rows cols)
toVarMap builder = do
    rows <- Vec.fromList $ rowVars builder
    cols <- Vec.fromList $ colVars builder

    pure $ VarMap rows cols

