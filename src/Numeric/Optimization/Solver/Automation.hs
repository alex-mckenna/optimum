{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}

module Numeric.Optimization.Solver.Automation where

import Prelude hiding                           (id, (.))

import Control.Arrow
import Control.Category

import Numeric.Optimization.Solver.Class


-- Solving a linear programming problem with a given method is performed by
-- running an automaton that will be defined later. The type of automata is
-- a function from the current state to a potential next result and next state.
--
newtype Optimization e a b = Optimization
    { runOptimization :: a -> (Either e b, Optimization e a b) }


instance Show (Optimization e a b) where
    show = const "<solver>"


instance Functor (Optimization e a) where
    fmap f a = Optimization $ \x ->
        let (y, a') = runOptimization a x
        in  (fmap f y, fmap f a')


instance Category (Optimization e) where
    id      = Optimization $ \x -> (Right x, id)

    g . f   = Optimization $ \x ->
        let (y, f') = runOptimization f x
            (z, g') = either (\e -> (Left e, g)) (runOptimization g) y
        in  (z, g' . f')


instance Arrow (Optimization e) where
    arr f   = Optimization $ \x -> (Right (f x), arr f)

    first a = Optimization $ \(x, z) ->
        let (y, a') = runOptimization a x
        in  (fmap (, z) y, first a')


instance ArrowChoice (Optimization e) where
    left a  = Optimization $ \x ->
        case x of
            Left e  ->
                let (e', a') = runOptimization a e
                in  (fmap Left e', left a')

            Right s -> (Right (Right s), left a)


instance ArrowLoop (Optimization e) where
    loop a  = Optimization $ \x ->
        case getResult x of
            Left e        -> (Left e, loop a)
            Right (s, a') -> (Right s, loop a')
      where
        getResult x = do
            rec let (f, a') = runOptimization a (x, feedback)
                (y, feedback) <- f

            pure (y, a')


advance :: (Solver method) => Optimization (Stop method) method method
advance = Optimization $ \x -> (step x, advance)


advance' :: (Solver method) => Optimization (Stop method) method (Vars method)
advance' = proc state -> do
    rec
        let output  = if done then current else toResult next
        let done    = isOptimal state
        let current = toResult state

        next <- advance -< state

    returnA -< output


solver :: forall method. (Solver method)
    => Optimization (Stop method) method (Vars method)
solver = loop f >>> arr toResult
  where
    f :: Optimization (Stop method) (method, method) (method, method)
    f = undefined

