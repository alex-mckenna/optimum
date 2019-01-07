# Optimium

Optimum is a numeric optimization library for Haskell, which allows linear
optimization problems to be expressed in a simple and elegant manner. The
library aims to provide a strongly typed representation of numeric optimization
which is idiomatic to use and easy to extend.

## Solvers

A solver is a strategy for solving a problem. To solve a problem, only three
things are needed:

  * A function to check if the current state is optimal
  * A function to extract a result from the current state
  * A function to attempt to step to the next state

The library includes a simple implementation of the two phase simplex method,
using the statically sized interface from `hmatrix`. It is possible to define
other solvers to use this library, such as

  * A FFI wrapper to a well established solver
  * A fancy parallel solver using accelerate or repa
  * Your favourite linear optimization method written in Haskell

## Limitations

Currently there are some limitations in the library. These are either conscious
choices to make implementation simpler, or limitations from libraries used /
Haskell as a language. These are:

  * Defining problems requires a dependency on `vector-sized`. Without
      sacrificing type safety, this could be amended by making sized vectors
      an instance of `IsList` (if possible)

  * All problems are currently expressed as maximisations. This is to prevent
      the implementation of the two phase simplex from becoming any more
      complicated than it needs to be. Minimisation problems can still be
      expressed by multiplying every coefficient in the objective function by
      -1.

## Example

```haskell
{-# LANGUAGE DataKinds #-}

import qualified Data.Vector.Storable.Sized as Vec
import           Numeric.Optimization
import           Numeric.Optimization.TwoPhase


-- This is a representation of the following problem:
--
-- Maximise   2x1 + 3x2 + 4x3
-- Such That  3x1 + 2x2 +  x3 <= 10
--            2x1 + 5x2 + 3x3 <= 15
--             x1 ,  x2 ,  x3 >= 0
--
-- The Problem type is indexed by the number of decision variables, slack
-- variables (introduced by LEQ or GEQ constraints) and artificial slack
-- variables (introduced by EQU constraints).
--
problem :: Problem 3 2 0
problem = Maximise (Vec.fromTuple (2, 3, 4))
    `SuchThat` LEQ (Vec.fromTuple (3, 2, 1)) 10
    `SuchThat` LEQ (Vec.fromTuple (2, 5, 3)) 15


-- Here is how we solve the problem:
solution :: Either TwoPhaseError (TwoPhaseResult 3)
solution = solveWith twoPhase problem
```

