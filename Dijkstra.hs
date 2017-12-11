{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}

module Dijkstra where

import GHC.TypeLits
import Data.Proxy
import Types.List
import Types.Utils
import Prelude (Bool (..))

type Adj =
  '[ '[ 0, 10, 2, 5 ]
   , '[ Inf, 0, Inf, Inf ]
   , '[ Inf, 7, 0, Inf ]
   , '[ Inf, 1, Inf, 0 ]
   ]

type family Length (x :: [a]) :: Nat where
  Length '[] = 0
  Length (x ': xs) = 1 + Length xs

type VerticeCount = Length Adj

type Visited = Replicate 'False VerticeCount

type Inf = 1000
type Dists = 0 ': (Replicate Inf (VerticeCount - 1))

-- | Determine whether node 'node' is visited
type family IsVisited (visited :: [Bool]) (node :: Nat) :: Bool where
  IsVisited visited n = AtIx visited n

-- | Get the neighbours of node 'node'
type family Neighbours (node :: Nat) :: [Nat] where
  Neighbours n = Neighbours' 0 (Adj `AtIx` n)

type family Neighbours' (ix :: Nat) (xs :: [a]) :: [Nat] where
  Neighbours' _ '[] = '[]
  Neighbours' ix (x ': xs) = IfElse (Eq 0 x :||: Eq Inf x) (Neighbours' (ix + 1) xs) (ix ': Neighbours' (ix + 1) xs)

-- | Given distance data 'dists', Update distance of node 'source' according to its neighbours 'neighbours'
type family UpdateDist (source :: Nat) (neighbours :: [Nat]) (dists :: [Nat]) :: [Nat] where
  UpdateDist s '[] dists = dists
  UpdateDist s (n ': nx) dists =
    UpdateDist s nx (
      IfElse ((AtIx2 Adj '(s, n)) <=? (dists `AtIx` n))
        (UpdateIx dists n (dists `AtIx` s + AtIx2 Adj '(s, n)))
        dists
    )

-- | Set Node 's' visited
type family SetVisited (s :: Nat) (visited :: [Bool]) :: [Bool] where
  SetVisited s visited = UpdateIx visited s 'True

type family Solve (stack :: [Nat]) (dists :: [Nat]) (visited :: [Bool]) :: [Nat] where
  Solve '[] dists _ = dists
  Solve (s ': sx) dists visited =
    IfElse (IsVisited visited s)
      (Solve sx dists visited)
      (Solve
        ((Neighbours s) :++: sx)
        (UpdateDist s (Neighbours s) dists)
        (SetVisited s visited)
      )

type Solution = Solve '[0] Dists Visited

test_result_1 :: (Solution ~ x, x ~ '[0, 6, 2, 5]) => Proxy x
test_result_1 = Proxy
