{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.List where

import GHC.TypeLits

-- | Get a value on 2d matrix
--
-- O(n^2)
type family AtIx2 (adj :: [[a]]) (coord :: (Nat, Nat)) :: a where
  AtIx2 (row ': rows) '(0, y) = AtIx row y
  AtIx2 (row ': rows) '(x, y) = AtIx2 rows '(x - 1, y)

-- | Get a value on 1d matrix
--
-- O(n)
type family AtIx (row :: [a]) (x :: Nat) :: a where
  AtIx (v ': vx) 0 = v
  AtIx (v ': vx) x = vx `AtIx` (x - 1)

-- | Concat two lists
--
-- O(n)
type family Concat (l1 :: [a]) (l2 :: [a]) :: [a] where
  Concat '[] l2 = l2
  Concat (x ': xs) l2 = x ': (Concat xs l2)

type (:++:) l1 l2 = Concat l1 l2

-- | Update a value on a list
--
-- O(n) (because we go until zero to find that indexed value)
type family UpdateIx (l :: [a]) (ix :: Nat) (val :: a) :: [a] where
  UpdateIx (x ': xs) 0 val = val ': xs
  UpdateIx (x ': xs) n val = x ': UpdateIx xs (n - 1) val

type family Replicate (val :: a) (amount :: Nat) :: [a] where
  Replicate _ 0 = '[]
  Replicate v n = v ': Replicate v (n - 1)
