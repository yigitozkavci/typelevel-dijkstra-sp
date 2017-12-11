{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Utils where

import Prelude (Bool (..))
import GHC.TypeLits

type family IfElse (pred :: Bool) (left :: a) (right :: a) :: a where
  IfElse 'True left _ = left
  IfElse 'False _ right = right

type family Eq left right :: Bool where
  Eq a a = 'True
  Eq _ _ = 'False

type family And (left :: Bool) (right :: Bool) :: Bool where
  'True `And` 'True = 'True
  _ `And` _ = 'False

type (:&&:) l r = And l r

type family Or (left :: Bool) (right :: Bool) :: Bool where
  'False `Or` 'False = 'False
  _ `Or` _ = 'True

type (:||:) l r = Or l r
