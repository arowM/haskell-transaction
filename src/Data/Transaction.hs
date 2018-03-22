{-# LANGUAGE DeriveFunctor #-}

{- |
Module      :  Data.Transaction

Copyright   :  Kadzuya Okamoto 2018
License     :  MIT

Stability   :  experimental
Portability :  unknown

Monadic representation of transactions.
-}
module Data.Transaction
  (
  -- * Constructors
  action

  -- * Converters
  , reduce
  , toList
  , tMap
  , tFilter
  , tFilterMap

  -- * Types
  , Transaction
  , TransactionM
  ) where

{- ==============
 -     Types
 - ============== -}

data TransactionM a x
  = TVal a (TransactionM a x)
  | TNull x
  deriving (Functor)

type Transaction a = TransactionM a ()

instance Applicative (TransactionM a) where
  pure = TNull
  TVal a next <*> f = TVal a (next <*> f)
  TNull g <*> f = f >>= (pure . g)

instance Monad (TransactionM a) where
  return = pure
  TVal a next >>= f = TVal a (next >>= f)
  TNull a >>= f = f a

{- ==============
 -   Operators
 - ============== -}

{- |
>>> :{
toList $ do
  action 4
  action 5
  action 6
:}
[4,5,6]
-}
action :: a -> Transaction a
action a = TVal a $ pure ()

{- ==============
 -   Converters
 - ============== -}

{- |
>>> :{
toList $ do
  action 4
  tMap (+1) $ do
    action 5
    action 6
  action 7
:}
[4,6,7,7]
-}
tMap :: (a -> b) -> Transaction a -> Transaction b
tMap f = tFilterMap (pure . f)

{- |
>>> :{
toList $ do
  action 4
  tFilter even $ do
    action 5
    action 6
  action 7
:}
[4,6,7]
-}
tFilter :: (a -> Bool) -> Transaction a -> Transaction a
tFilter p = tFilterMap $ \a ->
  if p a
    then Just a
    else Nothing

{- |
>>> :{
toList $ do
  action 4
  tFilterMap (\x -> if even x then Just (x + 1) else Nothing) $ do
    action 5
    action 6
  action 7
:}
[4,7,7]
-}
tFilterMap :: (a -> Maybe b) -> Transaction a -> Transaction b
tFilterMap f (TVal a next) =
  case f a of
    Just b ->
      TVal b $ tFilterMap f next
    Nothing ->
      tFilterMap f next
tFilterMap _ (TNull ()) = TNull ()

reduce :: (b -> a -> b) -> b -> Transaction a -> b
reduce f b (TVal a next) = reduce f (f b a) next
reduce _ b (TNull ()) = b

toList :: Transaction a -> [a]
toList trans = reduce (\f a -> f . (a:)) id trans []
