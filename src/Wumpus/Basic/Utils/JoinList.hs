{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Utils.JoinList
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- A \"join list\" datatype and operations. 
--
-- A join list is implemented a binary tree, so joining two 
-- lists (catenation, aka (++)) is a cheap operation. 
--
-- This constrasts with the regular list datatype which is a 
-- cons list: while consing on a regular list is by nature cheap, 
-- joining (++) is expensive. 
--
--------------------------------------------------------------------------------


module Wumpus.Basic.Utils.JoinList 
  ( 
  -- * Join list datatype, opaque.
    JoinList

  -- * Left view as per Data.Sequence  
  , ViewL(..)
  , ViewR(..)

  -- * Conversion between join lists and regular lists
  , fromList
  , fromListF
  , toList
  , toListF
  , toListM
  , zipWithIntoList

  -- * Construction
  , empty
  , one
  , cons
  , snoc
  , join

  -- * Basic functions  
  , head
  , takeL
  , length

  , takeWhileL
  , accumMapL
  , null


  -- * Views
  , viewl
  , viewr
  , unViewL
  , unViewR
  
  ) where


import Control.Applicative hiding ( empty )
import Control.Monad hiding ( join )

import Data.Foldable ( Foldable )
import qualified Data.Foldable as F
import Data.Monoid
import Data.Traversable ( Traversable(..) )

import Prelude hiding ( head, take, length, mapM, null )

data JoinList a = Empty 
                | One a 
                | Join (JoinList a) (JoinList a)
  deriving (Eq)

data ViewL a = EmptyL | a :< (JoinList a)
  deriving (Eq,Show)

data ViewR a = EmptyR | (JoinList a) :> a
  deriving (Eq,Show)

--------------------------------------------------------------------------------

instance Show a => Show (JoinList a) where
  showsPrec _ xs = showString "fromList " . shows (toList xs) 


instance Monoid (JoinList a) where
  mempty        = Empty
  mappend       = join

instance Functor JoinList where
  fmap _ Empty      = Empty
  fmap f (One a)    = One (f a)
  fmap f (Join t u) = Join (fmap f t) (fmap f u)



instance Foldable JoinList where
  foldMap _ Empty      = mempty
  foldMap f (One a)    = f a
  foldMap f (Join t u) = F.foldMap f t `mappend` F.foldMap f u

  foldr                = joinfoldr
  foldl                = joinfoldl

instance Traversable JoinList where
  traverse _ Empty      = pure Empty
  traverse f (One a)    = One <$> f a
  traverse f (Join t u) = Join <$> traverse f t <*> traverse f u

  mapM mf               = step . viewl
    where
      step EmptyL    = return Empty
      step (x :< xs) = liftM2 cons (mf x) (step $ viewl xs)


-- Views

instance Functor ViewL where
  fmap _ EmptyL         = EmptyL
  fmap f (a :< as)      = f a :< fmap f as

instance Functor ViewR where
  fmap _ EmptyR         = EmptyR
  fmap f (as :> a)      = fmap f as :> f a


--------------------------------------------------------------------------------
-- Conversion

-- | Convert a join list to a regular list.
--
toList :: JoinList a -> [a]
toList = joinfoldr (:) []



-- | Build a join list from a regular list.
--
-- This builds a tall skinny list.
--
-- WARNING - throws an error on empty list.
--

fromList :: [a] -> JoinList a
fromList []     = Empty
fromList [x]    = One x
fromList (x:xs) = Join (One x) (fromList xs)

fromListF :: (a -> b) -> [a] -> JoinList b
fromListF f = step 
  where
    step []     = Empty
    step [x]    = One (f x)
    step (x:xs) = Join (One $ f x) (step xs)


toListF :: (a -> b) -> JoinList a -> [b]
toListF f = ($ []) . step 
  where
    step Empty       = id
    step (One x)     = (\ls -> f x : ls)
    step (Join t u)  = step t . step u


toListM :: Monad m => (a -> m b) -> JoinList a -> m [b]
toListM mf = liftM ($ []) . step 
  where
    step Empty       = return id
    step (One x)     = mf x >>= \a -> return (\ls -> a : ls)
    step (Join t u)  = step t >>= \f -> step u >>= \g  -> return (f . g)


zipWithIntoList :: (a -> b -> c) -> JoinList a -> [b] -> [c]
zipWithIntoList f jl xs0 = step (viewl jl) xs0
  where
    step EmptyL    _      = []
    step _         []     = []
    step (a :< as) (x:xs) = f a x : step (viewl as) xs

--------------------------------------------------------------------------------


null :: JoinList a -> Bool
null Empty       = True
null _           = False




-- | Create an empty join list.
--
empty :: JoinList a
empty = Empty



-- | Create a singleton join list.
--
one :: a -> JoinList a
one = One



infixr 5 `cons`

-- | Cons an element to the front of the join list.
--
cons :: a -> JoinList a -> JoinList a
cons a xs = Join (One a) xs  

-- | Snoc an element to the tail of the join list.
--
snoc :: JoinList a -> a -> JoinList a
snoc xs a = Join xs (One a)




infixr 5 `join`

--
join :: JoinList a -> JoinList a -> JoinList a
join Empty b     = b
join a     Empty = a 
join a     b     = Join a b


--------------------------------------------------------------------------------
-- Basic functions

-- | Extract the first element of a join list - i.e. the leftmost
-- element of the left spine. An error is thrown if the list is 
-- empty. 
-- 
-- This function performs a traversal down the left spine, so 
-- unlike @head@ on regular lists this function is not performed 
-- in constant time.
--
-- This function throws a runtime error on the empty list.
-- 
head :: JoinList a -> a
head Empty      = error "JoinList - head called on empty list"
head (One a)    = a
head (Join t _) = head t


takeL :: Int -> JoinList a -> JoinList a
takeL i xs | i < 1     = Empty
           | otherwise = case viewl xs of
                           a :< rest -> cons a $ takeL (i-1) rest
                           EmptyL    -> Empty     

length :: JoinList a -> Int
length = joinfoldr (\_ n -> n+1) 0 



takeWhileL :: (a -> Bool) -> JoinList a -> JoinList a
takeWhileL test = step . viewl
  where
    step EmptyL                 = Empty
    step (x :< xs)  | test x    = x `cons` step (viewl xs)
                    | otherwise = Empty  


accumMapL :: (x -> st -> (y,st)) -> JoinList x -> st -> (JoinList y,st)
accumMapL f xs st0 = go xs st0 
  where
    go Empty       st = (Empty,st)
    go (One x)     st = let (y,st') = f x st in (One y,st')
    go (Join t u)  st = (Join v w, st'')
                             where (v,st')  = go t st
                                   (w,st'') = go u st'




-- | Right-associative fold of a JoinList.
--
joinfoldr :: (a -> b -> b) -> b -> JoinList a -> b
joinfoldr f = go
  where
    go e Empty      = e
    go e (One a)    = f a e
    go e (Join t u) = go (go e u) t


-- | Left-associative fold of a JoinList.
--
joinfoldl :: (b -> a -> b) -> b -> JoinList a -> b
joinfoldl f = go 
  where
    go e Empty      = e
    go e (One a)    = f e a
    go e (Join t u) = go (go e t) u

--------------------------------------------------------------------------------
-- Views

-- | Access the left end of a sequence.
--
-- Unlike the corresponing operation on Data.Sequence this is 
-- not a cheap operation, the joinlist must be traversed down 
-- the left spine to find the leftmost node.
--
-- Also the traversal may involve changing the shape of the 
-- underlying binary tree.
--
viewl :: JoinList a -> ViewL a
viewl Empty      = EmptyL
viewl (One a)    = a :< Empty
viewl (Join t u) = step t u
  where
    step Empty        r = viewl r
    step (One a)      r = a :< r
    step (Join t' u') r = step t' (Join u' r)

-- | Access the right end of a sequence.
--
-- Unlike the corresponing operation on Data.Sequence this is 
-- not a cheap operation, the joinlist must be traversed down 
-- the left spine to find the leftmost node.
--
-- Also the traversal may involve changing the shape of the 
-- underlying binary tree.
--
viewr :: JoinList a -> ViewR a
viewr Empty      = EmptyR
viewr (One a)    = Empty :> a
viewr (Join t u) = step t u
  where
    step l Empty        = viewr l
    step l (One a)      = l :> a
    step l (Join t' u') = step (Join l t') u'


unViewL :: ViewL a -> JoinList a
unViewL EmptyL    = Empty
unViewL (x :< xs) = cons x xs

unViewR :: ViewR a -> JoinList a
unViewR EmptyR    = Empty
unViewR (xs :> x) = snoc xs x

