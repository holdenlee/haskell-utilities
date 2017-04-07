{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module Fors  where
import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.List
import Control.Monad.ST
import Data.STRef
import Data.Traversable

-- *Assignment Operators

(=.~) :: STRef s a -> a -> ST s ()
(=.~) = writeSTRef

(=%~) :: STRef s a -> (a -> a) -> ST s ()
(=%~) = modifySTRef'

-- Add a number to a reference
(+=) :: (Num a) => STRef s a -> a -> ST s ()
x += y = modifySTRef' x (+y)

(+~) :: (Num a) => STRef s a -> ST s ()
x +~ y = x += 1

-- Add a reference to another reference
(+=.) :: (Num a) => STRef s a -> STRef s a -> ST s ()
x +=. y' = do
    y <- readSTRef y'
    x += y

-- Add a number to a reference
(-=) :: (Num a) => STRef s a -> a -> ST s ()
x -= y = modifySTRef' x (\x -> x-y)

-- Add a reference to another reference
(-=.) :: (Num a) => STRef s a -> STRef s a -> ST s ()
x -=. y' = do
    y <- readSTRef y'
    x -= y

(-~) :: (Num a) => STRef s a -> ST s ()
x -~ y = x -= 1

-- Add a number to a reference
(*=) :: (Num a) => STRef s a -> a -> ST s ()
x *= y = modifySTRef' x (*y)

-- Add a reference to another reference
(*=.) :: (Num a) => STRef s a -> STRef s a -> ST s ()
x *=. y' = do
    y <- readSTRef y'
    x *= y

{-
The one annoyance is that you have to get the value out. 
For example if you have references x, y, z
you have to do x' <- x; y' <- y...
-} 

-- *Loops

whileM :: (Monad m) => m Bool -> m a -> a -> m a
whileM mb ma a = 
    do
      x <- ma
      b <- mb
      if b then ma >> (whileM mb ma a) else return a

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ mb ma = 
    do
      b <- mb
      if b then ma >> (whileM_ mb ma) else return ()

is :: STRef s a -> (a -> Bool) -> ST s Bool
is x f = fmap f $ readSTRef x

{- Example:
stWhile' :: ST s Int
stWhile' = 
  do
    s <- newSTRef 0
    x <- newSTRef 1
    whileM_ (x `is` (<=10)) $ do
            s +=. x
            x += 1
    readSTRef s
-}
