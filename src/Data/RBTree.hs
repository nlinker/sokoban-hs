{-# LANGUAGE DeriveGeneric #-}

module Data.RBTree where

import Control.DeepSeq (NFData, force)
import Debug.Trace
import GHC.Generics    (Generic)

data Color
  = R
  | B
  deriving (Show, Eq, Generic)

data Tree a
  = E
  | T Color (Tree a) a (Tree a)
  deriving (Show, Eq, Generic)

instance NFData Color

instance (NFData a) => NFData (Tree a)

insert :: (NFData a, Show a, Ord a) => a -> Tree a -> Tree a
insert x t =
  let T _ a y b = ins 0 t
  in T B (force a) y (force b)
  where
    ins i u
      | trace (indent i ++ "[ins " ++ show x ++ " " ++ desc u ++ "]") False = undefined
    ins _ E = T R E x E
    ins i s@(T col a y b)
      | x < y =
        let bt = balance (i + 1) col (ins (i + 1) a) y b
        in trace (indent (i + 1) ++ "   => " ++ desc bt) bt
      | x > y =
        let bt = balance (i + 1) col a y (ins (i + 1) b)
        in trace (indent (i + 1) ++ "   => " ++ desc bt) bt
      | otherwise = s

desc :: Show a => Tree a -> String
desc E = "E"
desc t = "(" ++ show t ++ ")"

indent :: Int -> String
indent i = concat $ replicate i "  "

balance :: Show a => Int -> Color -> Tree a -> a -> Tree a -> Tree a
balance i c t1 x t2
  | trace
     (indent i ++ "[balance " ++ show c ++ " " ++ desc t1 ++ " " ++ show x ++ " " ++ desc t2 ++ "]")
     False = undefined
balance _ B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance _ B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance _ B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance _ B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance _ col a x b = T col a x b
