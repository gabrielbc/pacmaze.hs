--
-- Copyright (c) 2009 - 2010 Brendan Hickey - http://bhickey.net
-- New BSD License (see http://www.opensource.org/licenses/bsd-license.php)
--

-- | 'Data.Heap.Binary' provides a binary min-heap. Balance is maintained through descendant counting.
module Heap 
(BinaryHeap, headH, tailH, merge, singleton, emptyH, nullH, fromList, toList, insertH) 
where

import Prelude hiding (headH, tailH, nullH)

data BinaryHeap n =
    Leaf
  | Node n !Int (BinaryHeap n) (BinaryHeap n) deriving (Eq, Ord)

instance (Ord n, Show n) => Show (BinaryHeap n) where
  show Leaf = "Leaf"
  show (Node n _ h1 h2) = "Node " ++ show n ++ " (" ++ show h1 ++ " " ++ show h2 ++ ")"

rank :: (Ord n) => BinaryHeap n -> Int
rank Leaf = 0
rank (Node _ d _ _) = d

-- | /O(1)/. 'emptyH' produces an emptyH heap.
emptyH :: (Ord a) => BinaryHeap a
emptyH = Leaf 

-- | /O(1)/. 'singleton' consumes an element and constructs a singleton heap.
singleton :: (Ord a) => a -> BinaryHeap a
singleton a = Node a 1 Leaf Leaf

-- | 'merge' consumes two binary heaps and merges them.
merge :: (Ord a) => BinaryHeap a -> BinaryHeap a -> BinaryHeap a
merge Leaf n = n
merge n Leaf = n
merge h1@(Node n1 d1 h1l h1r) h2@(Node n2 d2 _ _) = 
  if  n1<n2 || (n1==n2 && d1<=d2)
  then if rank h1l < rank h1r
       then Node n1 (d1 + d2) (merge h1l h2) h1r
       else Node n1 (d1 + d2) h1l (merge h1r h2)
  else merge h2 h1

-- | /O(lg n)/.
insertH :: (Ord a) => a -> BinaryHeap a -> BinaryHeap a
insertH a h = merge h (singleton a)

-- | /O(1)/.
nullH :: (Ord a) => BinaryHeap a -> Bool
nullH Leaf = True
nullH _    = False

-- | /O(n lg n)/.
toList :: (Ord a) => BinaryHeap a -> [a]
toList Leaf = []
toList h@(Node _ _ _ _) = headH h : toList (tailH h)

-- | /O(n)/. 'fromList' constructs a binary heap from an unsorted list.
fromList :: (Ord a) => [a] -> BinaryHeap a
fromList [] = Leaf
fromList l = mergeList (map singleton l)
              where mergeList [a] = a
                    mergeList x = mergeList (mergePairs x)
                    mergePairs (a:b:c) = merge a b : mergePairs c
                    mergePairs x = x

-- | /O(1)/. 'headH' returns the element root of the heap.
headH :: (Ord a) => BinaryHeap a -> a
headH Leaf = error "Data.Tree.Heap: emptyH list"
headH (Node n _ _ _) = n

-- | /O(lg n)/. 'tailH' discards the root of the heap and merges the subtrees.
tailH :: (Ord a) => BinaryHeap a -> BinaryHeap a
tailH Leaf = error "Data.Heap emptyH list"
tailH (Node _ _ h1 h2) = merge h1 h2