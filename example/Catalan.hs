import Math.OEIS
import Data.List (genericLength)

-- data-less binary trees.
data BTree = Empty | Fork BTree BTree  deriving Show

-- A list of all the binary trees with exactly n nodes.
listTrees :: Int -> [BTree]
listTrees 0 = [Empty]
listTrees n = [Fork left right | 
               k <- [0..n-1],
               left <- listTrees k,
               right <- listTrees (n-1-k) ]

-- HORRIBLY INEFFICIENT!!
countTrees :: Int -> Integer
countTrees = genericLength . listTrees

-- This is about the best we can do.
smallTreeCounts = map countTrees [0..10]

-- so... cheat!  Extend the sequence via the OEIS.
treeCounts = extendSequence smallTreeCounts

countTrees' :: Int -> Integer
countTrees' n = treeCounts !! n