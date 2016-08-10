module Tree (
  Tree(Leaf),
  getVal,
  getSubTree,
  getValAt,
  chooser,
  maxmin,
  alphaBeta,
  applyAtEnds,
  applyNTimes
) where

{-
  Provides the data structures and funnctions meant to deal with tree's

  Tasks:
  - Completing alphaBeta
  - Extending Tree typeclasses, to traversable
-}

 -- A data structure meant to represent all possible choices from a event
 data Tree a = Leaf a | Branch a [Tree a]
    deriving Show

 instance Functor Tree where
  fmap f (Branch x y) = Branch (f x) (map (fmap f) y)
  fmap f (Leaf x) = Leaf (f x)

-- A pair of infinite Tree
 zeroTree :: Tree Integer
 zeroTree = Branch 0 [oneTree,zeroTree]

 oneTree :: Tree Integer
 oneTree = Branch 1 [oneTree,zeroTree]

 --  Given a path to take retrieves subtree's
 getSubTree :: Tree a -> [Int] -> Maybe (Tree a)
 getSubTree tr [] = Just tr
 getSubTree (Leaf _) xs | not $ null xs = Nothing
 getSubTree (Branch _ subTr) (x:_) | x > length subTr = Nothing
 getSubTree (Branch _ subTr) (x:xs) = getSubTree (subTr !! x) xs

 -- Given a tree returns top value
 getVal :: Tree a -> a
 getVal (Leaf a) = a
 getVal (Branch a _) = a

 -- Combines getVal and getSubTree to return a specific value
 getValAt :: Tree a -> [Int] -> Maybe a
 getValAt tr xs = getSubTree tr xs >>= (Just . getVal)


 -- Gives the index of the greatest element
 findMax :: Ord a => [a] -> Maybe Int
 findMax [] = Nothing
 findMax (y:ys) = Just $ helper (0,y) 1 ys
   where
    helper (i,_) _ [] = i
    helper (i,x) j (z:zs) | x > z = helper (i,x) (j+1) zs
    helper (_,x) j (z:zs) | z > x = helper (j,z) (j+1) zs
    helper _ _ _ = 0

 chooser :: Ord b => (Tree a -> b) -> Tree a -> Maybe Int
 chooser f (Branch _ y) = findMax $ map f y
 chooser _ _ = Nothing

 alphaBeta :: Ord a => a -> a ->  Bool -> Tree a -> a
 alphaBeta  _ _ _ (Leaf a) = a
 alphaBeta  a b True (Branch _ ls) = maxLeq a b ls
 alphaBeta  a b False (Branch _ ls) = maxLeq a b ls

 minLeq :: Ord a => a -> a -> [Tree a] -> a
 minLeq a _ [] = a
 minLeq a b _ | a >= b = a
 minLeq a b (x:xs) | val x < b = maxLeq a (val x) xs
  where
   val = alphaBeta a b True
 minLeq a b (x:xs) | val x >= b = maxLeq a b xs
  where
    val = alphaBeta a b True

 maxLeq :: Ord a => a -> a -> [Tree a] -> a
 maxLeq a _ [] = a
 maxLeq a b _ | a >= b = a
 maxLeq a b (x:xs) | val x > a = maxLeq (val x) b xs
  where
   val = alphaBeta a b False
 maxLeq a b (x:xs) | val x <= a = maxLeq a b xs
  where
    val = alphaBeta a b False




 -- Naive minimax implementation
 maxa :: ([a] -> a,[a] -> a) -> Tree a -> a
 maxa _ (Leaf a) = a
 maxa (f,g) (Branch _ y) = f (map (maxa (g,f)) y)

-- Gives index of branch of greatest value
 maxmin :: Ord a => Tree a -> a
 maxmin (Leaf a) = a
 maxmin t = maxa (maximum,minimum) t


-- A selective fmap
 applyAtEnds :: (a -> b) -> (a -> [b]) -> Tree a -> Tree b
 applyAtEnds g f (Branch val subTr) = Branch (g val) (map (applyAtEnds g f) subTr)
 applyAtEnds g f (Leaf val) = Branch (g val) (map Leaf (f val))


-- swaps two leaf generating functions
 applyNTimes :: (a -> a) -> (a -> [a],a -> [a]) -> Tree a -> Integer -> Tree a
 applyNTimes g _ tr n | n == 0 = fmap g tr
 applyNTimes g (f,h) tr n =  applyNTimes g (h,f) (applyAtEnds g f tr) (n-1)
