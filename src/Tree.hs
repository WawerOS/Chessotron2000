module Tree (
  Tree(Leaf,Branch),
  getBottom,
  depth,
  getVal,
  getSubTree,
  getAllSubTrees,
  getValAt,
  findMax,
  chooser,
  maxmin,
  alphaBeta,
  applyAtEnds,
  applyNTimes
) where
 import Data.Monoid
 import Test.QuickCheck

{-
  Provides the data structures and funnctions meant to deal with tree's

  Tasks:
  - Extending Tree typeclasses, to traversable
-}

-- A data structure meant to represent all possible choices from a event
 data Tree a = Leaf a | Branch [Tree a]
    deriving Show

 instance Arbitrary a => Arbitrary (Tree a) where
   arbitrary = sized sizedArbitrary

 sizedArbitrary :: Arbitrary a => Int -> Gen(Tree a)
 sizedArbitrary m = do
   ls <- vectorOf (m `div` 2) (sizedArbitrary (m `div` 2))
   chk <- choose (1,2) :: Gen Integer
   val <- arbitrary
   return (cusMap val ls chk)

  where
     cusMap val ls chk = if chk == 1 then Branch ls else Leaf val
 instance Functor Tree where
  fmap f (Branch y) = Branch (map (fmap f) y)
  fmap f (Leaf x) = Leaf (f x)

 instance Foldable Tree where
  foldMap f (Leaf a) = f a
  foldMap f (Branch br) = foldMap (foldMap f) br

 getAllSubTrees :: Tree a -> [Tree a]
 getAllSubTrees (Leaf _) = []
 getAllSubTrees (Branch trs) = trs

 --  Given a path to take retrieves subtree's
 getSubTree :: Tree a -> [Int] -> Maybe (Tree a)
 getSubTree tr [] = Just tr
 getSubTree (Leaf _) xs | not (null xs) = Nothing
 getSubTree (Branch subTr) (x:_) | x > length subTr = Nothing
 getSubTree (Branch subTr) (x:xs) = getSubTree (subTr !! x) xs

 -- Given a tree returns top value
 getVal :: Tree a -> Maybe a
 getVal (Leaf a) = Just a
 getVal (Branch _) = Nothing

 -- Combines getVal and getSubTree to return a specific value
 getValAt :: Tree a -> [Int] -> Maybe a
 getValAt tr xs = getVal <$> getSubTree tr xs


 -- Gives the index of the greatest element
 findMax :: Ord a => [a] -> Maybe Int
 findMax [] = Nothing
 findMax (y:ys) = Just $ helper 0 y 1 ys
   where
    helper i _ _ [] = i
    helper i x j (z:zs) | x >= z = helper i x (j+1) zs
    helper _ x j (z:zs) | z > x = helper j z (j+1) zs
    helper _ _ _ _ = 0

 (<==>) :: Eq a => Maybe a -> a -> Bool
 Nothing <==> _ = False
 (Just a) <==> a' = a == a'

 prop_findMax :: Ord a => [a] -> Bool
 prop_findMax [] = True
 prop_findMax ls = fmap (ls !!) (findMax ls) <==> maximum ls

 chooser :: Ord b => (Tree a -> b) -> Tree a -> Maybe Int
 chooser f (Branch cly) = findMax $ map f cly
 chooser _ _ = Nothing

 alphaBeta :: Ord a => a -> a ->  Bool -> Tree a -> a
 alphaBeta  _ _ _ (Leaf a) = a
 alphaBeta  a b True (Branch ls) = maxLeq a b ls
 alphaBeta  a b False (Branch ls) = minLeq a b ls

 minLeq :: Ord a => a -> a -> [Tree a] -> a
 minLeq _ b [] = b
 minLeq a b _ | a >= b = b
 minLeq a b (x:xs) | val x < b = maxLeq a (val x) xs
  where
   val = alphaBeta a b True
 minLeq a b (x:xs) | val x >= b = maxLeq a b xs
  where
    val = alphaBeta a b True

 maxLeq :: Ord a => a -> a -> [Tree a] -> a
 maxLeq a _ [] = a
 maxLeq a b _ | a > b = a
 maxLeq a b (x:xs) | val x >= a = maxLeq (val x) b xs
  where
   val = alphaBeta a b False
 maxLeq a b (x:xs) | val x < a = maxLeq a b xs
  where
    val = alphaBeta a b False

 -- Naive minimax implementation
 maxa :: ([a] -> a,[a] -> a) -> Tree a -> a
 maxa _ (Leaf a) = a
 maxa (f,g) (Branch y) = f (map (maxa (g,f)) y)

-- Gives value of branch of greatest value
 maxmin :: Ord a => Tree a -> a
 maxmin (Leaf a) = a
 maxmin t = maxa (maximum,minimum) t


-- A selective fmap
 applyAtEnds :: (a -> [b]) -> Tree a -> Tree b
 applyAtEnds f (Branch []) = Branch []
 applyAtEnds f (Branch subTr) = Branch (map (applyAtEnds f) subTr)
 applyAtEnds f (Leaf val) = Branch (map Leaf (f val))

 prop_applyAtEnds :: Tree a -> Bool
 prop_applyAtEnds tr = (depth tr + 1) == depth (applyAtEnds (replicate 2) tr)
 prop_applyNTimes :: (Integer,Tree a) -> Bool
 prop_applyNTimes (i,tr) = (depth tr + i) == depth (applyNTimes [replicate 2] tr i)


-- swaps two leaf generating functions
 applyNTimes :: [a -> [a]] -> Tree a -> Integer -> Tree a
 applyNTimes _ tr n | n == 0 = tr
 applyNTimes (f:xs) tr n =  applyNTimes (xs ++ [f]) (applyAtEnds f tr) (n-1)

 isBranch :: Tree a -> Bool
 isBranch (Branch _) = True
 isBranch (Leaf _) = False

 depth :: Tree a -> Integer
 depth (Branch []) = 1
 depth (Leaf _) = 1
 depth (Branch ts) = 1 + maximum (map depth ts)

 getBottom :: Tree a -> [a]
 getBottom (Branch tr) = concatMap getBottom tr
 getBottom (Leaf a) = [a]
