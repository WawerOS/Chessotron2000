module Tree (
  Tree(Leaf),
  getVal,
  getSubTree,
  getValAt,
  maxmin,
  applyAtEnds,
  applyNTimes
) where

 data Tree a = Leaf a | Branch a [Tree a]
    deriving Show

 getSubTree :: Tree a -> [Int] -> Maybe (Tree a)
 getSubTree tr [] = Just tr
 getSubTree (Leaf _) xs | not $ null xs = Nothing
 getSubTree (Branch _ subTr) (x:_) | x > length subTr = Nothing
 getSubTree (Branch _ subTr) (x:xs) = getSubTree (subTr !! x) xs

 getVal :: Tree a -> a
 getVal (Leaf a) = a
 getVal (Branch a _) = a


 getValAt :: Tree a -> [Int] -> Maybe a
 getValAt tr xs = getSubTree tr xs >>= (Just . getVal)

 instance Functor Tree where
   fmap f (Branch x y) = Branch (f x) (map (fmap f) y)
   fmap f (Leaf x) = Leaf (f x)

 maxa :: ([a] -> a,[a] -> a) -> Tree a -> a
 maxa _ (Leaf a) = a
 maxa (f,g) (Branch _ y) = f (map (maxa (g,f)) y)


 findMax :: Ord a => [a] -> Maybe Int
 findMax [] = Nothing
 findMax (y:ys) = Just $ helper (0,y) 1 ys
   where
    helper (i,_) _ [] = i
    helper (i,x) j (z:zs) | x > z = helper (i,x) (j+1) zs
    helper (_,x) j (z:zs) | z > x = helper (j,z) (j+1) zs
    helper _ _ _ = 0

 maxmin :: Ord a => Tree a -> Maybe Int
 maxmin (Leaf _) = Nothing
 maxmin (Branch _ y) = findMax (map (maxa (maximum,minimum)) y)

 applyAtEnds :: (a -> b) -> (a -> [b]) -> Tree a -> Tree b
 applyAtEnds g f (Branch val subTr) = Branch (g val) (map (applyAtEnds g f) subTr)
 applyAtEnds g f (Leaf val) = Branch (g val) (map Leaf (f val))

 applyNTimes :: (a -> a) -> (a -> [a],a -> [a]) -> Tree a -> Integer -> Tree a
 applyNTimes g _ tr n | n < 0 = fmap g tr
 applyNTimes g (f,h) tr n =  applyNTimes g (h,f) (applyAtEnds g f tr) (n-1)
