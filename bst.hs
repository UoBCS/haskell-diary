-- Binary trees
data BT a = Empty
          | Fork a (BT a) (BT a) deriving (Show, Read, Eq, Ord)

instance Functor BT where
         fmap f Empty = Empty
         fmap f (Fork x l r) = Fork (f x) (fmap f l) (fmap f r)

instance Monad BT where
         Empty      >>= k = Empty
         Fork n l r >>= k = Fork n (k l) (k r)
         return x         = Fork x Empty Empty
         

leaf :: a -> BT a
leaf x = Fork x Empty Empty

mirror :: BT a -> BT a
mirror Empty = error "Empty tree"
mirror (Fork x l r) = Fork x (mirror r) (mirror l)

height :: BT a -> Int
height Empty = -1
height (Fork _ l r) = 1 + max (height l) (height r)

flatten :: BT a -> [a]
flatten Empty = []
flatten (Fork x l r) = (flatten l) ++ [x] ++ (flatten r)

isBst :: Ord a => BT a -> Bool
isBst bt = isSorted (flatten bt)

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : xs) = x < y && isSorted (y : xs)

occurs :: Ord a => a -> BT a -> Bool
occurs _ Empty = False
occurs x (Fork y l r) = x == y || (x < y && occurs x l) || (x > y && occurs x r)

insert :: Ord a => a -> BT a -> BT a
insert x Empty = leaf x
insert x (Fork y l r) | x == y = Fork y l r
                      | x <  y = Fork y (insert x l) r
                      | x >  y = Fork y l (insert x r)
