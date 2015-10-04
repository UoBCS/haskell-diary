

-- Doubles a given number
doubleMe x = x + x

-- Doubles a given number if it's greater than 100
doubleSmallNum x = if x > 100 then x else x * 2

-- Max (using guards)
max' :: (Ord a) => a -> a -> a
max' a b
	| a > b 	= a
	| otherwise = b
	
-- initials (using 'where' clause)
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname
          
-- cylinder (using 'let .. in')
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea


-- Function: capital
capital :: String -> String
capital "" = "Empty string provided"
capital l@(x:_) = "The first letter of " ++ l ++ " is: " ++ [x]

-- Custom lists
data List a = Nil
            | Cons a (List a) deriving (Show, Read, Eq, Ord)

toHsList :: List a -> [a]
toHsList Nil = []
toHsList (Cons x xs) = x : toHsList xs

toList :: [a] -> List a
toList [] = Nil
toList (x : xs) = Cons x (toList xs)

-- Binary trees
data BT a = Empty
          | Fork a (BT a) (BT a) deriving (Show, Read, Eq, Ord)

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



