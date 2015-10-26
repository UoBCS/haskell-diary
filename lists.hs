
-- lists.sh
-- Contains list functions (concepts: recursion and comprehesions)


-- Gets the last element of a list
mLast :: [a] -> a
mLast [] = error "Empty list"
mLast [x] = x
mLast (_:xs) = mLast xs

-- Gets the first element of a list
mFirst :: [a] -> a
mFirst [] = error "Empty list"
mFirst (x:_) = x

-- Gets the length of a list
mLength :: [a] -> Int
mLength [] = 0
mLength (_:xs) = 1 + mLength xs

-- Gets the elements at a given position in a list
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index out of bounds"
elementAt (x : _) 0 = x
elementAt (x : xs) n = elementAt xs (n - 1)

-- Reverses a given list
mReverse :: [a] -> [a]
mReverse [] = []
mReverse (x : xs) = (mReverse xs) ++ [x]

-- Palindrome of a list
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == reverse l

-- Flattens a list
flatten :: [[a]] -> [a]
flatten l = flatten' l []
        where
        flatten' [] acc = acc
        flatten' (x : xs) acc = flatten' xs (acc ++ x)

-- Duplicates the elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = (x:(x:(dupli xs)))

-- Replicate the elements of a list
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (repli' x n) ++ (repli xs n)
      where repli' _ 0 =  []
            repli' x n = x : (repli' x (n - 1))
