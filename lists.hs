
-- lists.sh
-- Contains list functions (concepts: recursion and comprehesions)


-- Function: mLast
mLast [] = error "Empty list"
mLast [x] = x
mLast (_:xs) = mLast xs

-- Function: mFirst
mFirst [] = error "Empty list"
mFirst (x:_) = x

-- Function: mLength
mLength [] = 0
mLength (_:xs) = 1 + mLength xs

-- Function: elementAt
elementAt [] _ = error "Index out of bounds"
elementAt (x : _) 0 = x
elementAt (x : xs) n = elementAt xs (n - 1)

-- Function: mReverse
mReverse [] = []
mReverse (x : xs) = (mReverse xs) ++ [x]

-- Function: isPalindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == reverse l

-- Function: flatten
flatten :: [[a]] -> [a]
flatten l = flatten' l []
        where
        flatten' [] acc = acc
        flatten' (x : xs) acc = flatten' xs (acc ++ x)

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = (x:(x:(dupli xs)))

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (repli' x n) ++ (repli xs n)
      where repli' _ 0 =  []
            repli' x n = x : (repli' x (n - 1))



