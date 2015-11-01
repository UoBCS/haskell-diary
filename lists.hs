
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

-- Drop from list every n elements
dropEvery :: [a] -> Int -> [a]
dropEvery l n  = dropEvery' l 1
          where dropEvery' [] _ = []
                dropEvery' (x:xs) i | i `mod` n == 0 = dropEvery' xs (i + 1)
                                    | otherwise      = x : (dropEvery' xs (i + 1))

splitFn n a b = let (l, r, i) = a in
              if    i <= n
              then (l ++ [b], r, i + 1)
              else (l, r ++ [b], i + 1)

split :: [a] -> Int -> ([a], [a])
split ls n = let (l, r, _) = foldl (splitFn n) ([], [], 1) ls in (l, r)

sliceFn l u a b = let (ls, i) = a in
          if i >= l && i <= u
          then (ls ++ [b], i + 1)
          else (ls, i + 1)

slice :: [a] -> Int -> Int -> [a]
slice ls l u = let (result, _) = foldl (sliceFn l u) ([], 1) ls in result


remove_at :: Int -> [a] -> [a]
remove_at 0 _  = error "Index out of bounds"
remove_at _ [] = error "Index out of bounds"
remove_at n (x:xs) | n == 1    = xs
                   | otherwise = x : (remove_at (n - 1) xs)
