
-- lists.sh
-- Contains list functions (concepts: recursion and comprehesions)


-- Function: mLast
mLast [] 	 = error "Empty list"
mLast [x] 	 = x
mLast (_:xs) = mLast xs

-- Function: mFirst
mFirst [] 	  = error "Empty list"
mFirst (x:_)  = x

-- Function: mLength
mLength [] = 0
mLength (_:xs) = 1 + mLength xs 
