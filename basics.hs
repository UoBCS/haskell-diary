

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
