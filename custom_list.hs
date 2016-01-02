
-- custom_list.hs
-- Custom lists

data List a = Nil
            | Cons a (List a) deriving (Show, Read, Eq, Ord)

toHsList :: List a -> [a]
toHsList Nil = []
toHsList (Cons x xs) = x : toHsList xs

toList :: [a] -> List a
toList [] = Nil
toList (x : xs) = Cons x (toList xs)
