{-
        FUNCTOR LAWS

        1) fmap id = id
        2) fmap (f . g) F = fmap f (fmap g F)
-}

{-instance Functor ((->) r) where
         fmap f g = (\x -> f $ g x)-}

-- Function Functor
instance Functor ((->) r) where
         fmap = (.)

-- IO Functor
{-instance Functor IO where
         fmap f action = do
                         res <- action
                         return (f res)-}

instance Functor IO where
         fmap f action = action >>= (\res -> return (f res))

-- Breaking the law with CMaybe
data CMaybe a = CNothing | CMaybe Int a deriving (Show)

instance Functor CMaybe where
         fmap f CNothing = CNothing
         fmap f (Just n x) = Just (n + 1) (f x)
