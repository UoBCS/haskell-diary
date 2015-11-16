import Control.Applicative

class (Functor f) => Applicative' f where
      pure :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b

instance Applicative' Maybe where
         pure          = Just
         Nothing <*> _ = Nothing
         Just f <*> a  = fmap f a


{-instance Applicative' IO where
         pure    = return
         a <*> b = do
                   f <- a
                   x <- b
                   return (f x)-}


instance Applicative' IO where
         pure    = return
         a <*> b = a >>= (\f -> b >>= (\x -> return (f x)))

instance Applicative' ((->) r) where
         pure a   = \_ -> a
         ff <*> g = (\x -> ff x (g x))
