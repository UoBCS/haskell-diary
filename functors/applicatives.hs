import Control.Applicative

class (Functor f) => Applicative' f where
      pure :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
         pure          = Just
         Nothing <*> _ = Nothing
         Just f <*> a  = fmap f a
