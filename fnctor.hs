-- data [] a = p[] | a : [a]
-- data Maybe a = Nothing | Just a
-- instance Functor Maybe where
	-- fmap :: (a -> b) -> Maybe a -> Maybe b
--	fmap function Nothing = Nothing
--	fmap function (Just x) = Just (function x)

-- fmap :: Functor f => (a -> b) -> (f a -> f b)

-- data (Either a) b = Left a | Right b
-- instance Functor (Either a) where
	-- fmap :: (a -> b) -> (Either x) a -> (Either x) b
--	fmap function (Left x) = Left x

data (Choice a) b = This a | That b
instance Functor (Choice a) where
-- fmap :: (b -> c) -> (Either x) b -> (Either x) c
	fmap function (This x) = This x
	fmap function (That b) = That (function b)

data Pair a b = Pair a b
instance Functor (Pair a) where
	-- fmap (b -> c) -> (Pair x) b -> Pair x c
	fmap function (Pair x b) = Pair x (function b)	
