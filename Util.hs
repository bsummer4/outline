{-# LANGUAGE UnicodeSyntax #-}

module Util where
import Prelude

myintersperse :: a -> [a] -> [a]
myintersperse _ [] = []
myintersperse _ [a] = [a]
myintersperse sep (a:cs) = a:sep:myintersperse sep cs

one :: a -> [a]
one n = [n]

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap _ Nothing = Nothing
mmap f (Just a) = Just (f a)

fromJust :: Maybe a -> a
fromJust Nothing = error "Bad code! Used fromJust on a Nothing value!"
fromJust (Just a) = a

data WalkOp a = DeleteSubtree | Descend | Replace a

lwalk :: (Int → a → WalkOp a) → [a] → [a]
lwalk f l = r 0 l where
	r _ [] = []
	r i (x:xs) = case f i x of
		DeleteSubtree -> r (i+1) xs
		Descend -> x:r (i+1) xs
		Replace a -> a:r (i+1) xs

-- mymapM :: Monad m => (a -> m b) -> [a] -> m [b]
mymapM f l = sequence $ map f l

-- iter :: Monad m => (a -> m b) -> [a] -> m ()
iter f l = mymapM f l >> return ()

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f l = zipWith f [0..] l

prefix :: Eq a => [a] -> [a] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (a:as) (b:bs) = if a==b then prefix as bs else False

comma :: [String] -> String
comma = concat . myintersperse ","

uncomma1 :: String -> Maybe(String,String)
uncomma1 "" = Nothing
uncomma1 str = Just(r [] str) where
	r acc [] = (reverse acc,"")
	r acc (',':cs) = (reverse acc,cs)
	r acc (c:cs) = r (c:acc) cs

uncomma :: String -> [String]
uncomma str = r [] str where
	r acc e = case uncomma1 e of
		Nothing -> reverse acc
		Just(s,ss) -> r (s:acc) ss

digit :: Char -> Maybe Int
digit c = case c of
	{ '0' -> Just 0; '1' -> Just 1; '2' -> Just 2; '3' -> Just 3 ; '4' -> Just 4
	; '5' -> Just 5; '6' -> Just 6; '7' -> Just 7; '8' -> Just 8 ; '9' -> Just 9
	; _ -> Nothing }

parseInt' :: String -> Int
parseInt' s = case parseInt s of {Nothing->error "parseInt failed"; Just i->i}

parseInt :: String -> Maybe Int
parseInt str = case str of {[]->Nothing; l->r 0 1 $ reverse l} where
	r a _ [] = Just a
	r a m (c:cs) = case digit c of
		Nothing -> Nothing
		Just code -> r (a+(m*code)) (m*10) cs
