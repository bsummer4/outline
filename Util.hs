module Util where
import Prelude

myintersperse :: a -> [a] -> [a]
myintersperse sep [] = []
myintersperse sep [a] = [a]
myintersperse sep (a:cs) = a:sep:myintersperse sep cs
one n = [n]
mymapM f l = sequence $ map f l
iter f l = mymapM f l >> return ()
mapi f l = zipWith f [0..] l
prefix [] l = True
prefix l [] = False
prefix (a:as) (b:bs) = if a==b then prefix as bs else False

comma :: [String] -> String
comma = concat . myintersperse ","

uncomma1 :: String -> Maybe(String,String)
uncomma1 "" = Nothing
uncomma1 cs = Just(r [] cs) where
	r acc [] = (reverse acc,"")
	r acc (',':cs) = (reverse acc,cs)
	r acc (c:cs) = r (c:acc) cs

uncomma :: String -> [String]
uncomma s = r [] s where
	r acc s = case uncomma1 s of
		Nothing -> reverse acc
		Just(s,ss) -> r (s:acc) ss

digit c = case c of
	{ '0' -> Just 0; '1' -> Just 1; '2' -> Just 2; '3' -> Just 3 ; '4' -> Just 4
	; '5' -> Just 5; '6' -> Just 6; '7' -> Just 7; '8' -> Just 8 ; '9' -> Just 9
	; _ -> Nothing }

parseInt' s = case parseInt s of {Nothing->error "parseInt failed"; Just i->i}
parseInt l = case l of {[]->Nothing; l->r 0 1 $ reverse l} where
	r a m [] = Just a
	r a m (c:cs) = case digit c of
		Nothing -> Nothing
		Just c -> r (a+(m*c)) (m*10) cs
