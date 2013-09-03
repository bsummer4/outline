module Lib where
import Prelude
type Addr = [Int]
data DOM = Node String [(String,String)] (Maybe String) [DOM]
data Outline = OLTree String [Outline] | OLLeaf String
data State = State Outline Addr
data Err = InvalidAddress
data Message
 = Select
 | InsBefore String
 | InsAfter String
 | InsBelow String
 | InsAbove String
 | Replace String
 | Delete

apply :: State -> Message -> Either State Err
apply s m = Left s

one n = [n]
mapM f l = sequence $ map f l
iter f l = mapM f l >> return ()
mapi f l = zipWith f [0..] l

split1 "" = Nothing
split1 cs = Just $ r [] cs where
	r acc [] = (reverse acc,"")
	r acc (',':cs) = (reverse acc, cs)
	r acc (c:cs) = r (c:acc) cs

split s = r [] s where
	r acc s = case split1 s of
		Nothing -> reverse acc
		Just (s,ss) -> r (s:acc) ss

charInt :: Char -> Maybe Int
charInt '0' = Just 0
charInt '1' = Just 1
charInt '2' = Just 2
charInt '3' = Just 3
charInt '4' = Just 4
charInt '5' = Just 5
charInt '6' = Just 6
charInt '7' = Just 7
charInt '8' = Just 8
charInt '9' = Just 9
charInt _ = Nothing

-- TODO Incomplete
readI :: String -> Maybe Int
readI [] = Nothing
readI [c] = charInt c
readI _ = Nothing
