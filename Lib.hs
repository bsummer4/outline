module Lib where
import Prelude hiding (mapM, intersperse, lex)

type Addr = [Int]
data DOM = Node String [(String,String)] (Maybe String) [DOM]
data Outline = OLTree String [Outline] | OLLeaf String
	deriving Show
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

intersperse :: a -> [a] -> [a]
intersperse sep [] = []
intersperse sep [a] = [a]
intersperse sep (a:cs) = a:sep:intersperse sep cs
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
charInt c = case c of
	'0' -> Just 0
	'1' -> Just 1
	'2' -> Just 2
	'3' -> Just 3
	'4' -> Just 4
	'5' -> Just 5
	'6' -> Just 6
	'7' -> Just 7
	'8' -> Just 8
	'9' -> Just 9
	_ -> Nothing

readI l = case l of {[]->Nothing; l->r 0 1 $ reverse l} where
	r a m [] = Just a
	r a m (c:cs) = case charInt c of
		Nothing -> Nothing
		Just c -> r (a+(m*c)) (m*10) cs

showOutline n = r (0 :: Int) n ++ "\n" where
	join = concat . intersperse "\n"
	r d (OLLeaf s) = indent d ++ s
	r d (OLTree s cs) = join $ (indent d ++ s):(map (r(d+1)) cs)
	indent 0 = ""
	indent n = if n<0 then error "Bad Logic in ‘indent’" else "\t" ++ indent(n-1)

data Lexeme = INDENT | DEDENT | LINE String deriving Show

parse = finalize . p . reorder . lex
reorder (t:INDENT:ts) = INDENT:t:reorder ts
reorder (t:ts) = t:reorder ts
reorder [] = []
finalize (ol,[]) = ol
finalize _ = err "finalize"
err s = error ("wut? " ++ s)
p (INDENT:(LINE l):ts) = case pseq [] ts of {(subs,r)->(OLTree l subs,r)}
p (LINE l:ts) = (OLLeaf l,ts)
p _ = err "p"
pseq acc (DEDENT:remain) = (reverse acc, remain)
pseq acc ts = case p ts of {(t,remain) -> pseq (t:acc) remain}
lex s = getIndent [] (0::Int) (0::Int) s
getIndent acc prev count ('\t':cs) = getIndent acc prev (count+1) cs
getIndent acc prev count cs = dent acc prev count cs
pgetText acc prev [] [] = reverse acc
pgetText acc prev [] "\n" = reverse acc
pgetText acc prev str [] = reverse ((LINE $ reverse str):acc)
pgetText acc prev str ('\n':cs) = getIndent ((LINE$reverse str):acc) prev 0 cs
pgetText acc prev str (c:cs) = pgetText acc prev (c:str) cs
dent acc o n cs = case compare n o of
	GT -> if o+1/=n then err "dent" else pgetText (INDENT:acc) n "" cs
	LT -> dent (DEDENT:acc) (o-1) n cs
	EQ -> pgetText acc n "" cs
