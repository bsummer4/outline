{-# LANGUAGE UnicodeSyntax #-}

module OL
	( OLStr, ols, unols, olget, oltext
	, OL(OL), olmap, olread, olshow, olexample
	, Addr(Addr), addrshow, addrread, addrmap, addrOk
	, addrBefore, addrAfter, addrParent, addrChild
	, olmapAddr, isChildOf
	) where

import Prelude
import Util
data Addr = Addr [Int] deriving (Eq,Show)
data OLStr = OLStr String deriving (Eq,Show)
data OL = OL OLStr [OL] deriving (Eq,Show)
data Lexeme = INDENT | DEDENT | LINE OLStr deriving Show

unols :: OLStr -> String
unols (OLStr s) = s

oltext :: OL -> String
oltext (OL (OLStr s) _) = s

addrOk :: Addr -> OL -> Bool
addrOk a ol = case olget a ol of {Nothing->False; Just _->True}

addrBefore (Addr a) = Addr $ case a of {[]->[]; b:bs->(b-1):bs}
addrAfter (Addr a) = Addr $ case a of {[]->[]; b:bs->(b+1):bs}
addrParent (Addr a) = Addr $ case a of {[]->[]; b:bs->bs}
addrChild (Addr a) = Addr $ (0:a)

olget :: Addr → OL → Maybe OL
olget (Addr addr) ol = r (reverse addr) ol where
	r [] o = Just o
	r _ (OL _ []) = Nothing
	r (a:as) (OL _ sub) =
		if or[a>=length sub,a<0] then Nothing else r as (sub!!a)

ols :: String -> OLStr
ols str = OLStr $ case trim str of {[]->"#"; ts->ts} where
	trim s = map untab $ reverse $ unf $ reverse $ unf s
	unf "" = ""
	unf (' ':s) = unf s
	unf ('\t':s) = unf s
	unf s = s
	untab '\t' = ' '
	untab x = x

olexample :: OL
olexample = ol "h" [l "i",l "j",ol "k" [l "hihihi there",ol "w" [l "t",l "f"]]]
	where
		ol t s = OL (ols t) s
		l t = OL (ols t) []

addrmap (Addr a) f l = mapi (\i e -> f (Addr(i:a)) e) l
addrshow (Addr a) = comma $ map show a
addrread s = Addr $ map parseInt' $ uncomma s

olshow node = r (0::Int) node ++ "\n" where
	join = concat . myintersperse "\n"
	r d (OL (OLStr s) []) = indent d ++ s
	r d (OL (OLStr s) cs) = join $ (indent d ++ s):(map (r(d+1)) cs)
	indent 0 = ""
	indent n = if n<0 then error "Bad Logic in ‘indent’" else "\t" ++ indent(n-1)

olread = finalize . ppp . reorder . ollex where
	reorder (t:INDENT:ts) = INDENT:t:reorder ts
	reorder (t:ts) = t:reorder ts
	reorder [] = []
	finalize (ol,[]) = ol
	finalize _ = error "wut.finalize"
	ppp (INDENT:(LINE l):ts) = case pseq [] ts of {(subs,r)->(OL l subs,r)}
	ppp (LINE l:ts) = (OL l [],ts)
	ppp _ = error "wut.ppp"
	pseq acc (DEDENT:remain) = (reverse acc, remain)
	pseq acc ts = case ppp ts of {(t,remain) -> pseq (t:acc) remain}
	ollex s = getIndent [] (0::Int) (0::Int) s
	getIndent acc pr count ('\t':cs) = getIndent acc pr (count+1) cs
	getIndent acc pr count cs = dent acc pr count cs
	pgetText acc _ [] [] = reverse acc
	pgetText acc _ [] "\n" = reverse acc
	pgetText acc _ str [] = reverse ((LINE $ ols $ reverse str):acc)
	pgetText acc pr str ('\n':cs) = getIndent ((LINE$ols$reverse str):acc) pr 0 cs
	pgetText acc pr str (c:cs) = pgetText acc pr (c:str) cs
	overdent = "too much indentation"
	dent acc o n cs = case compare n o of
		GT -> if o+1/=n then error overdent else pgetText(INDENT:acc) n "" cs
		LT -> dent (DEDENT:acc) (o-1) n cs
		EQ -> pgetText acc n "" cs

olmap f ol = case f ol of {Just ol'->ol'; Nothing->descend ol} where
	descend (OL l []) = (OL l [])
	descend (OL l subs) = OL l $ map (olmap f) subs

olmapAddr f outline = olmapAddr' (Addr[]) outline where
	olmapAddr' a ol = case f a ol of {Just ol'->ol'; Nothing->descendAddr a ol}
	descendAddr _ (OL l []) = (OL l [])
	descendAddr addr (OL l subs) = OL l $ addrmap addr olmapAddr' subs

isChildOf :: [Int] -> [Int] -> Maybe Int
isChildOf parent child = arr (reverse parent) (reverse child) where
	arr [] _ = Nothing
	arr [i] [] = Just i
	arr _ [] = Nothing
	arr (a:as) (b:bs) = if (a==b) then arr as bs else Nothing
