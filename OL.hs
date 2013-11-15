module OL
	( OLStr, ols, unols
	, OL(OL), olmap, olread, olshow, olexample
	, Addr(Addr), addrshow, addrread, addrmap
	, olmapAddr, isChildOf
	) where

import Prelude
import Util
data Addr = Addr [Int] deriving (Eq,Show)
data OLStr = OLStr String deriving (Eq,Show)
data OL = OL OLStr [OL] deriving (Eq,Show)
data Lexeme = INDENT | DEDENT | LINE OLStr deriving Show
trim s = map untab $ reverse $ unf $ reverse $ unf s where
	unf "" = ""
	unf (' ':s) = unf s
	unf ('\t':s) = unf s
	unf s = s
	untab '\t' = ' '
	untab x = x

ols s = OLStr $ case trim s of {[]->"#"; ts->ts}
unols (OLStr s) = s
α = ols
ol t s = OL (α t) s
l t = OL (α t) []
olexample = ol "h" [l "i",l "j",ol "k" [l "hihihi there",ol "w" [l "t",l "f"]]]
addrmap (Addr a) f l = mapi (\i e -> f (Addr(i:a)) e) l
addrshow (Addr a) = comma $ map show a
addrread s = Addr $ map parseInt' $ uncomma s

olshow n = r (0::Int) n ++ "\n" where
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
	pgetText acc pr [] [] = reverse acc
	pgetText acc pr [] "\n" = reverse acc
	pgetText acc pr str [] = reverse ((LINE $ ols $ reverse str):acc)
	pgetText acc pr str ('\n':cs) = getIndent ((LINE$ols$reverse str):acc) pr 0 cs
	pgetText acc pr str (c:cs) = pgetText acc pr (c:str) cs
	overdent = "too much indentation"
	dent acc o n cs = case compare n o of
		GT -> if o+1/=n then error overdent else pgetText(INDENT:acc) n "" cs
		LT -> dent (DEDENT:acc) (o-1) n cs
		EQ -> pgetText acc n "" cs

olmap f ol = case f ol of {Just ol'->ol'; Nothing->descend f ol} where
	descend f (OL l []) = (OL l [])
	descend f (OL l subs) = OL l $ map (olmap f) subs

olmapAddr f ol = olmapAddr' f (Addr[]) ol where
	olmapAddr' f a ol = case f a ol of {Just ol'->ol'; Nothing->descendAddr f a ol}
	descendAddr f addr (OL l []) = (OL l [])
	descendAddr f addr (OL l subs) = OL l $ addrmap addr (olmapAddr' f) subs
	ns (OL s []) = s
	ns (OL s _) = s

isChildOf a b = arr (reverse a) (reverse b) where
	arr [] _ = Nothing
	arr [i] [] = Just i
	arr _ [] = Nothing
	arr (a:as) (b:bs) = if (a==b) then arr as bs else Nothing
