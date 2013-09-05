module OL where
import Prelude
import Util

data Addr = Addr [Int] deriving Eq
data OL = OLTree String [OL] | OLLeaf String
data Lexeme = INDENT | DEDENT | LINE String deriving Show

olexample = OLTree "h" [OLLeaf "i", OLLeaf "j", OLTree "k"
	[OLLeaf "hihihi there", OLTree "w" [OLLeaf "t", OLLeaf "f"]]]

addrmap (Addr a) f l = mapi (\i e -> f (Addr(i:a)) e) l
addrshow (Addr a) = comma $ map show a
addrread s = Addr $ map parseInt' $ uncomma s

olshow n = r (0::Int) n ++ "\n" where
	join = concat . myintersperse "\n"
	r d (OLLeaf s) = indent d ++ s
	r d (OLTree s cs) = join $ (indent d ++ s):(map (r(d+1)) cs)
	indent 0 = ""
	indent n = if n<0 then error "Bad Logic in ‘indent’" else "\t" ++ indent(n-1)

olread = finalize . ppp . reorder . ollex where
	reorder (t:INDENT:ts) = INDENT:t:reorder ts
	reorder (t:ts) = t:reorder ts
	reorder [] = []
	finalize (ol,[]) = ol
	finalize _ = error "wut.finalize"
	ppp (INDENT:(LINE l):ts) = case pseq [] ts of {(subs,r)->(OLTree l subs,r)}
	ppp (LINE l:ts) = (OLLeaf l,ts)
	ppp _ = error "wut.ppp"
	pseq acc (DEDENT:remain) = (reverse acc, remain)
	pseq acc ts = case ppp ts of {(t,remain) -> pseq (t:acc) remain}
	ollex s = getIndent [] (0::Int) (0::Int) s
	getIndent acc prev count ('\t':cs) = getIndent acc prev (count+1) cs
	getIndent acc prev count cs = dent acc prev count cs
	pgetText acc prev [] [] = reverse acc
	pgetText acc prev [] "\n" = reverse acc
	pgetText acc prev str [] = reverse ((LINE $ reverse str):acc)
	pgetText acc prev str ('\n':cs) = getIndent ((LINE$reverse str):acc) prev 0 cs
	pgetText acc prev str (c:cs) = pgetText acc prev (c:str) cs
	overdent = "too much indentation"
	dent acc o n cs = case compare n o of
		GT -> if o+1/=n then error overdent else pgetText(INDENT:acc) n "" cs
		LT -> dent (DEDENT:acc) (o-1) n cs
		EQ -> pgetText acc n "" cs

olmap f ol = case f ol of {Just ol'->ol'; Nothing->descend f ol}
descend f (OLLeaf l) = (OLLeaf l)
descend f (OLTree l subs) = OLTree l $ map (olmap f) subs
olmapAddr f ol = olmapAddr' f (Addr[]) ol
olmapAddr' f a ol = case f a ol of {Just ol'->ol'; Nothing->descendAddr f a ol}
descendAddr f addr (OLLeaf l) = (OLLeaf l)
descendAddr f addr (OLTree l subs) = OLTree l$addrmap addr (olmapAddr' f) subs
ns (OLLeaf s) = s
ns (OLTree s _) = s
isChildOf a b = isChildOf' (reverse a) (reverse b)
isChildOf' [] _ = Nothing
isChildOf' [i] [] = Just i
isChildOf' _ [] = Nothing
isChildOf' (a:as) (b:bs) = if (a==b) then isChildOf' as bs else Nothing
