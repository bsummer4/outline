{-# LANGUAGE UnicodeSyntax #-}

module Outline
	( OLStr, ols, unols
	, Outline(OL), oltext, olread, olshow, olexample, olget, olwalk
	, Addr(Addr), addrmap, addrOk, addrBefore, addrAfter, addrParent
	, addrShow, addrChild, isChildOf
	) where

import Prelude
import Util

data Addr = Addr [Int] deriving (Eq,Show)
data OLStr = OLStr String deriving (Eq,Show)
data Outline = OL OLStr [Outline] deriving (Eq,Show)
data Lexeme = INDENT | DEDENT | LINE OLStr deriving Show

unols :: OLStr → String
unols (OLStr s) = s

oltext :: Outline → String
oltext (OL (OLStr s) _) = s

addrShow (Addr i) = unwords $ map show i

addrOk :: Addr → Outline → Bool
addrOk a ol = case olget a ol of {Nothing->False; Just _->True}

addrBefore :: Addr → Addr
addrBefore (Addr a) = Addr $ case a of {[]->[]; b:bs->(b-1):bs}
addrAfter :: Addr → Addr
addrAfter (Addr a) = Addr $ case a of {[]->[]; b:bs->(b+1):bs}
addrParent :: Addr → Addr
addrParent (Addr a) = Addr $ case a of {[]->[]; _:bs->bs}
addrChild :: Addr → Addr
addrChild (Addr a) = Addr $ (0:a)

olget :: Addr → Outline → Maybe Outline
olget (Addr addr) ol = r (reverse addr) ol where
	r [] o = Just o
	r _ (OL _ []) = Nothing
	r (a:as) (OL _ sub) =
		if or[a>=length sub,a<0] then Nothing else r as (sub!!a)

ols :: String → OLStr
ols str = OLStr $ case trim str of {[]->"#"; ts->ts} where
	trim s = map untab $ reverse $ f $ reverse $ f s
	f "" = ""
	f (' ':s) = f s
	f ('\t':s) = f s
	f s = s
	untab '\t' = ' '
	untab x = x

olexample :: Outline
olexample = ol "h" [l "i",l "j",ol "k" [l "hihihi there",ol "w" [l "t",l "f"]]]
	where
		ol t s = OL (ols t) s
		l t = OL (ols t) []

addrmap :: Addr → (Addr → a → b) → [a] → [b]
addrmap (Addr a) f l = mapi (\i e -> f (Addr(i:a)) e) l

olshow :: Outline → String
olshow node = r (0::Int) node ++ "\n" where
	join = concat . myintersperse "\n"
	r d (OL (OLStr s) []) = indent d ++ s
	r d (OL (OLStr s) cs) = join $ (indent d ++ s):(map (r(d+1)) cs)
	indent 0 = ""
	indent n = if n<0 then undefined else "\t" ++ indent(n-1)

olwalk :: (Addr → Outline → WalkOp Outline) → Outline → Outline
olwalk f outline = foo $ r (Addr[]) outline where
	amap (Addr a) l = lwalk (\i e -> r (Addr(i:a)) e) l
	foo (Replace x) = x
	foo DeleteSubtree = (OL (ols "") [])
	foo Descend = undefined
	descendAddr addr (OL l subs) = OL l $ amap addr subs
	r a ol = case f a ol of
		DeleteSubtree -> DeleteSubtree
		Descend -> Replace $ descendAddr a ol
		Replace newNode -> Replace newNode

olread :: String → Outline
olread = finalize . ppp . reorder . ollex where
	reorder (t:INDENT:ts) = INDENT:t:reorder ts
	reorder (t:ts) = t:reorder ts
	reorder [] = []
	finalize (ol,[]) = ol
	finalize _ = undefined
	ppp (INDENT:(LINE l):ts) = case pseq [] ts of {(subs,r)->(OL l subs,r)}
	ppp (LINE l:ts) = (OL l [],ts)
	ppp _ = undefined
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

isChildOf :: [Int] → [Int] → Maybe Int
isChildOf parent child = arr (reverse parent) (reverse child) where
	arr [] _ = Nothing
	arr [i] [] = Just i
	arr _ [] = Nothing
	arr (a:as) (b:bs) = if (a==b) then arr as bs else Nothing
