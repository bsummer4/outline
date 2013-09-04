--module OL (Addr(Addr),addrshow,addrread,addrmap,OL(OLTree,OLLeaf),olexample,olshow,olread,olreplace,olmap,olnop,olchtxt,olinsertAt,chLast,validSel) where
module OL where
import Prelude
import Util

data Addr = Addr [Int] deriving Eq
data OL = OLTree String [OL] | OLLeaf String
data Lexeme = INDENT | DEDENT | LINE String deriving Show

addrmap (Addr a) f l = mapi (\i e -> f (Addr(i:a)) e) l
addrshow (Addr a) = comma $ map show a
addrread s = Addr $ map parseInt' $ uncomma s

olexample = OLTree "h" [OLLeaf "i", OLLeaf "j", OLTree "k"
	[OLLeaf "hihihi there", OLTree "w" [OLLeaf "t", OLLeaf "f"]]]

olshow n = r (0 :: Int) n ++ "\n" where
	join = concat . myintersperse "\n"
	r d (OLLeaf s) = indent d ++ s
	r d (OLTree s cs) = join $ (indent d ++ s):(map (r(d+1)) cs)
	indent 0 = ""
	indent n = if n<0 then error "Bad Logic in ‘indent’" else "\t" ++ indent(n-1)

olread = finalize . ppp . reorder . ollex
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
dent acc o n cs = case compare n o of
	GT -> if o+1/=n then error "wut.dent" else pgetText (INDENT:acc) n "" cs
	LT -> dent (DEDENT:acc) (o-1) n cs
	EQ -> pgetText acc n "" cs

olmap f ol = case f ol of {Just ol'->ol'; Nothing->descend f ol}
descend f (OLLeaf l) = (OLLeaf l)
descend f (OLTree l subs) = (OLTree l $ map (olmap f) subs)

olmapAddr :: (Addr -> OL -> Maybe OL) -> OL -> OL
olmapAddr f ol = olmapAddr' f (Addr[]) ol

olmapAddr' :: (Addr -> OL -> Maybe OL) -> Addr -> OL -> OL
olmapAddr' f a ol = case f a ol of {Just ol'->ol'; Nothing->descendAddr f a ol}

descendAddr :: (Addr -> OL -> Maybe OL) -> Addr -> OL -> OL
descendAddr f addr (OLLeaf l) = (OLLeaf l)
descendAddr f addr (OLTree l subs) = (OLTree l $ addrmap addr (olmapAddr' f) subs)

olnop :: OL -> OL
olnop = olmap (\_ -> Nothing)

ns (OLLeaf s) = s
ns (OLTree s _) = s

olchtxt :: String -> String -> OL -> OL
olchtxt test replace n =
	olmapAddr f n where
		f a n = if (ns n)/=test then Nothing
			else case n of
				OLLeaf _ -> Just $ OLLeaf replace
				OLTree _ subs -> Just $ OLTree replace subs

olreplace :: Addr -> String -> OL -> OL
olreplace addr txt n = olmapAddr f n where
	f a n = if a/=addr then Nothing else case n of
		OLLeaf _ -> Just $ OLLeaf txt
		OLTree _ subs -> Just $ OLTree txt subs

olinsertAt :: Addr -> String -> OL -> OL
olinsertAt (Addr addr) txt n = olmapAddr f n where
	f (Addr a) n = case addr `isChildOf` a of
		Nothing -> Nothing
		Just index -> case n of
			OLLeaf _ -> error "Bad address given to olinsertAt"
			OLTree s cs -> Just $ OLTree s $ insertAt cs index $ OLLeaf txt

isChildOf a b = isChildOf' (reverse a) (reverse b)
isChildOf' [] _ = Nothing
isChildOf' [i] [] = Just i
isChildOf' _ [] = Nothing
isChildOf' (a:as) (b:bs) = if (a==b) then isChildOf' as bs else Nothing

insertAt :: [a] -> Int -> a -> [a]
insertAt l 0 e = e:l
insertAt [] n e = [e] -- If the index is bad, insert at the end.
insertAt (a:as) n e = a:insertAt as (n-1) e

chLast f (Addr[]) = (Addr[])
chLast f (Addr(i:is)) = (Addr(f i:is))

-- Editing Utilities -----------------------------------------------------------
validSel (Addr a) ol = validSel' (reverse a) ol
validSel' [] _ = True
validSel' _ (OLLeaf _) = False
validSel' (a:as) (OLTree _ sub) =
	if or[a>=length sub,a<0] then False else
		validSel' as (sub!!a)

down (Addr a) = Addr $ case a of {[]->[]; b:bs->(b+1):bs}
left (Addr a) = Addr $ case a of {[]->[]; b:bs->bs}
right (Addr a) = Addr $ (0:a)
up (Addr a) = Addr $ case a of {[]->[]; b:bs->(b-1):bs}
moveTo a' (State a o) = if validSel a' o then State a' o else State a o

olreplaceAt :: Addr -> (OL -> Maybe OL) -> OL -> OL
olreplaceAt (Addr[]) f o = case f o of {Nothing -> (OLLeaf "#"); Just o' -> o'}
olreplaceAt (Addr(i:parent)) f o = olmapAddr r o where
	r (Addr a) n = if a/=parent then Nothing else case n of
		OLLeaf _ -> Nothing
		OLTree t subs -> Just $ OLTree t $ replaceAt subs i f

replaceAt :: [a] -> Int -> (a -> Maybe a) -> [a]
replaceAt [] n f = error "Bad list index given to ‘replaceAt’"
replaceAt (a:as) 0 f = case f a of {Just a'->(a':as); Nothing->as}
replaceAt (a:as) n f = a:replaceAt as (n-1) f

-- Editing Operations ----------------------------------------------------------
data State = State Addr OL
data Mut
	= SelDown | SelLeft | SelUp | SelRight | Select Addr
	| Edit String | Delete | Nada
	| InsBefore String | InsAfter String | InsAbove String | InsBelow String

-- instance Show Addr where show = addrshow
-- instance Show OL where show = olshow
-- instance Show State where
-- 	show (State a o) = show a ++ "\n" ++ olshow o

applies muts s = foldl (\a b -> apply b a) s muts
apply Nada s = s
apply (Edit t) (State a o) = State a $ olreplace a t o
apply (Select a) s = moveTo a s
apply SelDown (State a o) = moveTo (down a) (State a o)
apply SelUp (State a o) = moveTo (up a) (State a o)
apply SelLeft (State a o) = moveTo (left a) (State a o)
apply SelRight (State a o) = moveTo (right a) (State a o)
apply (InsBefore t) (State a o) = State a $ olinsertAt a t o
apply (InsAfter t) (State b o) = State a $ olinsertAt a t o where a = down b
apply Delete (State a o) = State (m a) $ olreplaceAt a (\_ -> Nothing) o where
	m (Addr[]) = Addr[]
	m (Addr(a:as)) = Addr as

apply (InsAbove t) (State a o) = State a $ olreplaceAt a r o where
	r x = Just $ OLTree t [x]

apply (InsBelow t) (State a o) =
	apply SelRight $ State a $ olreplaceAt a r o where
		r (OLLeaf l) = Just $ OLTree l [OLLeaf t]
		r (OLTree l cs) = Just $ OLTree l (OLLeaf t:cs)
