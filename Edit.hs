module Edit where
import Prelude
import Util
import OL

data State = State Addr OL
data Mut
	= SelDown | SelLeft | SelUp | SelRight | Select Addr
	| Edit String | Delete | Nada
	| InsBefore String | InsAfter String | InsAbove String | InsBelow String

olinsertAt (Addr addr) txt n = olmapAddr f n where
	f (Addr a) n = case addr `isChildOf` a of
		Nothing -> Nothing
		Just index -> case n of
			OLLeaf _ -> error "Bad address given to olinsertAt"
			OLTree s cs -> Just $ OLTree s $ insertAt cs index $ OLLeaf txt

insertAt l 0 e = e:l
insertAt [] n e = [e] -- If the index is bad, insert at the end.
insertAt (a:as) n e = a:insertAt as (n-1) e
validSel (Addr a) ol = r (reverse a) ol where
	r [] _ = True
	r _ (OLLeaf _) = False
	r (a:as) (OLTree _ sub) =
		if or[a>=length sub,a<0] then False else r as (sub!!a)

down (Addr a) = Addr $ case a of {[]->[]; b:bs->(b+1):bs}
left (Addr a) = Addr $ case a of {[]->[]; b:bs->bs}
right (Addr a) = Addr $ (0:a)
up (Addr a) = Addr $ case a of {[]->[]; b:bs->(b-1):bs}
moveTo a' (State a o) = if validSel a' o then State a' o else State a o

olreplace addr txt n = olmapAddr f n where
	f a n = if a/=addr then Nothing else case n of
		OLLeaf _ -> Just $ OLLeaf txt
		OLTree _ subs -> Just $ OLTree txt subs

olreplaceAt (Addr[]) f o = case f o of {Nothing -> (OLLeaf "#"); Just o' -> o'}
olreplaceAt (Addr(i:parent)) f o = olmapAddr r o where
	r (Addr a) n = if a/=parent then Nothing else case n of
		OLLeaf _ -> Nothing
		OLTree t subs -> Just $ OLTree t $ replaceAt subs i f

replaceAt [] n f = error "replaceAt: Bad list index"
replaceAt (a:as) 0 f = case f a of {Just a'->(a':as); Nothing->as}
replaceAt (a:as) n f = a:replaceAt as (n-1) f

insAbove t a o = State a $ olreplaceAt a r o where r x = Just $ OLTree t [x]
insBelow t a o = apply SelRight $ State a $ olreplaceAt a r o where
	r (OLLeaf l) = Just $ OLTree l [OLLeaf t]
	r (OLTree l cs) = Just $ OLTree l (OLLeaf t:cs)

del a o = State (m a) $ olreplaceAt a (\_ -> Nothing) o where
	m (Addr[]) = Addr[]
	m (Addr(a:as)) = Addr as

applies muts s = foldl (flip apply) s muts
apply op s@(State a o) = case op of
	Nada -> s
	Delete -> del a o
	Edit t -> State a $ olreplace a t o
	Select a -> moveTo a s
	SelDown -> moveTo (down a) (State a o)
	SelUp -> moveTo (up a) (State a o)
	SelLeft -> moveTo (left a) (State a o)
	SelRight -> moveTo (right a) (State a o)
	InsBefore t -> State a $ olinsertAt a t o
	InsAfter t -> State (down a) $ olinsertAt (down a) t o
	InsAbove t -> insAbove t a o
	InsBelow t -> insBelow t a o
