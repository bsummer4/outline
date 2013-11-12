module Edit where
import Prelude
import Util
import OL

data State = State Addr OL
data Mut
	= SelDown | SelLeft | SelUp | SelRight | Select Addr
	| Edit OLStr | Delete | Nada
	| InsBefore OLStr | InsAfter OLStr | InsAbove OLStr | InsBelow OLStr

getNode :: State -> OLStr
getNode (State (Addr a) ol) = r (reverse a) ol where
	r [] (OL s _) = s
	r _ (OL _ []) = error "invalid selection"
	r (a:as) (OL _ sub) =
		if or[a>=length sub,a<0] then error "invalid selection" else r as (sub!!a)

olinsertAt (Addr addr) txt n = olmapAddr f n where
	f (Addr a) n = case addr `isChildOf` a of
		Nothing -> Nothing
		Just index -> case n of
			OL _ [] -> error "Bad address given to olinsertAt"
			OL s cs -> Just $ OL s $ insertAt cs index $ OL txt []

insertAt l 0 e = e:l
insertAt [] n e = [e] -- If the index is bad, insert at the end.
insertAt (a:as) n e = a:insertAt as (n-1) e
validSel (Addr a) ol = r (reverse a) ol where
	r [] _ = True
	r _ (OL _ []) = False
	r (a:as) (OL _ sub) =
		if or[a>=length sub,a<0] then False else r as (sub!!a)

down (Addr a) = Addr $ case a of {[]->[]; b:bs->(b+1):bs}
left (Addr a) = Addr $ case a of {[]->[]; b:bs->bs}
right (Addr a) = Addr $ (0:a)
up (Addr a) = Addr $ case a of {[]->[]; b:bs->(b-1):bs}
moveTo a' (State a o) = if validSel a' o then State a' o else State a o

olreplace addr txt n = olmapAddr f n where
	f a n = if a/=addr then Nothing else case n of
		OL _ [] -> Just $ OL txt []
		OL _ subs -> Just $ OL txt subs

olreplaceAt (Addr[]) f o = case f o of {Nothing -> (OL (ols "#") []); Just o' -> o'}
olreplaceAt (Addr(i:parent)) f o = olmapAddr r o where
	r (Addr a) n = if a/=parent then Nothing else case n of
		OL _ [] -> Nothing
		OL t subs -> Just $ OL t $ replaceAt subs i f

replaceAt [] n f = error "replaceAt: Bad list index"
replaceAt (a:as) 0 f = case f a of {Just a'->(a':as); Nothing->as}
replaceAt (a:as) n f = a:replaceAt as (n-1) f

insAbove t a o = State a $ olreplaceAt a r o where r x = Just $ OL t [x]
insBelow t a o = apply SelRight $ State a $ olreplaceAt a r o where
	r (OL l []) = Just $ OL l [OL t []]
	r (OL l cs) = Just $ OL l ((OL t []):cs)

del a o = State (m a) $ olreplaceAt a (\_ -> Nothing) o where
	m (Addr[]) = Addr[]
	m (Addr(a:as)) = Addr as

applies muts s = foldl (flip apply) s muts
apply op s@(State a o) = case op of
	Nada -> s
	Delete -> del a o
	Edit t -> State a $ olreplace a t o
	Select a' -> moveTo a' s
	SelDown -> moveTo (down a) (State a o)
	SelUp -> moveTo (up a) (State a o)
	SelLeft -> moveTo (left a) (State a o)
	SelRight -> moveTo (right a) (State a o)
	InsBefore t -> State a $ olinsertAt a t o
	InsAfter t -> State (down a) $ olinsertAt (down a) t o
	InsAbove t -> insAbove t a o
	InsBelow t -> insBelow t a o
