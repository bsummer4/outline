module Editor where
import Prelude
import Util
import OL
import Edit

data State = State Addr OL deriving Eq
data Mut
	= SelDown | SelLeft | SelUp | SelRight | Select Addr
	| ReplaceTxt OLStr | Delete | Nada
	| InsBefore OLStr | InsAfter OLStr | InsAbove OLStr | InsBelow OLStr

-- Repair a potentially invalid address.
fudgeAddr :: State -> State
fudgeAddr (State (Addr addr) ol) =
	State (Addr $ reverse $ fudge (reverse addr) ol) ol where
		fudge [] _ = []
		fudge _ (OL _ []) = []
		fudge (a:as) (OL s sub) =
			if a<0 then fudge (0:as) (OL s sub) else
			if a>=length sub then fudge (a-1:as) (OL s sub) else
			a:fudge as (sub!!a)

getNode :: State -> OLStr
getNode (State (Addr addr) ol) = r (reverse addr) ol where
	r [] (OL s _) = s
	r _ (OL _ []) = error "invalid selection"
	r (a:as) (OL _ sub) =
		if or[a>=length sub,a<0] then error "invalid selection" else r as (sub!!a)

validSel a ol = st == fudgeAddr st where st = State a ol
down (Addr a) = Addr $ case a of {[]->[]; b:bs->(b+1):bs}
left (Addr a) = Addr $ case a of {[]->[]; b:bs->bs}
right (Addr a) = Addr $ (0:a)
up (Addr a) = Addr $ case a of {[]->[]; b:bs->(b-1):bs}
moveTo a' (State a o) = if validSel a' o then State a' o else State a o
simpleEdit o e = fst $ edit o e
simpleEdits o es = fst $ edits o es

applies :: [Mut] -> State -> State
applies muts s = foldl (flip apply) s muts

apply :: Mut -> State -> State
apply op s@(State a o) = case op of
	Nada -> s
	Select a' -> moveTo a' s
	SelDown -> moveTo (down a) (State a o)
	SelUp -> moveTo (up a) (State a o)
	SelLeft -> moveTo (left a) (State a o)
	SelRight -> moveTo (right a) (State a o)
	Delete -> fudgeAddr $ State a $ simpleEdit o $ DEL a
	ReplaceTxt t -> State a $ simpleEdit o $ EDT a t
	InsBefore t -> State a $ simpleEdit o $ ADD a $ OL t []
	InsAfter t -> State (down a) $ simpleEdit o $ ADD (down a) $ OL t []
	InsBelow t -> State (right a) $ simpleEdit o $ ADD (right a) $ OL t []
	InsAbove t -> State a $ simpleEdits o $ [
		DEL a, ADD a (OL t []), ADD (right a) (olget a o)]
