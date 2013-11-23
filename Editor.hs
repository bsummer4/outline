module Editor where
import Prelude
import Util
import OL
import Edit

data State = State Addr OL [(Addr,[Edit])] deriving Eq
data Operation
	= SelDown | SelLeft | SelUp | SelRight | Select Addr
	| ReplaceTxt OLStr | Delete | Nada
	| InsBefore OLStr | InsAfter OLStr | InsAbove OLStr | InsBelow OLStr
	| Undo

-- Repair a potentially invalid address.
fudgeAddr :: State -> State
fudgeAddr (State (Addr addr) ol undos) =
	State (Addr $ reverse $ fudge (reverse addr) ol) ol undos where
		fudge [] _ = []
		fudge _ (OL _ []) = []
		fudge (a:as) (OL s sub) =
			if a<0 then fudge (0:as) (OL s sub) else
			if a>=length sub then fudge (a-1:as) (OL s sub) else
			a:fudge as (sub!!a)

getNode :: State -> OLStr
getNode (State (Addr addr) ol undos) = r (reverse addr) ol where
	r [] (OL s _) = s
	r _ (OL _ []) = error "invalid selection"
	r (a:as) (OL _ sub) =
		if or[a>=length sub,a<0] then error "invalid selection" else r as (sub!!a)

validSel a ol = st == fudgeAddr st where st = (State a ol [])
down (Addr a) = Addr $ case a of {[]->[]; b:bs->(b+1):bs}
left (Addr a) = Addr $ case a of {[]->[]; b:bs->bs}
right (Addr a) = Addr $ (0:a)
up (Addr a) = Addr $ case a of {[]->[]; b:bs->(b-1):bs}

moveAddr :: Addr -> State -> Addr
moveAddr a' (State a o _) = if validSel a' o then a' else a

apply :: Operation -> State -> State
apply op s@(State a ol undos) = fudgeAddr$State a' ol'((a,esMirror):undos) where
	(a',es) = compile op s
	(ol',esMirror) = edits ol es

-- The address that we yeild might not be valid, but passing it through
-- ‘FudgeAddr’ should give the correct result.
compile :: Operation -> State -> (Addr,[Edit])
compile op s@(State a o u) = case op of
	Nada -> (a,[])
	Select a' -> (moveAddr a' s, [])
	SelDown -> (moveAddr (down a) s, [])
	SelUp -> (moveAddr (up a) s, [])
	SelLeft -> (moveAddr (left a) s, [])
	SelRight -> (moveAddr (right a) s, [])
	Delete -> (a,[DEL a])
	ReplaceTxt t -> (a,[EDT a t])
	InsBefore t -> (a,[ADD a $ OL t []])
	InsAfter t -> (down a,[ADD (down a) $ OL t []])
	InsBelow t -> (right a,[ADD (right a) $ OL t []])
	InsAbove t -> (a,[DEL a, ADD a (OL t []), ADD (right a) (olget a o)])
	Undo -> case u of
		[] -> (a,[])
		(a',ops):more -> (a',ops)
