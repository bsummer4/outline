module Editor (
	State, editor, apply, stSel, stOL, Operation(
		SelDown, SelLeft, SelUp, SelRight, Select, ReplaceTxt, Delete,
		Nada, InsBefore, InsAfter, InsAbove, InsBelow, Undo
	)) where
import Prelude
import Util
import Outline
import Edit

data State = State
	{ stSel :: Addr
	, stOL :: Outline
	, stUndos :: [(Addr,[Edit])]
	, stClip :: Maybe Outline
	}

editor :: Addr -> Outline -> State
editor a o = State a o [] Nothing

data Operation
	= SelDown | SelLeft | SelUp | SelRight | Select Addr
	| ReplaceTxt OLStr | Delete | Nada
	| InsBefore OLStr | InsAfter OLStr | InsAbove OLStr | InsBelow OLStr
	| Undo

-- Repair a potentially invalid address.
fudgeAddr :: State -> State
fudgeAddr (State (Addr addr) ol undos c) =
	State (Addr $ reverse $ fudge (reverse addr) ol) ol undos c where
		fudge [] _ = []
		fudge _ (OL _ []) = []
		fudge (a:as) (OL s sub) =
			if a<0 then fudge (0:as) (OL s sub) else
			if a>=length sub then fudge (a-1:as) (OL s sub) else
			a:fudge as (sub!!a)

-- TODO use addrParent, addrOlder, etc instead of these.
down (Addr a) = Addr $ case a of {[]->[]; b:bs->(b+1):bs}
left (Addr a) = Addr $ case a of {[]->[]; _:bs->bs}
right (Addr a) = Addr $ (0:a)
up (Addr a) = Addr $ case a of {[]->[]; b:bs->(b-1):bs}

moveAddr :: Addr -> State -> Addr
moveAddr a' (State a o _ _) = if addrOk a' o then a' else a

doundo :: State -> Maybe State
doundo (State _ o u c) = case u of
	[] -> Nothing
	(a',ops):m -> case edits o ops of
		Nothing -> Nothing
		Just (o',_) -> Just $ State a' o' m c

dropNOPs :: State -> State
dropNOPs (State a o [] c) = State a o [] c
dropNOPs (State a o ((_,[]):more) c) = dropNOPs $ State a o more c
dropNOPs s = s

apply :: Operation -> State -> State
apply o s = case apply' o s of {Nothing->s; Just s'->s'}

apply' :: Operation -> State -> Maybe State
apply' Undo s = doundo s
apply' op s@(State a ol u c) = case compile op s of
	(a',es) -> case edits ol es of
		Nothing -> Nothing
		Just (ol',esMirror) ->
			Just $ dropNOPs $ fudgeAddr $ State a' ol' ((a,esMirror):u) c

-- The address that we yeild might not be valid, but passing it through
-- ‘FudgeAddr’ should give the correct result.
compile :: Operation -> State -> (Addr,[Edit])
compile op s@(State a o _ _) = case op of
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
	InsAbove t -> (a,[DEL a, ADD a (OL t []), ADD (right a) (fromJust $ olget a o)])
	Undo -> undefined
