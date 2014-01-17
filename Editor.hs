module Editor
	( Editor, mkeditor, emptyEditor, apply, stSel, stOL, Operation
		( SelDown, SelLeft, SelUp, SelRight, Select, ReplaceTxt, Delete
		, Nada, InsBefore, InsAfter, InsAbove, InsBelow
		, Undo, Copy, Cut, PasteAfter, PasteBefore
	)) where
import Prelude
import Util
import Outline
import Edit

data Editor = State
	{ stSel :: Addr
	, stOL :: Outline
	, stUndos :: [(Addr,[Edit])]
	, stClip :: Maybe Outline
	}
	deriving (Show,Eq)

mkeditor :: Addr -> Outline -> Editor
mkeditor a o = State a o [] Nothing

emptyEditor :: Editor
emptyEditor = mkeditor (Addr[]) $ OL (ols "") []

data Operation
	= SelDown | SelLeft | SelUp | SelRight | Select Addr
	| ReplaceTxt OLStr | Delete | Nada
	| InsBefore OLStr | InsAfter OLStr | InsAbove OLStr | InsBelow OLStr
	| Undo | Copy | Cut | PasteBefore | PasteAfter

-- Repair a potentially invalid address.
fudgeAddr :: Editor -> Editor
fudgeAddr (State (Addr addr) ol undos c) =
	State (Addr $ reverse $ fudge (reverse addr) ol) ol undos c where
		fudge [] _ = []
		fudge _ (OL _ []) = []
		fudge (a:as) (OL s sub) =
			if a<0 then fudge (0:as) (OL s sub) else
			if a>=length sub then fudge (a-1:as) (OL s sub) else
			a:fudge as (sub!!a)

down :: Addr -> Addr
down = addrAfter
left :: Addr -> Addr
left = addrParent
right :: Addr -> Addr
right = addrChild
up :: Addr -> Addr
up = addrBefore

moveAddr :: Addr -> Editor -> Addr
moveAddr a' (State a o _ _) = if addrOk a' o then a' else a

doundo :: Editor -> Maybe Editor
doundo (State _ o u c) = case u of
	[] -> Nothing
	(a',ops):m -> case edits o ops of
		Nothing -> Nothing
		Just (o',_) -> Just $ State a' o' m c

dropNOPs :: Editor -> Editor
dropNOPs (State a o [] c) = State a o [] c
dropNOPs (State a o ((_,[]):more) c) = dropNOPs $ State a o more c
dropNOPs s = s

apply :: Operation -> Editor -> Editor
apply o s = case apply' o s of {Nothing->s; Just s'->s'}

apply' :: Operation -> Editor -> Maybe Editor
apply' Undo s = doundo s
apply' Copy (State a o u _) = Just $ State a o u $ olget a o
apply' Cut s = apply' Delete $ fromJust $ apply' Copy s
apply' op s@(State a ol u c) = case compile op s of
	(a',es) -> case edits ol es of
		Nothing -> Nothing
		Just (ol',esMirror) ->
			Just $ dropNOPs $ fudgeAddr $ State a' ol' ((a,esMirror):u) c

-- The address that we yeild might not be valid, but passing it through
-- ‘FudgeAddr’ should give the correct result.
compile :: Operation -> Editor -> (Addr,[Edit])
compile op s@(State a o _ c) = case op of
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
	PasteBefore -> case c of {Nothing->(a,[]); Just f->(a,[ADD a f])}
	PasteAfter -> case c of {Nothing->(a,[]); Just f->(down a,[ADD (down a) f])}
	Cut -> compile Delete s
	Copy -> undefined
	Undo -> undefined
