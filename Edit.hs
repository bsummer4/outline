{-# LANGUAGE UnicodeSyntax #-}

-- This is our core set of editing operations. All operations are implemented in
-- terms of these. To enable undo/redo, all operations are reversible. The
-- ‘edit’ function returns the result of an operation and the reversed
-- operation.

-- The core operations are add, delete, move, and replace-text.
-- Add a node to the location of an existing node doesn't delete anything, it
-- just shifts the existing node and it's lower siblings down one.

-- Editing operations are strict. For example, deleting a non-existent node is
-- a fatal error. TODO This isn't completely true yet.

module Edit(Edit(ADD,DEL,MOV,EDT),edit,edits) where
import Prelude
import Util
import OL

data Edit = ADD Addr OL|RPL Addr OL|DEL Addr|MOV Addr Addr|EDT Addr OLStr
	deriving (Show,Eq)

edit :: OL → Edit → (OL,Edit)
edit o e = (mutate o e,undo o e)

edits :: OL → [Edit] → (OL,[Edit])
edits outline es = foldl r (outline,[]) es where
	r (o,undos) e = case edit o e of (o',undoOp) -> (o',undoOp:undos)

-- Utilities ------------------------------------------------------------------
-- testundo :: OL → Edit → Bool
-- testundo ol op = case edit ol op of
	-- (olNew,opUndo) -> case edit olNew opUndo of
		-- (olRestore,opRestore) -> olRestore==ol && opRestore==op

text :: OL → OLStr
text (OL s _) = s

insert l 0 e = e:l
insert [] _ e = [e] -- If the index is bad, insert at the end.
insert (a:as) n e = a:insert as (n-1) e

data WalkOp a = Delete | Descend | Replace a

lwalk :: (Int → a → WalkOp a) → [a] → [a]
lwalk f l = r 0 l where
	r _ [] = []
	r i (x:xs) = case f i x of
		Delete -> r (i+1) xs
		Descend -> x:r (i+1) xs
		Replace a -> a:r (i+1) xs

walk :: (Addr → OL → WalkOp OL) → OL → OL
walk f outline = foo $ r (Addr[]) outline where
	amap (Addr a) l = lwalk (\i e -> r (Addr(i:a)) e) l
	foo (Replace x) = x
	foo Delete = (OL (ols "") [])
	foo Descend = error "this will never happen"
	descendAddr addr (OL l subs) = OL l $ amap addr subs
	r a ol = case f a ol of
		Delete -> Delete
		Descend -> Replace $ descendAddr a ol
		Replace newNode -> Replace newNode

-- Operations and Their Inverses ----------------------------------------------
undo :: OL → Edit → Edit
undo outline pedit = case pedit of
	RPL a frag -> RPL a $ olget a outline
	DEL (Addr[]) -> RPL (Addr[]) outline
	DEL a -> ADD a $ olget a outline
	ADD a _ -> DEL a
	MOV f t -> MOV t f
	EDT a _ -> EDT a $ text $ olget a outline

mutate :: OL → Edit → OL
mutate outline operation = case operation of
	DEL delAt -> del outline delAt
	ADD addAt frag -> add outline addAt frag
	RPL rplAt frag -> rpl outline rplAt frag
	EDT a t -> edt outline a t
	MOV _ _ -> error "TODO"

edt :: OL → Addr → OLStr → OL
edt o at txt = walk f o where
	f a _ = if a/=at then Descend else case olget at o of
		(OL _ oldchilds) -> Replace $ OL txt oldchilds

del :: OL -> Addr -> OL
del outline delAt = walk f outline where
	f a _ = if a==delAt then Delete else Descend

rpl :: OL → Addr → OL → OL
rpl outline replaceAt frag = walk f outline where
	f a _ = if a==replaceAt then Replace frag else Descend

-- TODO Throw an error if we can't use the address.
add :: OL → Addr → OL → OL
add _ (Addr[]) _ = error "Can't add a node at the top-level"
add outline (Addr addAt) frag = walk f outline where
	f (Addr a) (OL s childs) = case addAt `isChildOf` a of
		Nothing -> Descend
		Just idx -> Replace $ OL s $ insert childs idx frag
