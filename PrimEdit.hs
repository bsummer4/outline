{-# LANGUAGE UnicodeSyntax #-}

-- This is our core set of editing operations. All operations are implemented in
-- terms of these. To enable undo/redo, all operations are reversable. The
-- ‘edit’ function returns the result of an operation and the reversed
-- operation.

-- The core operations are add, delete, move, and replace-text.
-- Add a node to the location of an existing node doesn't delete anything, it
-- just shifts the existing node and it's lower siblings down one.

-- Editing operations are strict. For example, deleting a non-existant node is
-- a fatal error. TODO This isn't complete true yet.

module PrimEdit(PrimEdit(ADD,DEL,MOV,EDT),edit) where
import Prelude
import Util
import OL

data PrimEdit = ADD Addr OL | DEL Addr | MOV Addr Addr | EDT Addr OLStr
	deriving (Show,Eq)

edit :: OL → PrimEdit → (OL,PrimEdit)
edit o e = (mutate o e,undo o e)

-- Utilities ------------------------------------------------------------------
testundo :: OL → PrimEdit → Bool
testundo ol op = case edit ol op of
	(olNew,opUndo) -> case edit olNew opUndo of
		(olRestore,opRestore) -> olRestore==ol && opRestore==op

text :: OL → OLStr
text (OL s _) = s

insert l 0 e = e:l
insert [] n e = [e] -- If the index is bad, insert at the end.
insert (a:as) n e = a:insert as (n-1) e

get :: Addr → OL → OL
get (Addr a) ol = r (reverse a) ol where
	r [] o@(OL s _) = o
	r _ (OL _ []) = error "invalid address"
	r (a:as) (OL _ sub) =
		if or[a>=length sub,a<0] then error "invalid address" else r as (sub!!a)

data WalkOp a = Delete | Descend | Replace a

lwalk :: (Int → a → WalkOp a) → [a] → [a]
lwalk f l = r 0 l where
	r i [] = []
	r i (x:xs) = case f i x of
		Delete -> r (i+1) xs
		Descend -> x:r (i+1) xs
		Replace a -> a:r (i+1) xs

walk :: (Addr → OL → WalkOp OL) → OL → OL
walk f ol = foo $ r (Addr[]) ol where
	addrmap (Addr a) f l = lwalk (\i e -> f (Addr(i:a)) e) l
	foo (Replace x) = x
	foo Delete = (OL (ols "") [])
	foo Descend = error "this will never happen"
	descendAddr addr (OL l subs) = OL l $ addrmap addr r subs
	r a ol = case f a ol of
		Delete -> Delete
		Descend -> Replace $ descendAddr a ol
		Replace r -> Replace r

-- Operations and Their Inverses ----------------------------------------------
undo :: OL → PrimEdit → PrimEdit
undo outline pedit = case pedit of
	DEL a -> ADD a $ get a outline
	ADD a o -> DEL a
	MOV f t -> MOV t f
	EDT a s -> EDT a $ text $ get a outline

mutate :: OL → PrimEdit → OL
mutate outline operation = case operation of
	DEL delAt -> del outline delAt
	ADD addAt frag -> add outline addAt frag
	MOV f t -> error "TODO"
	EDT a s -> error "TODO"

del outline delAt = walk f outline where
	f a n = if a==delAt then Delete else Descend

-- TODO Throw an error if we can't use the address.
add :: OL → Addr → OL → OL
add _ (Addr[]) _ = error "Can't add a node at the top-level"
add outline (Addr (addAt@(loc:_)))  frag = walk f outline where
	f (Addr a) n@(OL s childs) = case addAt `isChildOf` a of
		Nothing -> Descend
		Just idx -> Replace $ OL s $ insert childs idx frag

validSel (Addr a) ol = r (reverse a) ol where
	r [] _ = True
	r _ (OL _ []) = False
	r (a:as) (OL _ sub) =
		if or[a>=length sub,a<0] then False else r as (sub!!a)
