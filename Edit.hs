{-# LANGUAGE UnicodeSyntax #-}

-- This is our core set of editing operations. All operations are implemented in
-- terms of these. To enable undo/redo, all operations are reversible. The
-- ‘edit’ function returns the result of an operation and the reversed
-- operation.

-- The core operations are add, replace sub-tree, delete, move, and
-- replace-text. Add a node to the location of an existing node doesn't delete
-- anything, it just shifts the existing node and it's lower siblings down one.

-- Editing operations are strict. For example, deleting a non-existent node will
-- yeild ‘Nothing’ instead simply not changing anything.

module Edit(Edit(ADD,RPL,DEL,EDT),edit,edit',edits) where
import Prelude
import Util
import Outline

data Edit
	= ADD Addr Outline
	| RPL Addr Outline
	| DEL Addr
	| EDT Addr OLStr
--	| MOV Addr Addr
	deriving (Show,Eq)

editShow (ADD a o) = "ADD " ++ addrShow a ++ "\n" ++ olshow o
editShow (RPL a o) = "RPL " ++ addrShow a ++ "\n" ++ olshow o
editShow (DEL a) = "DEL " ++ addrShow a
editShow (EDT a s) = "EDT " ++ addrShow a ++ unols s

edit :: Outline → Edit → Maybe (Outline,Edit)
edit o e = if opOkay o e then Just(mutate o e,undo o e) else Nothing

edit' :: Outline → Edit → (Outline,Edit)
edit' o e = case edit o e of {Just r -> r; Nothing ->
	error $ "Invalid edit: (" ++ show e ++ ") on (" ++ show o ++ ")."}

edits :: Outline → [Edit] → Maybe (Outline,[Edit])
edits outline es = foldl r (Just(outline,[])) es where
	r Nothing _ = Nothing
	r (Just (o,undos)) e = case edit o e of
		Nothing -> Nothing
		Just (o',undoOp) -> Just (o',undoOp:undos)

-- Utilities ------------------------------------------------------------------
bad :: a
bad = error "Something went terribly wrong in Edit.hs"

linsert :: [a] -> Int -> a -> [a]
linsert l 0 e = e:l
linsert [] _ _ = bad
linsert (a:as) n e = a:linsert as (n-1) e

canAddHere :: Addr -> Outline -> Bool
canAddHere (Addr[]) _ = False
canAddHere a@(Addr(0:_)) ol = addrOk (addrParent a) ol
canAddHere (Addr(i:is)) ol = addrOk (Addr(i-1:is)) ol

opOkay :: Outline -> Edit -> Bool
opOkay ol (ADD a _) = canAddHere a ol
opOkay ol (RPL a _) = addrOk a ol
opOkay ol (DEL a) = addrOk a ol
opOkay ol (EDT a _) = addrOk a ol
--opOkay ol (MOV f t) = addrOk f ol && canAddHere t ol

getFrag :: Addr -> Outline -> Outline
getFrag a o = case olget a o of {Nothing->bad; Just f->f}

-- Operations and Their Inverses ----------------------------------------------
undo :: Outline → Edit → Edit
undo outline pedit = case pedit of
	RPL a _ -> RPL a $ getFrag a outline
	DEL (Addr[]) -> RPL (Addr[]) outline
	DEL a -> ADD a $ getFrag a outline
	ADD a _ -> DEL a
	EDT a _ -> EDT a $ ols $ oltext $ getFrag a outline
--	MOV f t -> MOV t f

mutate :: Outline → Edit → Outline
mutate outline operation = case operation of
	DEL delAt -> del outline delAt
	ADD addAt frag -> add outline addAt frag
	RPL rplAt frag -> rpl outline rplAt frag
	EDT a t -> edt outline a t
--	MOV f t -> mutate (mutate outline $ DEL f) $ ADD t $ getFrag f outline

edt :: Outline → Addr → OLStr → Outline
edt o at txt = olwalk f o where
	f a _ = if a/=at then Descend else case getFrag at o of
		(OL _ oldchilds) -> Replace $ OL txt oldchilds

del :: Outline → Addr → Outline
del outline delAt = olwalk f outline where
	f a _ = if a==delAt then DeleteSubtree else Descend

rpl :: Outline → Addr → Outline → Outline
rpl outline replaceAt frag = olwalk f outline where
	f a _ = if a==replaceAt then Replace frag else Descend

add :: Outline → Addr → Outline → Outline
add _ (Addr[]) _ = bad
add outline (Addr addAt) frag = olwalk f outline where
	f (Addr a) (OL s childs) = case addAt `isChildOf` a of
		Nothing -> Descend
		Just idx -> Replace $ OL s $ linsert childs idx frag
