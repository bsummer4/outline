module UI where
import Prelude hiding (intersperse, getText)
import FFI
import JS
import Util
import OL
import DOM
import Edit

-- Mutable Global Variables ----------------------------------------------------
setSel (Addr is) = setVar "addr" $ concat $ myintersperse "," $ map show is
getSel = getVar "addr" >>= return.addrread
setOutline o = setVar "ol" $ olshow o
getOutline = getVar "ol" >>= return.(olread)
getState = do {s<-getSel; o<-getOutline; return(State s o)}
setState(State a o) = setSel a >> setOutline o

-- DOM -------------------------------------------------------------------------
setAttrs n [] = return ()
setAttrs n ((k,v):as) = setAttr n k v >> setAttrs n as
getOutlineNode = gid "outline" >>= (\ol -> case ol of
	Null -> error "There has to be a node with id=“outline” in the document."
	Nullable node -> return node)

clearPage = do {o<-getOutlineNode; getChilds o >>= iter (removeChild o)}
writePage d = getOutlineNode >>= (\o -> appendChild o d)
setPage x = clearPage >> writePage x
gendom (Node tag attrs txt childs) = do
	n <- mknode tag
	setAttrs n attrs
	case txt of {Nothing->return (); Just s->setText n s}
	mymapM gendom childs >>= iter (appendChild n)
	return n

-- Main ------------------------------------------------------------------------
prompt' q d = do
	x <- prompt q d
	case x of {Null->prompt' q d; Nullable ""->return "#"; Nullable t->return t}

nodeAddr n = getId n >>= r where
	r Null = error "Internal Error: Clickable node has invalid ID"
	r (Nullable i) = return(idAddr i)

select n = nodeAddr n >>= setSel >> buildit
editKey n k = do
	t <- getText n
	case k of
		"h" -> return SelLeft
		"l" -> return SelRight
		"k" -> return SelUp
		"j" -> return SelDown
		"d" -> return Delete
		"i" -> prompt' "Insert Before" "" >>= return . InsBefore . ols
		"a" -> prompt' "Insert After" "" >>= return . InsAfter . ols
		"o" -> prompt' "Insert Below" "" >>= return . InsBelow . ols
		"O" -> prompt' "Insert Above" "" >>= return . InsAbove . ols
		"\r" -> prompt' "Replace Text" t >>= return . Edit . ols
		_ -> return Nada

setupKeys = onKeyPress r where
	r k = do
		a <- getSel
		s <- getState
		n' <- gid $ addrId a
		n <- return $ case n' of {Null->error "bad addr"; Nullable z->z}
		op <- editKey n k
		setState $ apply op s
		buildit

fixAddr ol addr = if validSel addr ol then return addr else
	setSel (Addr[]) >> return(Addr[])

buildit = do
	ol <- getOutline
	a <- getSel >>= fixAddr ol
	gendom (olDom a ol) >>= setPage
	byClass "unselected" >>= iter (\e -> onClick e (select e))

main = do
	initVars
	setSel $ Addr[]
	setOutline olexample
	buildit
	setupKeys
