module UI where
import Prelude hiding (intersperse, getText)
import FFI
import JS
import Util
import OL
import DOM
import Edit
import FayRef

-- Manipulate the State Variable -----------------------------------------------
setSel st a = modifyFayRef st (\(State _ o) -> State a o)
getSel st = readFayRef st >>= (\(State a _) -> return a)
setOutline st o = modifyFayRef st (\(State a _) -> State a o)
getOutline st = readFayRef st >>= (\(State _ o) -> return o)
getState st = readFayRef st
setState st state = modifyFayRef st (\_ -> state)

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
prompt' q default' = do
	x <- prompt q default'
	case x of {Null->return default'; Nullable t->return t}

nodeAddr n = getId n >>= r where
	r Null = error "Internal Error: Clickable node has invalid ID"
	r (Nullable i) = return(idAddr i)

select st n = nodeAddr n >>= setSel st >> buildit st
editKey n k = do
	t <- getText n
	case k of
		"h" -> return SelLeft
		"l" -> return SelRight
		"k" -> return SelUp
		"j" -> return SelDown
		"d" -> return Delete
		"i" -> return $ InsBefore $ ols ""
		"a" -> return $ InsAfter $ ols ""
		"o" -> return $ InsBelow $ ols ""
		"O" -> return $ InsAbove $ ols ""
		"r" -> prompt' "Replace Text" t >>= return . Edit . ols
		"\r" -> prompt' "Replace Text" t >>= return . Edit . ols
		_ -> return Nada

setupKeys st = onKeyPress r where
	dumpText t = gendom payload >>= writePage where
		payload = Node "pre" [] (Just $ t++"\n") []
	r "!" = getState st >>= (\(State a ol) -> dumpText$olshow$ol)
	r k = do
		a <- getSel st
		s <- getState st
		n' <- gid $ addrId a
		n <- return $ case n' of {Null->error "bad addr"; Nullable z->z}
		op <- editKey n k
		setState st $ apply op s
		buildit st

fixAddr st = readFayRef st >>= \(State a o) ->
	if validSel a o then return() else
		modifyFayRef st (\(State a o) -> State (Addr[]) o)

buildit st = do
	fixAddr st
	State a ol <- readFayRef st
	gendom(olDom a ol) >>= setPage
	byClass "unselected" >>= iter (\e -> onClick e (select st e))

main = do
	state <- newFayRef $ State (Addr[]) olexample
	buildit state
	setupKeys state
