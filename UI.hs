-- TODO Go through all of the ffi calls
--  and make sure their type sigs handle error conditions.
-- TODO Disallow newlines and tabs in outline nodes.
-- TODO Serialize/deserialize the entire state object
-- TODO Store the state object inside a global JS string.
-- TODO Get things working in google Hangouts.

module UI where
import Prelude hiding (intersperse, getText)
import FFI
import JS
import Util
import OL
import DOM

-- DOM -------------------------------------------------------------------------
setAttrs n [] = return ()
setAttrs n ((k,v):as) = setAttr n k v >> setAttrs n as
appendChilds n cs = iter (appendChild n) cs
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
	mymapM gendom childs >>= appendChilds n
	return n

-- Main ------------------------------------------------------------------------
replace :: Addr -> String -> OL -> OL
replace i s ol = olreplace i s ol

chText :: Addr -> String -> Fay()
chText addr txt = do
	ol <- getOutline
	alert $ olshow ol
	setOutline $ replace addr txt ol
	alert(olshow ol)
	getVar "ol" >>= alert
	buildit

texteditor = do
	addr <- getSel
	n <- gid(addrId addr)
	n <- return $ case n of
		Null -> error "Invalid selection"
		Nullable n -> n
	txt <- getText n
	p <- prompt "New text" txt
	case p of {Null->texteditor; Nullable t->chText addr t}

select n = do
	id' <- getId n
	id <- return $ case id' of
		Null->error "Was expecting an ID, but didn't find one"
		Nullable i -> i
	addr <- return (idAddr id)
	setSel addr
	d <- gendom $ olDom addr olexample
	setPage d
	setupClicks
	return()

-- Mutable Variables
setSel :: Addr -> Fay()
setSel (Addr is) = setVar "addr" $ concat $ myintersperse "," $ map show is
getSel :: Fay Addr
getSel = getVar "addr" >>= return.addrread
setOutline :: OL -> Fay()
setOutline o = setVar "ol" $ olshow o
getOutline :: Fay OL
getOutline = getVar "ol" >>= return.(olread)

mover p = do
	a <- getSel
	b <- return $ p a
	n <- gid $ addrId b
	case n of
		Null -> return()
		Nullable n -> do
			setSel b >> buildit

setupclick e = onClick e $ select e
setupClicks = byClass "unselected" >>= iter setupclick
setupKeys = onKeyPress $ \s -> case s of
	"j" -> mover $ \(Addr a) -> case a of {[]->Addr[]; b:bs->Addr((b+1):bs)}
	"h" -> mover $ \(Addr a) -> case a of {[]->Addr[]; b:bs->Addr(bs)}
	"l" -> mover $ \(Addr a) -> Addr(0:a)
	"k" -> mover $ \(Addr a) -> case a of {[]->Addr[]; b:bs->Addr((b-1):bs)}
	"\r" -> texteditor
	_ -> return()

buildit = do
	a <- getSel
	ol <- getOutline
	gendom (olDom a ol) >>= setPage
	byClass "unselected" >>= iter setupclick

main = do
	initVars
	setSel (Addr[])
	setOutline olexample
	buildit
	setupKeys
