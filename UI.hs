-- TODO Go through all of the ffi calls
--  and make sure their type sigs handle error conditions.
-- TODO Disallow newlines and tabs in outline nodes.
-- TODO Serialize/deserialize the entire state object
-- TODO Store the state object inside a mutable JS string.

module UI where
import Prelude hiding (intersperse, getText)
import FFI
import Lib

-- FFI -------------------------------------------------------------------------
data JS
data JSCharCode
data JSDOM

appendChild = ffi "((%1).appendChild(%2))":: JSDOM -> JSDOM -> Fay()
gid = ffi "(document.getElementById(%1))":: String -> Fay(Nullable JSDOM)
mknode = ffi "(document.createElement(%1))":: String -> Fay JSDOM
setText = ffi "((%1).innerText = %2)":: JSDOM -> String -> Fay()
getText = ffi "((%1).innerText)":: JSDOM -> Fay String
alert = ffi "(alert(%1))":: String -> Fay()
alertI = ffi "(alert(%1))":: Int -> Fay()
alertN = ffi "(alert(%1))":: JSDOM -> Fay()
setAttr = ffi "((%1).setAttribute(%2,%3))":: JSDOM -> String -> String -> Fay()
toString = ffi "((%1).toString)":: [JSDOM] -> Fay String
getChilds = ffi "((%1).children)":: JSDOM -> Fay[JSDOM]
nodeName = ffi "((%1).nodeName)":: JSDOM -> Fay String
removeChild = ffi "((%1).removeChild(%2))":: JSDOM -> JSDOM -> Fay()
prompt = ffi "(prompt(%1,%2))":: String -> String -> Fay(Nullable String)
getId = ffi "((%1).id)":: JSDOM -> Fay(Nullable String)
onClick = ffi "((%1).onclick = (%2))":: JSDOM -> (Fay()) -> Fay()
byClass = ffi "(document.getElementsByClassName(%1))":: String -> Fay [JSDOM]
onKeyPress' = ffi "document.onkeypress = (%1)" :: (JS -> Fay()) -> Fay()
keyCode = ffi "((%1).keyCode)" :: JS -> JSCharCode
fromCharCode = ffi "(String.fromCharCode(%1))" :: JSCharCode -> String
onKeyPress :: (String -> Fay()) -> Fay()
onKeyPress p = onKeyPress' (p.fromCharCode.keyCode)

-- DOM -------------------------------------------------------------------------
setAttrs n [] = return ()
setAttrs n ((k,v):as) = setAttr n k v >> setAttrs n as
appendChilds n cs = mapM (appendChild n) cs >> return()
getOutlineNode = gid "outline" >>= (\ol -> case ol of
	Null -> error "There has to be a node with id=“outline” in the document."
	Nullable node -> return node)

clearPage = do {o<-getOutlineNode; getChilds o >>= mapM (removeChild o)}
writePage d = getOutlineNode >>= (\o -> appendChild o d)
setPage xs = clearPage >> mapM writePage xs
gendom (Node tag attrs txt childs) = do
	n <- mknode tag
	setAttrs n attrs
	case txt of {Nothing->return (); Just s->setText n s}
	mapM gendom childs >>= appendChilds n
	return n

walkdom p n = r n [] where
	r n addr = do
		p addr n
		cs <- getChilds n
		sequence $ zipWith (\i c -> r c (i:addr)) [0,1..] cs
		return()

-- Main ------------------------------------------------------------------------
texteditor = do
	addr <- getSelAddr
	n <- gid(addrToId addr)
	n <- return $ case n of
		Null -> error "Invalid selection"
		Nullable n -> n
	txt <- getText n
	p <- prompt "New text" txt
	case p of {Null->texteditor; Nullable t->setText n t}

x t = Node "p" [("align","center")] (Just t) []
addrToId a = concat $ intersperse "," $ "olnode":(map show a)
hi s = case readI s of
	Just i -> i
	Nothing -> error ("Bad Int: " ++ s)

idToAddr :: String -> [Int]
idToAddr s = case (split s) of
	"olnode":is -> map hi is
	_ -> error "Invalid address encoded in a node's id"

addrmap a f l = mapi (\i e -> f (i:a) e) l
outline selection tree = top tree where
	leaf a txt = Node "span" (attrs a) (Just txt) []
	attrs a = if a==selection
		then [("class","selected"),("id",addrToId a)]
		else [("class","unselected"),("id",addrToId a)]
	top (OLLeaf txt) = Node "p" [] Nothing [leaf [] txt]
	top  (OLTree txt cs) = Node "p" [] Nothing [leaf [] txt,
		Node "ul" [] Nothing (concat(addrmap [] r cs))]
	r a (OLLeaf txt) = [Node "li" [] Nothing [leaf a txt]]
	r a (OLTree txt cs) = [Node "li" [] Nothing [leaf a txt],
		Node "ul" [] Nothing (concat $ addrmap a r cs)]

olj = OLTree "h" [OLLeaf "i", OLLeaf "j", OLTree "k"
	[OLLeaf "hihihi there", OLTree "w" [OLLeaf "t", OLLeaf "f"]]]

select n = do
	id' <- getId n
	id <- return $ case id' of
		Null->error "Was expecting an ID, but didn't find one"
		Nullable i -> i
	addr <- return (idToAddr id)
	selAddr addr
	d <- gendom $ outline addr olj
	setPage [d]
	setupClicks
	return()

setupclick e = onClick e $ select e
setupClicks = byClass "unselected" >>= iter setupclick
foo = byClass "unselected" >>= iter setupclick

selAddr = ffi "selAddr = (%1)" :: [Int] -> Fay()
getSelAddr = ffi "(selAddr)" :: Fay [Int]
buildit = do
	a <- getSelAddr
	gendom(outline a olj) >>= setPage.one
	byClass "unselected" >>= iter setupclick

mover :: ([Int] -> [Int]) -> Fay()
mover p = do
	a <- getSelAddr
	b <- return $ p a
	n <- gid $ addrToId b
	case n of
		Null -> return()
		Nullable n -> do
			selAddr b >> buildit

main = do
	selAddr []
	gendom(outline [] olj) >>= setPage.one
	byClass "unselected" >>= iter setupclick
	txt <- return $ showOutline olj
	gendom (Node "pre" [] (Just txt) []) >>= writePage
	gendom (outline [] (parse txt)) >>= writePage
	onKeyPress $ \s -> case s of
		"j" -> mover $ \a -> case a of {[]->[]; (b:bs)->(b+1):bs}
		"h" -> mover $ \a -> case a of {[]->[]; (b:bs)->bs}
		"l" -> mover $ \a -> 0:a
		"k" -> mover $ \a -> case a of {[]->[]; (b:bs)->(b-1):bs}
		"\r" -> texteditor
		_ -> return()
