module UI(main) where
import Prelude hiding (intersperse)
import FFI
import Util
import Outline
import Editor
import FayRef
import Sanitize

-- DOM FFI --------------------------------------------------------------------
data JS
data JSDOM

gid :: String -> Fay(Nullable JSDOM)
gid = ffi "(document.getElementById(%1))"
mknode :: String -> Fay JSDOM
mknode = ffi "(document.createElement(%1))"
setText :: JSDOM -> String -> Fay()
setText = ffi "((%1).innerText = %2)"
setAttr :: JSDOM -> String -> String -> Fay()
setAttr = ffi "((%1).setAttribute(%2,%3))"
getChilds :: JSDOM -> Fay[JSDOM]
getChilds = ffi "((%1).children)"
removeChild :: JSDOM -> JSDOM -> Fay()
removeChild = ffi "((%1).removeChild(%2))"
prompt :: String -> String -> Fay(Nullable String)
prompt = ffi "(prompt(%1,%2))"
onClick :: JSDOM -> (Fay()) -> Fay()
onClick = ffi "((%1).onclick = (%2))"
onKeyDown' :: (JS -> Fay()) -> Fay()
onKeyDown' = ffi "document.onkeydown = (%1)"
onKeyPress' :: (JS -> Fay()) -> Fay()
onKeyPress' = ffi "document.onkeypress = (%1)"
keyCode :: JS -> Int
keyCode = ffi "((%1).keyCode)"
charCodeToString :: Int -> String
charCodeToString = ffi "(String.fromCharCode(%1))"
onKeyDown :: (Int -> Fay()) -> Fay()
onKeyDown p = onKeyDown' (p . keyCode)
onKeyPress :: (String -> Fay()) -> Fay()
onKeyPress p = onKeyPress' (p . charCodeToString . keyCode)
appendChild :: JSDOM -> JSDOM -> Fay()
appendChild = ffi "((%1).appendChild(%2))"

-- AJAX FFI -------------------------------------------------------------------
data Req
ajaxReq :: Fay Req
ajaxReq = ffi "new XMLHttpRequest()"
ajaxOpen :: Req -> String -> String -> Bool -> Fay()
ajaxOpen = ffi "(%1).open(%2,%3,%4)"
ajaxSend :: Req -> Fay()
ajaxSend = ffi "(%1).send()"
ajaxSendStr :: Req -> String -> Fay()
ajaxSendStr = ffi "(%1).send(%2)"
ajaxRecv :: Req -> Fay String
ajaxRecv = ffi "(%1).responseText"

-- DOM ------------------------------------------------------------------------
data DOM = Node
	{ tag :: String
	, attrs :: [(String,String)]
	, text :: Maybe String
	, click :: Maybe(JSDOM -> Fay())
	, childs :: [DOM] }

dom :: String -> DOM
dom s = Node {tag=s, attrs=[], text=Nothing, click=Nothing, childs=[]}

-- TODO There is a bug in Fay that breaks the following line.
-- (dP,dSPAN,dUL,dLI,dPRE)=(dom "p", dom "span", dom "ul", dom "li", dom "pre")
dP = (dom "p")
dSPAN = (dom "span")
dUL = (dom "ul")
dDIV = (dom "div")
dLI = (dom "li")
dPRE = (dom "pre")
dText s = dSPAN{text=Just s}

data Vars = Vars (Editor,RenderMethod)
data RenderMethod = ByText | ByList

render :: RenderMethod -> FayRef Vars -> Addr -> Outline -> DOM
render ByText = olTextDom
render ByList = olListDom

otherMeth :: RenderMethod -> RenderMethod
otherMeth ByText = ByList
otherMeth ByList = ByText

-- The ‘eol’ bit makes selecting the whole outline to copy elsewhere
-- work better. It shouldn't change how anything looks.
olTextDom :: FayRef Vars -> Addr -> Outline -> DOM
olTextDom vars selection tree = top tree where
	l a txt = fmt a $
		dSPAN{attrs=[cls a],text=Just(unols txt++"\n"),click=Just(select vars a)}
	cls a = ("class",if a==selection then "selected" else "unselected")
	fmt (Addr a) t = dDIV{childs= [dText$take(2*length a)$repeat ' ', t]}
	r a (OL txt cs) = l a txt:concat(addrmap a r cs)
	top (OL txt cs) = dPRE{childs=(l(Addr[])txt:concat(addrmap(Addr[])r cs))++eol}
	eol = [dText "\n"]

olListDom :: FayRef Vars -> Addr -> Outline -> DOM
olListDom vars selection tree = top tree where
	l a txt = dSPAN{attrs=[cls a],text=Just(unols txt),click=Just(select vars a)}
	cls a = ("class",if a==selection then "selected" else "unselected")
	r a (OL txt []) = [ dLI{childs=[l a txt]} ]
	r a (OL txt cs) = [ dLI{childs=[l a txt]}, dUL{childs=concat$addrmap a r cs} ]
	top (OL txt []) = dP{childs=[l (Addr[]) txt]}
	top (OL txt cs) = dP{childs=
		[ l (Addr[]) txt
		, dUL{childs=concat$addrmap (Addr[]) r cs} ]}

setAttrs :: JSDOM -> [(String,String)] -> Fay()
setAttrs _ [] = return ()
setAttrs n ((k,v):as) = setAttr n k v >> setAttrs n as

getOutlineNode :: Fay JSDOM
getOutlineNode = gid "outline" >>= (\ol -> case ol of
	Null -> error "There has to be a node with id=“outline” in the document."
	Nullable node -> return node)

clearPage :: Fay()
clearPage = do {o<-getOutlineNode; getChilds o >>= iter (removeChild o)}

writePage :: JSDOM -> Fay()
writePage d = getOutlineNode >>= (\o -> appendChild o d)

setPage :: JSDOM -> Fay()
setPage x = clearPage >> writePage x

gendom :: DOM -> Fay JSDOM
gendom d = do
	n <- mknode (tag d)
	setAttrs n (attrs d)
	case (text d) of {Nothing->return (); Just s->setText n s}
	mymapM gendom (childs d) >>= iter (appendChild n)
	case (click d) of {Nothing->return(); Just c->onClick n (c n)}
	return n

-- Main -----------------------------------------------------------------------
prompt' :: String -> String -> Fay String
prompt' q default' = do
	x <- prompt q default'
	case x of {Null->return default'; Nullable t->return t}

select :: FayRef Vars -> Addr -> JSDOM -> Fay()
select vars a _ = modifyFayRef vars f >> buildit vars where
	f(Vars(e,r)) = Vars(apply (Select a) e, r)

editKey :: String -> String -> Fay Operation
editKey t k = do
	case k of
		"h" -> return SelLeft
		"l" -> return SelRight
		"k" -> return SelUp
		"j" -> return SelDown
		"d" -> return Delete
		"u" -> return Undo
		"i" -> return $ InsBefore $ ols ""
		"a" -> return $ InsAfter $ ols ""
		"o" -> return $ InsBelow $ ols ""
		"O" -> return $ InsAbove $ ols ""
		"y" -> return $ Copy
		"P" -> return $ PasteBefore
		"p" -> return $ PasteAfter
		"x" -> return $ Cut
		"r" -> prompt' "Replace Text" t >>= return . ReplaceTxt . ols
		"\r" -> prompt' "Replace Text" t >>= return . ReplaceTxt . ols
		_ -> return Nada

setupKeys :: FayRef Vars -> Fay()
setupKeys vars = onKeyPress kpress >> onKeyDown kdown where
	kpress "!" = modifyFayRef vars (\(Vars(e,r)) -> Vars(e,otherMeth r)) >> buildit vars
	kpress k = do
		Vars(s,r) <- readFayRef vars
		case mmap oltext $ olget (stSel s) (stOL s) of
			Nothing -> return()
			Just e -> do
				op <- editKey e k
				writeFayRef vars (Vars(apply op s,r)) >> buildit vars
	kdown 37 = kpress "h"
	kdown 38 = kpress "k"
	kdown 39 = kpress "l"
	kdown 40 = kpress "j"
	kdown _ = return()

editingWhichFile :: Fay String
editingWhichFile = ffi "editingWhichFile"

buildit :: FayRef Vars -> Fay()
buildit vars = do
	Vars (editor,method) <- readFayRef vars
	gendom(render method vars (stSel editor) (stOL editor)) >>= setPage
	sendUpdate $ stOL editor

sendUpdate :: Outline -> Fay ()
sendUpdate ol = do
	filename <- editingWhichFile
	r <- ajaxReq
	ajaxOpen r "PUT" ("/__edit__/" ++ filename) True
	ajaxSendStr r $ olshow ol

getOutline :: Fay Outline
getOutline = do
	filename <- editingWhichFile
	r <- ajaxReq
	ajaxOpen r "GET" ("/__edit__/" ++ filename) False
	ajaxSend r
	str <- ajaxRecv r
	return $ olread str

main :: Fay()
main = do
	ol <- getOutline
	vars <- newFayRef $ Vars (mkeditor (Addr[]) ol, ByList)
	buildit vars
	setupKeys vars
