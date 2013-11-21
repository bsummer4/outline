module UI(main) where
import Prelude hiding (intersperse)
import FFI
import Util
import OL
import Edit
import Editor
import FayRef

-- FFI ------------------------------------------------------------------------
data JS
data JSCharCode
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
onKeyPress' :: (JS -> Fay()) -> Fay()
onKeyPress' = ffi "document.onkeypress = (%1)"
keyCode :: JS -> JSCharCode
keyCode = ffi "((%1).keyCode)"
fromCharCode :: JSCharCode -> String
fromCharCode = ffi "(String.fromCharCode(%1))"
onKeyPress :: (String -> Fay()) -> Fay()
onKeyPress p = onKeyPress' (p.fromCharCode.keyCode)
appendChild :: JSDOM -> JSDOM -> Fay()
appendChild = ffi "((%1).appendChild(%2))"

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
dLI = (dom "li")
dPRE = (dom "pre")

olDom :: FayRef State -> Addr -> OL -> DOM
olDom st selection tree = top tree where
	l a txt = dSPAN{attrs=[cls a],text=Just(unols txt),click=Just(select st a)}
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

select :: FayRef State -> Addr -> JSDOM -> Fay()
select st a _ = modifyFayRef st (\(State _ o) -> State a o) >> buildit st

editKey :: String -> String -> Fay Mut
editKey t k = do
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
		"r" -> prompt' "Replace Text" t >>= return . ReplaceTxt . ols
		"\r" -> prompt' "Replace Text" t >>= return . ReplaceTxt . ols
		_ -> return Nada

setupKeys :: (FayRef State) -> Fay()
setupKeys st = onKeyPress r where
	dumpText t = gendom payload >>= writePage where
		payload = dPRE{ text=Just $ t++"\n" }
	r "!" = readFayRef st >>= (\(State _ ol) -> dumpText$olshow$ol)
	r k = do
		s <- readFayRef st
		op <- editKey "TODO Get the currently selected text" k
		writeFayRef st $ apply op s
		buildit st

fixAddr :: (FayRef State) -> Fay()
fixAddr st = readFayRef st >>= \(State addr outline) ->
	if validSel addr outline then return() else
		modifyFayRef st (\(State _ o) -> State (Addr[]) o)

buildit :: (FayRef State) -> Fay()
buildit st = do
	fixAddr st
	State a ol <- readFayRef st
	gendom(olDom st a ol) >>= setPage

main :: Fay()
main = do
	state <- newFayRef $ State (Addr[]) olexample
	buildit state
	setupKeys state
