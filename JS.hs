-- TODO Go through all of the ffi calls
--  and make sure their type sigs handle error conditions.
-- TODO Disallow newlines and tabs in outline nodes.
-- TODO Serialize/deserialize the entire state object
-- TODO Store the state object inside a global JS string.
-- TODO Get things working in google Hangouts.

module JS where
-- import Prelude hiding (intersperse, getText)
import FFI

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
initVars = ffi "fayVars = {}" :: Fay()
setVar = ffi "fayVars[%1] = %2" :: String -> String -> Fay()
getVar' = ffi "(fayVars[%1])" :: String -> Fay (Defined String)
getVar k = getVar' k >>= (\o -> case o of
	{ Undefined -> error $ "unset variable ‘" ++ k ++ "’"
	; Defined v -> return v})
