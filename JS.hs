module JS where
import Prelude
import FFI

data JS
data JSCharCode
data JSDOM

appendChild :: JSDOM -> JSDOM -> Fay()
appendChild = ffi "((%1).appendChild(%2))"
gid :: String -> Fay(Nullable JSDOM)
gid = ffi "(document.getElementById(%1))"
mknode :: String -> Fay JSDOM
mknode = ffi "(document.createElement(%1))"
setText :: JSDOM -> String -> Fay()
setText = ffi "((%1).innerText = %2)"
getText :: JSDOM -> Fay String
getText = ffi "((%1).innerText)"
alert :: String -> Fay()
alert = ffi "(alert(%1))"
alertI :: Int -> Fay()
alertI = ffi "(alert(%1))"
alertN :: JSDOM -> Fay()
alertN = ffi "(alert(%1))"
setAttr :: JSDOM -> String -> String -> Fay()
setAttr = ffi "((%1).setAttribute(%2,%3))"
toString :: [JSDOM] -> Fay String
toString = ffi "((%1).toString)"
getChilds :: JSDOM -> Fay[JSDOM]
getChilds = ffi "((%1).children)"
nodeName :: JSDOM -> Fay String
nodeName = ffi "((%1).nodeName)"
removeChild :: JSDOM -> JSDOM -> Fay()
removeChild = ffi "((%1).removeChild(%2))"
prompt :: String -> String -> Fay(Nullable String)
prompt = ffi "(prompt(%1,%2))"
getId :: JSDOM -> Fay(Nullable String)
getId = ffi "((%1).id)"
onClick :: JSDOM -> (Fay()) -> Fay()
onClick = ffi "((%1).onclick = (%2))"
byClass :: String -> Fay [JSDOM]
byClass = ffi "(document.getElementsByClassName(%1))"
onKeyPress' :: (JS -> Fay()) -> Fay()
onKeyPress' = ffi "document.onkeypress = (%1)"
keyCode :: JS -> JSCharCode
keyCode = ffi "((%1).keyCode)"
fromCharCode :: JSCharCode -> String
fromCharCode = ffi "(String.fromCharCode(%1))"
onKeyPress :: (String -> Fay()) -> Fay()
onKeyPress p = onKeyPress' (p.fromCharCode.keyCode)
