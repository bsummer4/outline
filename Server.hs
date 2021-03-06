{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where
import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack,unpack)
import qualified Data.ByteString.Lazy as LBS
import Prelude.Unicode
import Editor
import GHC.Exts (fromString)
import qualified Data.String.Utils as StrUtils
import System.Directory (doesFileExist)
import Data.UnixTime (getUnixTime, utSeconds)
import Foreign.C.Types

io = liftIO

topurl = do
	(CTime utime) ← (io getUnixTime) >>= return∘utSeconds
	redirect $ pack $ "/t_" ++ show utime

main ∷ IO()
main = quickHttpServe $ ifTop topurl <|> route (
	[ ("__edit__/:editparam", getParam "editparam" >>= restAPI∘mkDoc)
	, ("__edit__/", (restAPI∘mkDoc) Nothing)
	, ("UI.js", sendJS)
	, (":path", getParam "path" >>= client∘mkDoc)
	])

data Doc = Doc String
mkDoc Nothing = mkDoc $ Just "scratch"
mkDoc (Just d) = Doc(unpack d)
toPath (Doc n) = "./doc/" ++ n

client ∷ Doc → Snap()
client (Doc docname) = do
	modifyResponse(setContentType "text/html")
	html <- liftIO $ readFile "test.html"
	writeBS $ pack $ StrUtils.replace "scratch" docname html

sendJS ∷ Snap()
sendJS = do
	modifyResponse(setContentType "text/javascript")
	sendFile "UI.js"

mkRestAPI ∷ Doc → [(Method,Doc→Snap())] → Snap()
mkRestAPI doc handlers = foldr r (writeBS "fak") handlers where
	r (m,snapFn) acc = method m (snapFn doc) <|> acc

restAPI ∷ Doc → Snap()
restAPI d = mkRestAPI d $
	[(GET,sendDoc), (PUT,recvDoc), (DELETE,delDoc), (POST,recvMsg)]

recvMsg = writeBS∘pack∘toPath
delDoc = writeBS∘pack∘toPath
recvDoc d = getRequestBody >>= liftIO∘(LBS.writeFile $ toPath d)
sendDoc d = do
	let path = toPath d
	modifyResponse(setContentType "text/plain")
	e <- liftIO(doesFileExist path)
	if e then sendFile path else writeBS "#\n"
