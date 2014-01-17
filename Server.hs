-- Okay, what do I want the webserver to do?
	-- When I hit the top-level page, I want the scratch page to be dumped.
	-- I want to accept editor messages at ‘/msg’
		-- These should change the scratch page.
	-- The scratch page needs to be persistant.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Server where
import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Control.Monad.IO.Class
import Data.ByteString.Lazy as LBS
import Prelude.Unicode
import Editor
import GHC.Exts (fromString)

main ∷ IO ()
main = quickHttpServe $ ifTop sendClient <|> r where
	r = route [("read/", sendDB), ("write/", recvDB), ("UI.js",sendJS)]
	sendDB = modifyResponse(setContentType "text/plain") >> sendFile db
	recvDB = getRequestBody >>= liftIO∘(LBS.writeFile db) >> writeBS "File written"
	sendClient = modifyResponse(setContentType "text/html") >> sendFile "test.html"
	sendJS = modifyResponse(setContentType "text/javascript") >> sendFile "UI.js"
	db = "./db"
