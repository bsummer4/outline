module DOM where
import Prelude
import OL
import Util

data DOM = Node String [(String,String)] (Maybe String) [DOM]

addrId a = "olnode" ++ addrshow a
idAddr s = if not(prefix "olnode" s) then error "bad id" else addrread(drop 6 s)

olDom :: Addr -> OL -> DOM
olDom selection tree = top tree where
	leaf a txt = Node "span" (attrs a) (Just txt) []
	attrs a = if a==selection
		then [("class","selected"),("id",addrId a)]
		else [("class","unselected"),("id",addrId a)]
	top (OLLeaf txt) = Node "p" [] Nothing [leaf (Addr[]) txt]
	top  (OLTree txt cs) = Node "p" [] Nothing [leaf (Addr[]) txt,
		Node "ul" [] Nothing (concat $ addrmap (Addr[]) r cs)]
	r a (OLLeaf txt) = [Node "li" [] Nothing [leaf a txt]]
	r a (OLTree txt cs) = [Node "li" [] Nothing [leaf a txt],
		Node "ul" [] Nothing (concat $ addrmap a r cs)]
