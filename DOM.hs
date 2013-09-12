module DOM where
import Prelude
import OL
import Util

data DOM = Node String [(String,String)] (Maybe String) [DOM]

addrId a = "olnode" ++ addrshow a
idAddr s = if not(prefix "olnode" s) then error "bad id" else addrread(drop 6 s)

olDom :: Addr -> OL -> DOM
olDom selection tree = top tree where
	α = unols
	leaf a txt = Node "span" (attrs a) (Just txt) []
	attrs a = if a==selection
		then [("class","selected"),("id",addrId a)]
		else [("class","unselected"),("id",addrId a)]
	top (OL txt []) = Node "p" [] Nothing [leaf (Addr[]) (α txt)]
	top  (OL txt cs) = Node "p" [] Nothing [leaf (Addr[]) (α txt),
		Node "ul" [] Nothing (concat $ addrmap (Addr[]) r cs)]
	r a (OL txt []) = [Node "li" [] Nothing [leaf a (α txt)]]
	r a (OL txt cs) = [Node "li" [] Nothing [leaf a (α txt)],
		Node "ul" [] Nothing (concat $ addrmap a r cs)]
