fayflags = --package fay-ref -O --sourcemap

all:V: o.CL o.Test o.Sanitize UI.js
clean:V:
	rm -f *.grin *.core *.o o.* *.hi *.html *.js *.mjs *.map

%.js: %.hs
	fay $fayflags $stem.hs -o $stem.js

o.%: %.hs
	ghc -hidir tmp -odir tmp -main-is $stem.main $stem.hs -o o.$stem

o.Test: Util.hs OL.hs Edit.hs PrimEdit.hs
o.CL: Util.hs OL.hs Edit.hs PrimEdit.hs
UI.js: DOM.hs Edit.hs JS.hs OL.hs Sanitize.hs UI.hs Util.hs PrimEdit.hs
