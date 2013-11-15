fayflags = -p --package fay-ref -O --sourcemap
gcflags = --compilation_level ADVANCED_OPTIMIZATIONS

all:V: o.CL o.Test o.Sanitize UI.js
clean:V:
	rm -f *.grin *.core *.o o.* *.hi *.html *.js *.mjs *.map

%.js: %.hs
	fay -s $fayflags $stem.hs | closure $gcflags >$stem.js

o.%: %.hs
	ghc -hidir tmp -odir tmp -main-is $stem.main $stem.hs -o o.$stem

o.Test: Util.hs OL.hs Edit.hs PrimEdit.hs
o.CL: Util.hs OL.hs Edit.hs PrimEdit.hs
UI.js: DOM.hs Edit.hs JS.hs OL.hs Sanitize.hs UI.hs Util.hs PrimEdit.hs
