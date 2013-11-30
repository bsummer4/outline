fayflags = --Wall --package fay-ref -O --sourcemap

all:V: o.Sanitize UI.js
clean:V:
	rm -f *.grin *.core *.o o.* *.hi *.js *.mjs *.map

%.js: %.hs
	fay $fayflags $stem.hs -o $stem.js

o.%: %.hs
	ghc -Wall -XCPP -hidir tmp -odir tmp -main-is $stem.main $stem.hs -o o.$stem

UI.js: Editor.hs Outline.hs Sanitize.hs UI.hs Util.hs Edit.hs
