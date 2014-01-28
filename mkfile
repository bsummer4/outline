fayflags = --library --Wall --package fay-ref -p -O --sourcemap

all:V: o.unexpand2 UI.js

tidy:V:
	echo -n >log/access.log
	echo -n >log/error.log

clean:V: tidy
	rm -f *.grin *.core *.o o.* *.hi *.js *.mjs *.map

%.js: %.hs
	fay $fayflags $stem.hs -o $stem.js

o.%: %.hs
	ghc -O2 -Wall -hidir tmp -odir tmp $stem.hs -o o.$stem

o.unexpand2: Sanitize.hs
UI.js: Editor.hs Outline.hs Sanitize.hs UI.hs Util.hs Edit.hs
