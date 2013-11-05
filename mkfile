all:V: UI.html o.CL
clean:V:
	rm -f *.grin *.core *.o o.* *.hi *.html *.js *.mjs *.map

o.%: %.hs
	ghc -hidir tmp -odir tmp -main-is $stem.main $stem.hs -o o.$stem

o.Test: Test.hs Util.hs OL.hs DOM.hs Edit.hs
o.CL: CL.hs Util.hs OL.hs DOM.hs Edit.hs
