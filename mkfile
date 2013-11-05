all:V: o.CL o.Test o.Sanitize
clean:V:
	rm -f *.grin *.core *.o o.* *.hi *.html *.js *.mjs *.map

o.%: %.hs
	ghc -hidir tmp -odir tmp -main-is $stem.main $stem.hs -o o.$stem

o.Test: Util.hs OL.hs Edit.hs
o.CL: Util.hs OL.hs Edit.hs
