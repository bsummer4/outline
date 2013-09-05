all:V: UI.js CL Test
clean:V: UI.js
	rm -f *.o Test CL *.hi

test:V: Test
	./Test

UI.js: UI.hs JS.hs Util.hs OL.hs DOM.hs Edit.hs
	fay -p UI.hs

Test: Test.hs Util.hs OL.hs DOM.hs Edit.hs
	ghc Test

CL: Util.hs OL.hs DOM.hs CL.hs Edit.hs
	ghc CL
