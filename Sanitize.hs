-- TODO Add flags to control vertical whitespace.
	-- -v none => No blank lines.
	-- -v auto => Blank line only after a DEDENT back to 0.
	-- -v one => Drop multiple consecutive blank lines.
	-- -v keep => Maintain blank lines.

module Sanitize(sanitize,main) where
import Data.Char (isSpace)
import Data.List (find)

main = getContents >>= putStr.sanitize
sanitize = unlex . vblanks AUTO . tolex
times i e = take i(repeat e)
tabw n = 8 * (n `div` 8)
trimEnd = reverse . dropWhile isSpace . reverse
data Veritcal = NONE | AUTO | ONE | KEEP
data Lex = INDENT | DEDENT | LINE String
tolex = b . foldl lexLine ([0],[]) . map unSpace . lines where b(_,r)=r
unSpace l = cs 0 l where
	cs n (' ':s) = cs (1+n) s
	cs n ('\t':s) = cs (tabw$8+n) s
	cs n s = (n,trimEnd s)

prettyBlanks ll = reverse $ snd $ foldl r (0,[]) ll where
	r (n,a) l@INDENT = (n+1,l:a)
	r (1,a) l@DEDENT = (0,l:LINE "":a)
	r (n,a) l@DEDENT = (n-1,l:a)
	r (n,a) l = (n,l:a)

vblanks NONE ll = filter p ll where p x = case x of {LINE ""->False; _->True}
vblanks ONE [] = []
vblanks ONE (LINE "":LINE "":ll) = vblanks ONE (LINE "":ll)
vblanks ONE (l:ll) = l:vblanks ONE ll
vblanks KEEP ll = ll
vblanks AUTO ll = prettyBlanks $ vblanks NONE ll

unlex = showit . foldl r (0,[]) where
	r (i,acc) INDENT = (i+1,acc)
	r (i,acc) DEDENT = (i-1,acc)
	r (i,acc) (LINE "") = (i,"":acc)
	r (i,acc) (LINE s) = (i,(times i '\t' ++ s):acc)
	showit (i,ls) = unlines $ reverse ls

lexLine ([],acc) (n,l) = ([n],acc ++ [LINE l])
lexLine (d:depths,acc) (n,l) = case compare n d of
	GT -> (n:d:depths,acc++[INDENT,LINE l])
	LT -> lexLine (depths,acc ++ [DEDENT]) (n,l)
	EQ -> (d:depths,acc++[LINE l])
