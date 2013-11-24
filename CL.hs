{-# LANGUAGE UnicodeSyntax #-}

module CL(main) where
import Outline
import Edit
import Editor
import System.IO

red ∷ String → String
red s = "\ESC[1;31m" ++ s ++ "\ESC[0m"

pretty ∷ State → String
pretty s = olshow $ stOL $ case olget (stSel s) (stOL s) of
	Nothing -> s
	Just sel -> apply (ReplaceTxt $ ols $ red $ oltext $ sel) s

writeScreen ∷ String → IO()
writeScreen s = putStr "\ESC[2J" >> putStr s

inputloop ∷ State → IO()
inputloop st = do
	writeScreen $ pretty st
	cmd <- getLine
	inputloop $ apply (parseCmd cmd) st

main ∷ IO()
main = inputloop $ editor (Addr[]) olexample

parseCmd ∷ String → Editor.Operation
parseCmd "h" = SelLeft
parseCmd "j" = SelDown
parseCmd "k" = SelUp
parseCmd "l" = SelRight
parseCmd "d" = Delete
parseCmd ('r':txt) = ReplaceTxt $ ols txt
parseCmd ('i':txt) = InsBefore $ ols txt
parseCmd ('a':txt) = InsAfter $ ols txt
parseCmd ('o':txt) = InsBelow $ ols txt
parseCmd ('O':txt) = InsAbove $ ols txt
parseCmd _ = Nada
