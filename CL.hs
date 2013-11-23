{-# LANGUAGE UnicodeSyntax #-}

module CL(main) where
import OL
import Edit
import Editor
import System.IO

red ∷ String → String
red s = "\ESC[1;31m" ++ s ++ "\ESC[0m"

pretty ∷ State → String
pretty s@(State a ol _) = olshow ol' where
	(State _ ol' undoOps) = apply (ReplaceTxt $ ols $ red $ unols $ getNode s) s

writeScreen ∷ String → IO()
writeScreen s = putStr "\ESC[2J" >> putStr s

inputloop ∷ State → IO()
inputloop st@(State a ol _) = do
	writeScreen $ pretty st
	cmd <- getLine
	inputloop $ apply (parseCmd cmd) st

main ∷ IO()
main = inputloop (State (Addr[]) olexample [])

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
