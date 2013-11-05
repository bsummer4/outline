module CL(main) where
import OL
import Edit
import Prelude

main = putStrLn $ showState $ apply SelRight $ State (Addr[]) olexample where
	showState (State _ ol) = olshow ol

