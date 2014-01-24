module Main(main) where
import Sanitize
import Prelude.Unicode

main :: IO()
main = getContents >>= putStr∘sanitize KEEP
