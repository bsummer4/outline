{-# LANGUAGE UnicodeSyntax #-}

module Test where
import Test.QuickCheck
import Data.List
import OL
import Prelude.Unicode

main = quickCheck ((\i is→i≡head(i:is))∷Int→[Int]→Bool)
