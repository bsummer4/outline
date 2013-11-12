module Test where
import Test.QuickCheck
import Data.List
import OL

main = quickCheck ((\i is -> i==head(i:is)) :: Int -> [Int] -> Bool)
