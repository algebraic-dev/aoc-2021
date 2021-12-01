module Day1

import System.File
import Data.String
import Data.List

dataFormat : String -> List Int 
dataFormat = map cast . lines

partOne : List Int -> Nat 
partOne ls = length $ filter (== LT) 
                    $ zipWith compare ls (drop 1 ls)                    

sumWindow : Nat -> List Int -> List Int 
sumWindow window = map sum 
                 . filter ((== window) . length) 
                 . transpose . take window . tails 
        
partTwo : List Int -> Nat 
partTwo = partOne . sumWindow 3
        
main : IO ()
main = do 
    Right content <- readFile "input.txt" | Left err => putStrLn "Cannot readFile"
    printLn $ (dataFormat content)