module Day1

import System.File
import Data.String
import Data.List

formatData : String -> List Int 
formatData = map cast . lines                

sumWindow : Nat -> List Int -> List Int 
sumWindow window = map sum 
                 . filter ((== window) . length) 
                 . transpose . take window . tails 
        
partOne : List Int -> Nat 
partOne ls = length $ filter (== LT) 
                    $ zipWith compare ls (drop 1 ls)    

partTwo : List Int -> Nat 
partTwo = partOne . sumWindow 3
        
main : IO ()
main = do 
    Right content <- readFile "input.txt" | Left err => putStrLn "Cannot readFile"
    let content = formatData content
    putStrLn "Part One: \{show $ partOne content}"
    putStrLn "Part Two: \{show $ partTwo content}"