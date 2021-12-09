module Day7

import System.File
import Data.String
import Data.List1 as L1
import Data.List
import Data.Fin

partOne : List Int -> Int 
partOne ls = let s = sort ls in
    case natToFin (length s `div` 2) (length s) of 
        Just idx => 
            let pos = index' s idx in 
            foldl (+) 0 $ map (\s => abs $ s - pos) ls
        Nothing  => 0

-- Brute force it lol
partTwo : List Int -> Int 
partTwo ls = 
        snd $ consume [0..(foldr max 0 ls)] (0, 1000000000000)
    where 
        triangular : Int -> Int 
        triangular n = (n*(n+1)) `div` 2

        consume : List Int -> (Int, Int) -> (Int, Int)
        consume []        (pos, max) = (pos, max)
        consume (x :: xs) (pos, max) = 
            let res = foldl (+) 0 $ map (\s => triangular $ abs $ s - x) ls
            in if res < max
                    then consume xs (x, res) 
                    else consume xs (pos, max)

main : IO ()
main = do 
    Right content <- readFile "input.txt" | Left err => putStrLn "Cannot readFile"
    let r : List Int = cast <$> (L1.forget $ String.split (== ',') content)
    putStrLn "PartOne \{show $ partOne r}"
    putStrLn "PartTwo \{show $ partTwo r}"