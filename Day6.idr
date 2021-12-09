module Day6 

import System.File
import Data.String
import Data.List1 as L1
import Data.List
import Data.Vect
import Data.Fin
import Debug.Trace

rotate : Vect 9 Int -> Vect 9 Int 
rotate (x :: ls) = updateAt 6 (\s => s + x) ls ++ [x]

genFreqByNums : List Nat -> Vect 9 Int -> Vect 9 Int 
genFreqByNums []        l = l
genFreqByNums (x :: xs) l with (natToFin x 9)
    _ | Just n  = genFreqByNums xs (updateAt n (+ 1) l)
    _ | Nothing = l

runTimes : Nat -> (a -> a) -> a -> a
runTimes Z     f l = l 
runTimes (S n) f l = runTimes n f (f l)

partOne : List Nat -> Int 
partOne = foldl (+) 0 
        . runTimes 80 rotate 
        . flip genFreqByNums (replicate 9 0)

partTwo : List Nat -> Int 
partTwo = foldl (+) 0 
        . runTimes 256 rotate 
        . flip genFreqByNums (replicate 9 0)

main : IO ()
main = do 
    Right content <- readFile "input.txt" | Left err => putStrLn "Cannot readFile"
    let r = cast <$> (L1.forget $ String.split (== ',') content)
    printLn (partOne r)
    printLn (partTwo r)