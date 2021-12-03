module Day3 

import System.File
import Data.String
import Data.List
import Data.List1
import Data.Binary
import Data.Binary.Digit

-- Binary casting

Cast String Bin where cast = map (\n => if n == '1' then I else O) . reverse . unpack

Cast Bin Int     where cast = cast . toNat 
Cast Bin Integer where cast = cast . toNat 

Show Digit where 
    show I = "1"
    show O = "0"

Num Bin where 
    (+) = fromInteger .: (+) `on` cast
    (*) = fromInteger .: (*) `on` cast
    fromInteger n with (n <= 0)
        _ | True = []
        _ | False = (if n `mod` 2 == 1 then I else O) :: fromInteger (n `div` 2) 

Eq Digit where 
    I == I = True
    O == O = True
    _ == _ = False

-- Part one

compl : Bin -> Bin 
compl = map (\case I => O; O => I)

getFreq : Eq a => a -> List a -> Int 
getFreq a = cast . length . filter (== a) 


getMostFreqBin : List Bin -> Bin 
getMostFreqBin ls = let half : Double = (cast $ length ls) / 2 in
             (\freq => ifThenElse (cast freq >= half) I O)
         <$> getFreq I 
         <$> transpose ls

partOne : List Bin -> Int 
partOne ls = let gammaRate = getMostFreqBin ls
                 epsilon   = compl gammaRate in 
             cast $ gammaRate * epsilon


getIndex : Bin -> Nat -> Digit  
getIndex (x :: xs) Z     = x
getIndex (x :: xs) (S n) = getIndex xs n 
getIndex [] _            = O

getRating : (List Bin -> Bin) -> List Bin -> Nat -> Nat -> Bin 
getRating f [] pos        Z = 0
getRating f (x :: xs) pos Z = x
getRating f ls pos    (S n) = 
    let bit = getIndex (reverse $ f ls) pos
        ls' = filter (((==) bit) . (`getIndex` pos) . reverse) ls 
    in case ls' of 
        [] => 0
        (x :: []) => x 
        ls' => getRating f ls' (pos + 1) n 

oxigenRating : List Bin -> Bin
oxigenRating []           = 0
oxigenRating ls@(x :: xs) = getRating getMostFreqBin ls 0 (length x)

co2Scrubber : List Bin -> Bin
co2Scrubber []           = 0
co2Scrubber ls@(x :: xs) = getRating (compl . getMostFreqBin) ls 0 (length x)

partTwo : List Bin -> Int 
partTwo n = cast $ co2Scrubber n * oxigenRating n

main : IO ()
main = do 
    Right content <- readFile "input.txt" | Left err => putStrLn "Cannot readFile"
    let content : List Bin = (map cast $ lines content)
    putStrLn "Part one: \{show $ partOne content}"
    putStrLn "Part two: \{show $ partTwo content}"