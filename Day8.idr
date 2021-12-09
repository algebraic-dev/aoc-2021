module Day8

import System.File
import Data.String
import Data.Vect
import Data.List
import Data.String.Extra
import Data.SortedMap
import Decidable.Equality

Map : Type
Map = SortedMap Char (List Char)

insertInf : List Char -> List Char -> Map -> Map
insertInf [] vals m = m
insertInf (key :: xs) vals m with (lookup key m)
    _ | Just last = insertInf xs vals (insert key (intersect last vals) m)
    _ | Nothing   = insert key vals m

getProbs : List Char -> Maybe (List Char)
getProbs l with (length l)   
    _ | 2 = Just ['c', 'f']
    _ | 4 = Just ['b', 'c', 'd', 'f']
    _ | 3 = Just ['a', 'c', 'f']
    _ | 7 = Just ['a', 'b', 'c', 'd', 'e', 'f']
    _ | _ = Nothing

parseData : String -> (List (List String, List String))
parseData str =
        map bag (lines str)
    where 
        bag : String -> (List String, List String)
        bag str = 
            let wordBag = words str 
                fstDigits = take 10 wordBag 
                fourLast  = take 4 $ drop 11 wordBag
            in (fstDigits, fourLast)


firstPart : (List (List String, List String)) -> Nat  
firstPart = 
          length
        . foldl (++) []
        . map (filter (isJust . isRecognizableDigit) . snd) 
    where
        isJust : Maybe a -> Bool 
        isJust (Just a) = True 
        isJust _ = False 

        isRecognizableDigit : String -> Maybe Int
        isRecognizableDigit l with (length l)   
            _ | 2 = Just 1
            _ | 4 = Just 4
            _ | 3 = Just 7
            _ | 7 = Just 8
            _ | _ = Nothing

main : IO ()
main = do 
    Right content <- readFile "input.txt" | Left err => putStrLn "Cannot readFile"
    let n = parseData content
    putStrLn "First Part \{show $ firstPart n}"