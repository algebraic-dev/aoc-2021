module Day5 

import System.File
import Data.String
import Data.List1
import Data.List
import Data.String.Extra
import Data.Zippable

data Line = MkLine (Nat, Nat) (Nat, Nat)
data Kind = Straight | Diagonal | Other

Eq Kind where 
    Straight == Straight = True
    Diagonal == Diagonal = True
    Other    ==    Other = True
    _        ==        _ = False

-- Process data 

processData : String -> Maybe (List Line)
processData ls = 
        traverse processLine $ words <$> lines ls 
    where
        processLine : List String -> Maybe Line 
        processLine [x, _, y] = 
            let (i, j) = break (== ',') x 
                (n, m) = break (== ',') y 
            in Just $ MkLine (cast i, cast $ drop 1 j) (cast n, cast $ drop 1 m)
        processLine _ = Nothing

getLineKind : Line -> Kind 
getLineKind (MkLine (x, y) (x', y')) = 
        if x == x' || y == y'                then Straight
        else if absDiff x x' == absDiff y y' then Diagonal
                                             else Other
    where absDiff : Nat -> Nat -> Nat 
          absDiff x y = cast $ abs (the Int (cast x - cast y))

getPoints : Line -> List (Nat, Nat) 
getPoints line@(MkLine (x, y) (x', y')) with (getLineKind line, x == x')
    _ | (Straight, True)  = [(x, newY) | newY <- [y..y']] 
    _ | (Straight, False) = [(newX, y) | newX <- [x..x']]
    _ | (Diagonal, _)     = zipWith (\f, s => (fst f, snd s)) [(newX, y) | newX <- [x..x']] [(x, newY) | newY <- [y..y']] 
    _ | (Other,    _)     = []

getIntersectionCount : List Line -> Nat 
getIntersectionCount = 
            length
          . filter (> 1)
          . map length
          . groupBy (==)
          . sort 
          . foldl (++) []
          . map getPoints

firstPart : List Line -> Nat 
firstPart = getIntersectionCount . filter ((== Straight) . getLineKind)

sndPart : List Line -> Nat 
sndPart = getIntersectionCount 
        . filter ((\c => c == Straight || c == Diagonal) . getLineKind)

main : IO ()
main = do 
    Right content <- readFile "input.txt" | Left err => putStrLn "Cannot readFile"
    let Just r = processData content | Nothing => putStrLn "Cannot parse data"
    putStrLn "First Part: \{show $ firstPart r}"
    putStrLn "Second Part: \{show $ sndPart r}"