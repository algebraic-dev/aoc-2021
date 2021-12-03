module Day2 

import System.File
import Data.String
import Data.List

data Direction = Forward Int | Down Int | Up Int

record Pos where 
    constructor MkPos
    x, y, aim : Int 

Rule : Type 
Rule = Int -> Pos -> Pos 

multiplyPos : Pos -> Int 
multiplyPos (MkPos x y _) = x * y

formatData : String -> Maybe (List Direction)
formatData content = traverse transform (words <$> lines content)
    where transform : List String -> Maybe Direction
          transform ["forward", n] = Just $ Forward (cast n)
          transform ["down"   , n] = Just $ Down (cast n)
          transform ["up"     , n] = Just $ Up (cast n)
          transform _              = Nothing 

processPos :  (Rule, Rule, Rule) -> List Direction -> Int 
processPos rule = multiplyPos . foldl (process rule) (MkPos 0 0 0) 
    where process : (Rule, Rule, Rule) -> Pos ->  Direction -> Pos
          process (fst, snd, third) pos = \case 
                    (Forward u) => fst u pos
                    (Up u)      => snd u pos
                    (Down u)    => third u pos

partOne : List Direction -> Int 
partOne = processPos ( \u => record {x $= (+ u)}
                     , \u => record {y $= (+ (-u))}
                     , \u => record {y $= (+ u)})

partTwo : List Direction -> Int 
partTwo = processPos ( \u, pos => record { x $= (+ u), y $= (+ (pos.aim * u))} pos
                     , \u      => record { aim $= (+ (-u)) }
                     , \u      => record { aim $= (+ u) })

main : IO ()
main = do 
    Right content <- readFile "input.txt" | Left err => putStrLn "Cannot readFile"
    let Just content = formatData content | Nothing => putStrLn "Invalid Input"
    putStrLn "Part One: \{show $ partOne content}"
    putStrLn "Part Two: \{show $ partTwo content}"
