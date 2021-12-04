module Day4

import System.File
import Data.String
import Data.List
import Data.Vect
import Debug.Trace
import Data.List1

import Decidable.Equality

record Board where 
    constructor MkBoard
    info   : Vect 5 (Vect 5 Int)
    played : Vect 5 (Vect 5 Bool)

vectForget : Vect n a -> List a 
vectForget [] = []
vectForget (x :: xs) = x :: (vectForget xs)

Show Board where 
    show (MkBoard info played) = 
          unlines 
        $ vectForget
        $ map (unwords . vectForget . map (\(n, m) => if m then "\x1b[1m\{show n}\x1b[0m" else show n))
        $ map (uncurry zip) (zip info played)

-- Bool helpers

all : (b -> Bool) -> Vect a b -> Bool 
all val ls = foldl (\acc, n => acc && val n) True ls 

any : (b -> Bool) -> Vect a b -> Bool 
any val ls = foldl (\acc, n => acc || val n) False ls 

-- Vect operations 

updateAt2 : (Fin m) -> Fin n -> e -> Vect m (Vect n e) -> Vect m (Vect n e)
updateAt2 x y el v = updateAt x (updateAt y (\a => el)) v

updateBoard : Fin 5 -> Fin 5 -> Board -> Board 
updateBoard x y board@(MkBoard _ played) = record { played = updateAt2 x y True played } board

doubleIndex : Fin m -> Fin n -> Vect m (Vect n e) -> e
doubleIndex x y = Vect.index y . Vect.index x

findIndexMult : (e -> Bool) -> Vect m (Vect n e) -> Maybe (Fin m, Fin n)
findIndexMult p []        = Nothing
findIndexMult p (x :: xs) with (findIndex p x)
    _ | Just r  = Just (FZ, r) 
    _ | Nothing = (\(a,b) => (FS a, b)) <$> findIndexMult p xs

-- Board operations 

makeBoard : Vect 5 (Vect 5 Int) -> Board 
makeBoard info = MkBoard info (replicate 5 (replicate 5 False))
            
checkBoard : Board -> Bool
checkBoard (MkBoard info played) = Day4.any (Day4.all id) played || Day4.any (Day4.all id) (transpose played)

play : Int -> Board -> Board 
play num board@(MkBoard info played) = 
    maybe board (\pos => updateBoard (fst pos) (snd pos) board) 
                (findIndexMult (== num) info)

getNotSelected : Board -> List Int
getNotSelected (MkBoard info played) = 
        map (\(x, y) => doubleIndex x y info) (indices played)
    where 
        indices : Vect 5 (Vect 5 Bool) -> List (Fin 5, Fin 5) 
        indices played = foldl (\acc, (x, ls) => acc ++ map (x, ) ls) []
                       $ zip (Vect.fromList $ forget $ allFins 4) 
                             (map (elemIndices False) played) 

-- Process the input data

transVec : (n : Nat) -> (m: List a) -> Maybe (Vect n a)
transVec n ls with (decEq (length ls) n) 
    _ | Yes prf   = rewrite (sym prf) in Just (Vect.fromList ls)
    _ | No contra = Nothing

processData : List String -> Maybe (List Int, List Board)
processData                      [] = Nothing
processData (rawInput :: rawBoards) = 
        let input  = forget $ cast <$> split (== ',') rawInput
            boards =  traverse mkBoard 
                   $  (map (map cast . words) . forget)
                  <$> filter isEmptyList (groupBy isNotEmpty rawBoards)
        in (input, ) <$> boards  
    where 
        isEmptyList : List1 String -> Bool
        isEmptyList ls = forget ls /= [""] 

        mkBoard : List (List Int) -> Maybe Board 
        mkBoard ls = makeBoard <$> (traverse (transVec 5) ls >>= transVec 5) 

        isNotEmpty : String -> String -> Bool 
        isNotEmpty a b = a /= "" && b /= ""

-- Part one

partOne : List Int -> List Board -> Int 
partOne []        ls = 0
partOne (x :: xs) ls = 
    let boards = map (play x) ls in 
    case foldr (\c => (<+> (ifThenElse (checkBoard c) (Just c) Nothing))) Nothing boards of 
        Just board => x * sum (getNotSelected board)
        Nothing    => partOne xs boards

deleteBoard : List Board -> Maybe (Board, List Board) 
deleteBoard [] = Nothing
deleteBoard (x :: xs) with (checkBoard x)
    _ | True  = Just (x, xs) <+> ((\(a, n) => (a, x :: xs)) <$> deleteBoard xs)
    _ | False = (\(a, n) => (a, x :: xs)) <$> deleteBoard xs


lastToWin : List Int -> Maybe (Board, Int) -> List Board -> Maybe (Board, Int) 
lastToWin [] w@(Just (we,_)) ls = trace ("RES:\n" ++ show we) w
lastToWin []          winner ls = winner
lastToWin (x :: xs)   winner ls = 
        let boards     = map (play x) ls in
        case (List.partition checkBoard boards) of 
            (w :: ws, boards) => lastToWin xs (Just (w, x)) boards
            ([],      bounds) => lastToWin xs winner boards
    where
        inv : (List a, b) -> (List a, b)
        inv (a, b) = (reverse a, b)

partTwo : List Int -> List Board ->  Int 
partTwo xs ls with (lastToWin xs Nothing ls)
    _ | Just (board, x) = x * sum (getNotSelected board)
    _ | Nothing         = 0

main : IO ()
main = do 
    Right content <- readFile "input.txt" | Left err => putStrLn "Cannot readFile"
    let Just (input, boards) = processData (lines content) | Nothing => putStrLn "Malformed input"
    putStrLn "PartOne : Result \{show $ partOne input boards}"
    putStrLn "PartOne : Result \{show $ partTwo input boards}"
    pure ()