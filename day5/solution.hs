import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.Char

main = do
    ls <- fmap Text.lines (Text.readFile "input.txt")
    let input = map Text.unpack ls
    let crates = take 8 input
    let indices = words $ input !! 8
    let stacks = [] : makeStacks indices crates
    let moves = drop 10 input
    let finalStacks = doMoves moves stacks
    let finalAnswer = readTop finalStacks
    putStrLn $ show finalAnswer

makeStacks :: [String] -> [String] -> [[Char]]
makeStacks [] _ = []
makeStacks (index : indices) crates =
    (getStackAtIndex index crates) : makeStacks indices crates

getStackAtIndex :: String -> [String] -> [Char]
getStackAtIndex index [] = []
getStackAtIndex index (crate : crates) =
    let indexNum = read index :: Int in
    let strIndex = (indexNum - 1) * 4 + 1 in
    if crate !! strIndex == ' '
        then getStackAtIndex index crates
        else (crate !! strIndex) : getStackAtIndex index crates

doMoves [] stacks = stacks
doMoves (move : moves) stacks =
    doMoves moves $ processMove move stacks

processMove move stacks =
    let parsed = words move in
    let from = read (parsed !! 3) :: Int in
    let to = read (parsed !! 5) :: Int in
    let num = read (parsed !! 1) :: Int in
    let toMove = take num (stacks !! from) in
    let newFromStack = drop num (stacks !! from) in 
    let newToStack = moveCratesP2 toMove (stacks !! to) in --Change here for P1/P2
    let (begin, (_ : end)) = splitAt from stacks in
    let stackWithNewFrom = begin ++ (newFromStack : end) in
    let (begin2, (_ : end2)) = splitAt to stackWithNewFrom in
    begin2 ++ (newToStack : end2)
    

moveCrates [] stack = stack
moveCrates (crate : crates) stack = moveCrates crates (crate : stack)

moveCratesP2 crates stack = crates ++ stack
    
readTop [] = []
readTop (stack : stacks) =
    if length stack > 0 then (stack !! 0) : readTop stacks else readTop stacks
