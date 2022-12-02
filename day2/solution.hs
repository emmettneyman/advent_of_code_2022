import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.Char

main = do
    ls <- fmap Text.lines (Text.readFile "input.txt")
    let games = map Text.unpack ls
    let score = processList games 0
    putStrLn (show score)
    let scorePart2 = processListPart2 games 0
    putStrLn (show scorePart2)

processList [] n = n
processList (game : games) n = processList games (score game) + n

score :: String -> Int
score s = 
    let ss = words s in
    (playScore (ss !! 1)) + (gameScore (ss !! 0) (ss !! 1))

playScore "X" = 1
playScore "Y" = 2
playScore "Z" = 3

gameScore "A" "X" = 3
gameScore "A" "Y" = 6
gameScore "A" "Z" = 0
gameScore "B" "X" = 0
gameScore "B" "Y" = 3
gameScore "B" "Z" = 6
gameScore "C" "X" = 6
gameScore "C" "Y" = 0
gameScore "C" "Z" = 3

processListPart2 [] n = n
processListPart2 (game : games) n =
    processListPart2 games (scorePart2 game) + n

scorePart2 s =
    let ss = words s in
    let oppMove = ss !! 0 in
    let result = ss !! 1 in
    playScore (myMove oppMove result) + scoreResult result

myMove "A" "X" = "Z"
myMove "A" "Y" = "X"
myMove "A" "Z" = "Y"
myMove "B" "X" = "X"
myMove "B" "Y" = "Y"
myMove "B" "Z" = "Z"
myMove "C" "X" = "Y"
myMove "C" "Y" = "Z"
myMove "C" "Z" = "X"

scoreResult "X" = 0
scoreResult "Y" = 3
scoreResult "Z" = 6
