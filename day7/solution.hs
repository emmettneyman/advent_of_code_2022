import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.Char
import Data.Maybe


data Dir = Dir {
    dirs :: [Dir],
    dName :: String,
    dSize :: Int
} deriving (Show, Eq)

main = do
    ls <- fmap Text.lines (Text.readFile "input.txt")
    let input = map Text.unpack ls
    let fixedInput = fixInput input
    let root = Dir [] "root" 0
    let slash = mkDir "/"
    let newRoot = Dir (slash : dirs root) (dName root) (dSize root)
    let tree = processInput fixedInput newRoot []
    let sumLessThanThresh = walkTree tree 0
    putStrLn $ show sumLessThanThresh
    let totalSpace = dSize tree
    let freeSpace = 70000000 - totalSpace
    let neededSpace = 30000000 - freeSpace
    let dirToDelete = findDirToDelete tree neededSpace $ dSize tree
    putStrLn $ show dirToDelete

fixInput input =
    let numCDs = getNumCDs input in
    let numDotDots = getNumDotDots input in
    let diff = (numCDs - numDotDots) - 1 in
    input ++ (take diff $ repeat "$ cd ..")

getNumCDs [] = 0
getNumCDs (hd : tl) =
    let toks = words hd in
    if length toks == 3 && (toks !! 2) /= ".."
        then 1 + getNumCDs tl
        else getNumCDs tl

getNumDotDots [] = 0
getNumDotDots (hd : tl) =
    let toks = words hd in
    if length toks == 3 && (toks !! 2) == ".."
        then 1 + getNumDotDots tl
        else getNumDotDots tl

processInput :: [String] -> Dir -> [Dir] -> Dir
processInput [] curr path = curr
processInput (line : lines) curr path =
    let toks = words line in
    if toks !! 0 == "$"
    then if toks !! 1 == "cd"
        then -- cd case
            if toks !! 2 == ".."
                then processInput lines (getParent curr (head path)) (tail path)
                else
                    let newDir = toks !! 2 in
                    processInput lines (mkDir newDir) (curr : path)
        else -- ls case
            processInput lines curr path
    else if toks !! 0 == "dir"
        then -- dir case
            processInput lines curr path
        else -- file case
            processInput lines (addFile curr (toks !! 0) (toks !! 1)) path

getParent curr par = 
    Dir (curr : (dirs par)) (dName par) (dSize curr + dSize par)

findDir (dir : dirs) target =
    if dName dir == target then dir else findDir dirs target

mkDir name = Dir [] name 0

addFile curr size name = Dir
    (dirs curr)
    (dName curr)
    ((read size :: Int) + (dSize curr))

walkTree dir n = 
    let newSum = if (dSize dir) < 100000 then (dSize dir) + n else n in
    -- let newSum = n + dSize dir in
    let directories = dirs dir in
    if length directories == 0 then newSum
    else newSum + walkDirs directories n

walkDirs [] n = n
walkDirs (d : ds) n =
    walkTree d n + walkDirs ds n

findDirToDelete :: Dir -> Int -> Int -> Int
findDirToDelete tree neededSpace candidate =
    let directories = dirs tree in
    if dSize tree > candidate || dSize tree < neededSpace
        then candidate
        else findDirToDeleteFromList directories
                                     neededSpace
                                     (min candidate $ dSize tree)

findDirToDeleteFromList :: [Dir] -> Int -> Int -> Int
findDirToDeleteFromList [] neededSpace candidate = candidate
findDirToDeleteFromList (d : ds) neededSpace candidate = 
    min
        (findDirToDelete d neededSpace candidate)
        (findDirToDeleteFromList ds neededSpace candidate)




