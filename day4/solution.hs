import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.List.Split
import Data.Char
import Data.Set

main = do
    ls <- fmap Text.lines (Text.readFile "input.txt")
    let pairs = Data.List.map Text.unpack ls
    let solutionP1 = processList pairs 0
    putStrLn $ show solutionP1

processList [] n = n
processList (pair : pairs) n =
    processList pairs $ (+) n $ rangeIsFullyContained pair
    
rangeIsFullyContained pair =
    let pairList = splitOn "," pair in
    let first = splitOn "-" $ pairList !! 0 in
    let second = splitOn "-" $ pairList !! 1  in
    let firstSet =
        fromList [read (first !! 0) :: Int .. read (first !! 1) :: Int] in
    let secondSet =
        fromList [read (second !! 0) :: Int .. read (second !! 1) :: Int] in
    part2 firstSet secondSet -- change this function call to switch parts

part1 firstSet secondSet =
    if isSubsetOf firstSet secondSet || isSubsetOf secondSet firstSet
        then 1
        else 0

part2 firstSet secondSet =
    if size (intersection firstSet secondSet) > 0 then 1 else 0
