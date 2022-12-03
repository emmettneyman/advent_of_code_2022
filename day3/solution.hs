
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.Char
import Data.Set

main = do
    ls <- fmap Text.lines (Text.readFile "input.txt")
    let sacks = Data.List.map Text.unpack ls
    let sum = processList sacks 0
    putStrLn $ show sum
    let sumPart2 = processListPart2 sacks 0
    putStrLn $ show sumPart2

processList :: [String] -> Int -> Int
processList [] n = n
processList (sack : sacks) n =
    processList sacks $ (+) n $ getPriority $ getRepeatingItem sack

getPriority item =
    let asciiVal = ord item in
    if asciiVal >= 97
        then asciiVal - 96
        else asciiVal - 38

getRepeatingItem sack = 
    let numItems = length sack in
    let firstCompartment = fromList $ Data.List.take (div numItems 2) sack in
    let secondCompartment = fromList $ Data.List.drop (div numItems 2) sack in
    elemAt 0 $ intersection firstCompartment secondCompartment

processListPart2 [] n = n
processListPart2 (elf1 : elf2 : elf3 : rest) n =
    processListPart2 rest $ (+) n $ getPriority $ getCommonItem elf1 elf2 elf3

getCommonItem l1 l2 l3 =
    let s1 = fromList l1 in
    let s2 = fromList l2 in
    let s3 = fromList l3 in
    elemAt 0 $ intersection s1 $ intersection s2 s3
