import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.Char

main = do
    ls <- fmap Text.lines (Text.readFile "input.txt")
    let input = map Text.unpack ls
    let grid = makeGrid input
    let coords = [ (x, y) |
                    x <- [0..(length $ head grid) - 1],
                    y <- [0..(length grid) - 1] ]
    let visibleTrees = findNumVisible grid coords
    putStrLn $ show visibleTrees
    let maxScenicScore = findMaxScenicScore grid coords
    putStrLn $ show maxScenicScore

makeGrid :: [String] -> [[Int]]
makeGrid [] = []
makeGrid (l : ls) =
    convertLine l : makeGrid ls

convertLine [] = []
convertLine (c : cs) = digitToInt c : convertLine cs

findNumVisible grid coords =
    sum $ map (isVisible grid) coords

isVisible :: [[Int]] -> (Int, Int) -> Int
isVisible grid (x, y) =
    -- First, check if (x, y) is on an edge
    if x == 0 || y == 0 then 1 else
    if x == (length $ head grid) - 1 || y == (length grid) - 1 then 1 else
    let row = grid !! y in
    let col = getCol grid x in
    if visibleFromRight x row ||
       visibleFromLeft x row ||
       visibleFromTop y col ||
       visibleFromBottom y col then 1 else 0

getCol:: [[Int]] -> Int -> [Int]
getCol [] x = []
getCol (hd : tl) x =
    (hd !! x) : getCol tl x

visibleFromRight x row = (row !! x) > (maximum $ drop (x + 1) row)

visibleFromLeft x row = (row !! x) > (maximum $ take x row)

visibleFromTop y col = (col !! y) > (maximum $ take y col)

visibleFromBottom y col = (col !! y) > (maximum $ drop (y + 1) col)

findMaxScenicScore grid coords =
    maximum $ map (getScenicScore grid) coords

getScenicScore grid (x, y) =
    let row = grid !! y in
    let col = getCol grid x in
    let leftScore = getScore (row !! x) $ reverse $ take x row in
    let rightScore = getScore (row !! x) $ drop (x + 1) row in
    let topScore = getScore (col !! y) $ reverse $ take y col in
    let bottomScore = getScore (col !! y) $ drop (y + 1) col in
    leftScore * rightScore * topScore * bottomScore

getScore val [] = 0
getScore val (hd : tl) =
    if hd < val then 1 + getScore val tl else 1
