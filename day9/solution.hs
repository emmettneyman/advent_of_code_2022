import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List as List
import Data.Char
import Data.Set as Set

main = do
    ls <- fmap Text.lines (Text.readFile "input.txt")
    let input = List.map Text.unpack ls
    let visitedPositions = processInput input (0, 0) (0, 0) $ singleton (0,0)
    putStrLn $ show $ size visitedPositions
    let partTwoSet = processPartTwo
                        input
                        (List.take 10 $ repeat (0, 0)) $
                        singleton (0, 0)
    putStrLn $ show $ size partTwoSet

processInput [] hpos tpos s = s
processInput (move : moves) (hx, hy) (tx, ty) s =
    let toks = words move in
    let direction = toks !! 0 in
    let num = read $ toks !! 1 :: Int in
    let (newTailPositions, (hx', hy'), (tx', ty')) = processMove
                                                        direction
                                                        num
                                                        (hx, hy)
                                                        (tx, ty) in
    processInput moves (hx', hy') (tx', ty') $ Set.union newTailPositions s

processMove _ 0 (hx, hy) (tx, ty) = (Set.empty, (hx, hy), (tx, ty))
processMove dir n  (hx, hy) (tx, ty) =
    let (nhx, nhy) = moveHead dir (hx, hy) in
    let newTailPos = if needToMoveTail (nhx, nhy) (tx, ty)
        then getNewTailPos (nhx, nhy) (tx, ty)
        else (tx, ty) in
    let (newTailPositions, (hx', hy'), (tx', ty')) = processMove
                                                        dir
                                                        (n - 1)
                                                        (nhx, nhy)
                                                        newTailPos in
    let newSet = Set.union newTailPositions $ singleton newTailPos in
    (newSet, (hx', hy'), (tx', ty'))

moveHead "U" (x, y) = (x, y+1)
moveHead "D" (x, y) = (x, y-1)
moveHead "R" (x, y) = (x+1, y)
moveHead "L" (x, y) = (x-1, y)

needToMoveTail (hx, hy) (tx, ty) = abs (hx - tx) > 1 || abs (hy - ty) > 1

getNewTailPos (hx, hy) (tx, ty) =
    if hx == tx -- we need to move the tail up or down
        then if hy > ty then (tx, ty+1) else (tx, ty-1)
    else if hy == ty -- we need to move the tail left or right
        then if hx > tx then (tx+1, ty) else (tx-1, ty)
    else -- some diagonal move is required
        if hy > ty && hx > tx -- move up and to the right
            then (tx+1, ty+1)
        else if hy > ty && hx < tx -- move up and to the left
            then (tx-1, ty+1)
        else if hy < ty && hx > tx -- move down and to the right
            then (tx+1, ty-1)
        else -- move down and to the left
            (tx-1, ty-1)

processPartTwo [] coords s = s
processPartTwo (move: moves) coords s =
    let toks = words move in
    let direction = toks !! 0 in
    let num = read $ toks !! 1 :: Int in
    let (newTailPositions, newCoords) = processMovePart2
                                            direction
                                            num
                                            coords in
    processPartTwo moves newCoords $ Set.union newTailPositions s

processMovePart2 _ 0 coords = (Set.empty, coords)
processMovePart2 dir n coords =
    let (hx, hy) = coords !! 0 in
    let (nhx, nhy) = moveHead dir (hx, hy) in
    let newCoords = processCoords dir $ (nhx, nhy) : List.drop 1 coords in
    let (resultSet, resultCoords) = processMovePart2 dir (n-1) newCoords in
    let newSet = Set.union resultSet $ singleton $ (newCoords !! 9) in
    (newSet, resultCoords)

processCoords _ (tl : []) = [tl]
processCoords dir (hd : tl : rest) =
    let (hx, hy) = hd in
    let (tx, ty) = tl in
    let newTailPos = if needToMoveTail (hx, hy) (tx, ty)
        then getNewTailPos (hx, hy) (tx, ty)
        else (tx, ty) in
    (hx, hy) : processCoords dir (newTailPos : rest)
    
