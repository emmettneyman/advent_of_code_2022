import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.Char

main = do
    ls <- fmap Text.lines (Text.readFile "input.txt")
    let input = map Text.unpack ls
    let registerVals = processInput input [1]
    let signalStrength = getSignalStrength registerVals
    putStrLn $ show signalStrength
    let screen = getScreenPixels registerVals 1
    putStrLn $ show screen

processInput :: [String] -> [Int] -> [Int]
processInput [] l = l
processInput (instr : instrs) l =
    if instr == "noop"
        then processInput instrs $ l ++ [last l]
        else
            let val = read $ (words instr) !! 1 :: Int in
            let currVal = last l in
            processInput instrs $ l ++ [currVal, currVal + val]
            
getSignalStrength vals =
    (20 * vals !! 19) + (60 * vals !! 59) + (100 * vals !! 99) +
    (140 * vals !! 139) + (180 * vals !! 179) + (220 * vals !! 219)

getScreenPixels [_] _ = []
getScreenPixels (regVal : tl) cycle =
    (if isWithinWindow regVal cycle then '#' else '.') :
            (getScreenPixels tl $ cycle + 1)

isWithinWindow regVal cycle = abs (regVal - ((cycle - 1) `mod` 40)) <= 1
