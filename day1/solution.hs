import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List

main = do
    ls <- fmap Text.lines (Text.readFile "input.txt")
    let sums = processList ls [] 0
    let sorted = reverse (sort sums)
    putStrLn (show (take 1 sorted))
    putStrLn (show (sum (take 3 sorted)))

processList :: [Text.Text] -> [Int] -> Int -> [Int]
processList (v : vs) sums curr =
    if Text.unpack v == "" then
        processList vs (curr:sums) 0 else
        processList vs sums (curr + (read (Text.unpack v) :: Int))
processList [] sums curr = curr:sums
