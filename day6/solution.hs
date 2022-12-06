import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.Char
import Data.Set

main = do
    stream <- fmap Text.unpack (Text.readFile "input.txt")
    -- Change the last arg in the next line from 13 to 3 to switch to part 1
    let indexOfFirstMarker = findFirstMarker stream 1 [] 13
    putStrLn $ show indexOfFirstMarker

findFirstMarker [] n _ _ = n
findFirstMarker (curr : stream) n lastSeen window =
    if length lastSeen < window
        then findFirstMarker stream (n + 1) (curr : lastSeen) window
    else
        if elem curr lastSeen || containsDups lastSeen
            then findFirstMarker stream
                                 (n + 1)
                                 (curr : (Data.List.take (window - 1) lastSeen))
                                 window
            else n

containsDups :: [Char] -> Bool
containsDups l = length l /= (size $ fromList l)

