import Data.List
import Data.Bool(bool)

main :: IO ()
main = do
    xs <- lines <$> readFile "puzzle-input/day03.txt"
    let vals = map sum (map (map (\x -> read (x:[]) :: Int)) (transpose xs))
    let gamma = bin2dec (map (\x -> x >= (div (length xs) 2)) vals)
    let epsilon = bin2dec (map (\x -> x < (div (length xs) 2)) vals)
    print (gamma * epsilon)



bin2dec :: (Foldable f, Integral i) => f Bool -> i
bin2dec = foldl (\a -> (+) (2*a) . bool 0 1) 0