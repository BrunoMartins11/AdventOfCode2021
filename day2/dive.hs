import System.IO
import Control.Monad
import Data.List.Split

main = do 
        content <- readFile "day2.txt" 
	let list = words $ content
        print $ multiplyPos2 (getPosition2 list (0,0,0))

readInt :: String -> Int
readInt = read

getPosition :: [String] -> (Int, Int) -> (Int, Int)
getPosition [] (depth, horizontal) = (depth, horizontal)
getPosition (command:number:t) (depth, horizontal)
			| command == "forward" = getPosition t (depth, horizontal + readInt(number))
			| command == "up" = getPosition t (depth - readInt(number), horizontal)
			| command == "down" = getPosition t (depth + readInt(number), horizontal)
			| otherwise = getPosition t (depth, horizontal)

getPosition2 :: [String] -> (Int, Int, Int) -> (Int, Int, Int)
getPosition2 [] (depth, horizontal, aim) = (depth, horizontal, aim)
getPosition2 (command:number:t) (depth, horizontal, aim)
			| command == "forward" = getPosition2 t (depth+(readInt(number)*aim), horizontal + readInt(number), aim)
			| command == "up" = getPosition2 t (depth, horizontal, aim - readInt(number))
			| command == "down" = getPosition2 t (depth, horizontal, aim + readInt(number))
			| otherwise = getPosition2 t (depth, horizontal, aim)


multiplyPos :: (Int, Int) -> Int
multiplyPos (x,y) = x*y

multiplyPos2 :: (Int, Int, Int) -> Int
multiplyPos2 (x,y,z) = x*y
