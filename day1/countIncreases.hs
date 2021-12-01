import System.IO
import Control.Monad


main= do 
        content <- readFile "day1.txt" 
	let list = map readInt . words $ content
	print (numberOfIncreases (unclearName list) 0)

readInt :: String -> Int
readInt = read

numberOfIncreases [x,y] ret =if x < y then ret + 1 else ret
numberOfIncreases (h:s:t) ret = if h < s then numberOfIncreases (s:t) ret+1 else numberOfIncreases (s:t) ret 

unclearName [x,y,z] = [x+y+z] 
unclearName (x:y:z:tail) = [x+y+z] ++ unclearName (y:z:tail) 
