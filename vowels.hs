-------------------------------------
---UofA Cs372 Sp17 Project 2#4 
---Stusent/Author : Dong Liang
---Instructor: Dr. McCann 
---Graders:Patrick/Andrea


---Description: This is haskell code
---              has function to do
---		 Vowel Histogram.
---              The main func read
---              String from a txt
---              file and use that
---              string and func be-
---              -low to do things.
------------------------------------


import System.IO 
import Data.Char

---Count how man input char is in the input string.
countLetters :: String -> Char -> Int
countLetters xs x = foldl (\count char -> if char == x||char == toUpper x then (count + 1) else count) 0 xs

---Convert a char to a String type.
charToStr :: Char -> String
charToStr x= [x]

---Get a String type A.
getA :: Char -> String -> String
getA ch str  = replicate (countLetters str ch) ch

---Get a String type B.
getB :: Char -> String -> String
getB ch str  = if (length str) > 0 then  (((charToStr ch)++": ") ++ (replicate (length str) '*'))++"\n"
else (((charToStr ch)++": ") ++ (replicate (length str) '*'))
 
---vowelListList :the function takes a String and create an array for 'a''e''i''o''u'
vowelListList :: String -> [String]
vowelListList str = [(getA 'a' str),(getA 'e' str),(getA 'i' str),(getA 'o' str),(getA 'u' str)]


---vowelHistogram :the function takes a special array and return the stars based on the number of the vowel
vowelHistogram :: [String] -> String
vowelHistogram arr = (getB 'a' (arr !! 0))++(getB 'e' (arr !! 1))++(getB 'i' (arr !! 2))++(getB 'o' (arr !! 3))++(getB 'u' (arr !! 4))


main :: IO ()
main = do
  content <- readFile "vowels.txt"
  let arr = vowelListList content
  let s = vowelHistogram arr
  putStrLn s


 
