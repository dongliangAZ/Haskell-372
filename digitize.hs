------------------------------------
---UofA Cs372 Sp17 Project 2 #5
---Stusent/Author : Dong Liang
---Instructor: Dr. McCann 
---Graders:Patrick/Andrea


---Description: This is haskell code
---              has function to do
---		 the digit operatio-
---              -ns.
------------------------------------

import System.IO 
import Data.Char 
import Data.Ord

---getDigits   just pull the number out from the input String and return a list of integers or ':'
getDigits :: String -> [Char]
getDigits [] = []
getDigits (x:xs) = if ord x >= 48 && ord x <= 58
  then (x): (getDigits xs)
  else (getDigits xs)

---isError   Determine the input str will cause error or not.
isError :: String -> Bool
isError str = if (length str)== (length (getDigits str) )  
  then False
  else True

---makeStringA Return the first line of result
makeStringA :: [Char] -> String
makeStringA "" = "";
makeStringA (x:xs) = (getStringA x) ++ (makeStringA xs)

---makeStringB Return the first line of result
makeStringB :: [Char] -> String
makeStringB "" = "";
makeStringB (x:xs) = (getStringB x) ++ (makeStringB xs)

---makeStringC Return the first line of result
makeStringC :: [Char] -> String
makeStringC "" = "";
makeStringC (x:xs) = (getStringC x) ++ (makeStringC xs)

---makeResult  Return the combined result of the String
makeResult :: [Char] -> String
makeResult s = (makeStringA s)++"\n" ++ (makeStringB s) ++"\n"++ (makeStringC s)
  


---getStringA take the input number and make the first line of the String
getStringA :: Char -> String
getStringA ':' = " "
getStringA '0' = " _ "
getStringA '1' = "   "
getStringA '2' = " _ "
getStringA '3' = " _ "
getStringA '4' = "   "
getStringA '5' = " _ "
getStringA '6' = " _ "
getStringA '7' = " _ "
getStringA '8' = " _ "
getStringA '9' = " _ "
 
---getStringB take the input number and make the 2nd line of the String
getStringB :: Char -> String
getStringB ':' = "."
getStringB '0' = "| |"
getStringB '1' = " | "
getStringB '2' = " _|"
getStringB '3' = " _|"
getStringB '4' = "|_|"
getStringB '5' = "|_ "
getStringB '6' = "|_ "
getStringB '7' = "  |"
getStringB '8' = "|_|"
getStringB '9' = "|_|"

---getStringC take the input number and make the 3rd line of the String
getStringC :: Char -> String
getStringC ':' = "."
getStringC '0' = "|_|"
getStringC '1' = " | "
getStringC '2' = "|_ "
getStringC '3' = " _|"
getStringC '4' = "  |"
getStringC '5' = " _|"
getStringC '6' = "|_|"
getStringC '7' = "  |"
getStringC '8' = "|_|"
getStringC '9' = " _|"


---This is the function that call other help functions and use the input string to return a result
---digitize Call other functions and get a result. The result of this function is a 3 lines magic String.
digitize :: String -> String
digitize [] = "\n\n"
digitize str = if (isError str) 
  then error "The input str is not legal."
  else (makeResult (getDigits str))
 