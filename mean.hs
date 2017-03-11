-------------------------------------
---UofA Cs372 Sp17 Project 2 #1
---Stusent/Author : Dong Liang
---Instructor: Dr. McCann 
---Graders:Patrick/Andrea


---Description: This is haskell code
---              has function to do
---		 finding the mean of
---		 a list of numbers.
------------------------------------

---mySum  A recusive help function to do sum a list since sum is not allowed in this assign.
mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x+(mySum xs)


meanIntList :: [Int] -> Float
meanIntList [] = error "The input list is empty."
meanIntList list = fromIntegral(mySum list ) / fromIntegral(length list)

