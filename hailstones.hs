-------------------------------------
---UofA Cs372 Sp17 Project 2 #3
---Stusent/Author : Dong Liang
---Instructor: Dr. McCann 
---Graders:Patrick/Andrea


---Description: This is haskell code
---              has function to do
---		 hailstones.
------------------------------------


hailstones :: Int -> [Int]
hailstones 1 = [1]
hailstones n = if n < 1
  then error "Input number should be >=1"
  else if((n `mod` 2)==0) then n : ( hailstones ( n `div` 2))
  else n : ( hailstones ( 3 * n + 1 ))



