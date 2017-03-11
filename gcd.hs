-------------------------------------
---UofA Cs372 Sp17 Project 2 #2
---Stusent/Author : Dong Liang
---Instructor: Dr. McCann 
---Graders:Patrick/Andrea


---Description: This is haskell code
---              has function to do
---		 gcd.
------------------------------------


ourGCD :: Int -> Int -> Int
ourGCD x y = 
  if x<0 || y<0 
  then error "x and y cannot be negative."
  else if x==0 && y==0 then error  "x and y cannot both equal to 0."
  else if y==0 then x
  else if x==0 then y
  else ourGCD y (mod x y)


