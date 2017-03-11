------------------------------------
---UofA Cs372 Sp17 Project 2 #5
---Stusent/Author : Dong Liang
---Instructor: Dr. McCann 
---Graders:Patrick/Andrea


---Description: This is haskell code
---              has function to do
---		 insertion sort and
---              in order.
------------------------------------

insertInOrder :: Int -> [Int] -> [Int]
insertInOrder i [] = [i]
insertInOrder i (j:js) = if i < j
  then i:j:js 
  else j : insertInOrder i js

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort [c] = [c]
insertionSort (i:is) = insertInOrder i (insertionSort is)

