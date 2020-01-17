import qualified Data.Map as Map
import Data.List
import Data.Ord

type ID = Int

type Expertise = Map.Map Int Categories

-- define Categories type
data Categories = Expert|Knowledgeable|Familiar|Inexpert deriving(Show,Eq)

-- define technical member type
data Member = Member {
                      memberId :: ID  -- member ID
                     ,expertise :: Expertise  --expertise relation between member and each patent
                     ,allocatedTasks :: [Int]
                   }deriving(Show)



--generate data of member and their relationship with each request
--assume assign 20 requests to 4 members
expertise1 :: Expertise 
expertise1 =  Map.fromList [(1,Expert),(2,Knowledgeable),(3,Expert),(4,Inexpert),(5,Familiar),(6,Inexpert),(7,Familiar),(8,Expert),(9,Expert),(10,Expert),(11,Familiar),(12,Familiar),(13,Inexpert),(14,Expert),(15,Inexpert),(16,Familiar),(17,Familiar),(18,Knowledgeable),(19,Knowledgeable),(20,Expert)]

member1 :: Member
member1 = Member 1 expertise1 []

expertise2 :: Expertise
expertise2 =  Map.fromList [(1,Familiar),(2,Expert),(3,Inexpert),(4,Inexpert),(5,Knowledgeable),(6,Knowledgeable),(7,Knowledgeable),(8,Expert),(9,Familiar),(10,Expert),(11,Knowledgeable),(12,Expert),(13,Inexpert),(14,Familiar),(15,Familiar),(16,Familiar),(17,Expert),(18,Knowledgeable),(19,Familiar),(20,Expert)]

member2 :: Member
member2 = Member 2 expertise2 []

expertise3 :: Expertise
expertise3 =  Map.fromList [(1,Expert),(2,Inexpert),(3,Familiar),(4,Knowledgeable),(5,Knowledgeable),(6,Inexpert),(7,Expert),(8,Expert),(9,Expert),(10,Expert),(11,Familiar),(12,Knowledgeable),(13,Familiar),(14,Expert),(15,Inexpert),(16,Familiar),(17,Familiar),(18,Expert),(19,Knowledgeable),(20,Inexpert)]

member3 :: Member
member3 = Member 3 expertise3 []

expertise4 :: Expertise
expertise4 =  Map.fromList [(1,Knowledgeable),(2,Inexpert),(3,Expert),(4,Inexpert),(5,Familiar),(6,Expert),(7,Knowledgeable),(8,Knowledgeable),(9,Expert),(10,Expert),(11,Expert),(12,Familiar),(13,Inexpert),(14,Inexpert),(15,Inexpert),(16,Familiar),(17,Familiar),(18,Expert),(19,Knowledgeable),(20,Knowledgeable)]

member4 :: Member
member4 = Member 4 expertise4 []

members = [member1,member2,member3,member4]


-- get the count of specific element in a list
count :: Int ->[Int] -> Int
count x = length.filter(==x)


-- get a list of frequency of each elements from high to low
topFrequent :: [Int] -> [Int]
topFrequent list = map head . sortBy(comparing (Down . length)) . group . sort$ list

-- get the count of specific element which has maximum count in a list
maximumCount :: [Int] -> Int
maximumCount list = count (head (topFrequent list)) list

-- get the count of specific element which has mimimum count in a list
minimumCount :: [Int] -> Int
minimumCount list = count (last (topFrequent list)) list


--numberOfExpert :: [Int] ->Int ->Int ->Int
--numberOfExpert list memberId bidId = length(filter(expertise (members !! memberId Map.! bidId) == Expert) list)

numberOfExpert :: [Int] ->Int ->Int ->Int
numberOfExpert [] _ _ = 0
numberOfExpert (x:xs) memberId bidId
                     | ((expertise (members !! memberId) Map.! bidId) == Expert) =1+ numberOfExpert xs memberId bidId
                     | otherwise = numberOfExpert xs memberId bidId



--check if this solution fit the requirements
check :: Int -> Int-> [Int] -> Int -> Int -> Bool

check numBids x y m k 
             -- no member will review request submission from their inexpert category
             |((expertise (members !! (x-1)) Map.! numBids) == Inexpert) = False
             -- no member will review more than k submission from the familiar category
             |((expertise (members !! (x-1) ) Map.! numBids == Familiar)&&(count x y>=k)) = False
             -- the workloads of each members should be approximately equal
             -- in list, max count - min count no more than m
             |(maximumCount y - minimumCount y > m) = False 
             -- the total number of submission assigned to a member who placed in the expert category should be as large as possible
             -- I plan to set a threhold value(0.6) to expert, which make sure at least a certain percentage of expert requests should be in list
          -- |((numberOfExpert y (x-1) numBids)) < 0.6*numBids = False
     
             |otherwise = True


-- return a list of solutions
solutions :: Int -> Int -> Int ->Int -> [[Int]] 
solutions 0 n m k = [[]]
solutions numBids n m k = [x:y|y<-solutions(numBids-1) n m k,x <- [1..n],check numBids x y m k]

-- Interpretation of output result
-- output is a 2-d list, which contains all suitable solutions
-- for one solution such as [1,3,4,4,3,2,1,3,4,1,4,2,3,4,2,2,3,1,1,1] which means there are 20 tasks ,the number in the array is member ID. Therefore the first request is assigned to member 1, then the second request is assigned to member 3 ......


main = do
           let numBids = 20 
           let n = 4
           let m = 2
           let k = 5           
           print(solutions numBids n m k)
         
        

