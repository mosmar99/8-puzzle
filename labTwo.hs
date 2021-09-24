-- Task 1
type Board = [[Int]] -- revise
type Position = (Int,Int) -- revise
type State = Int

-- task 2
-- left, down, right, up, derives the characteristics of Eq
data Action = L | U | R | D deriving Eq 

--  creates an instance of type Action with four methods using the show function
instance Show Action where -- 
    show L = "Left"
    show U = "Up"
    show R = "Right"
    show D = "Down"

-- task 3
solution :: Board
solution = [[1,2,3], [4,5,6], [7,8,-1]]

-- isSolved board = concatmap filter (a -> Bool) ([a])
-- idea: concat both maps, check if each values is identical in sol and isSolved


