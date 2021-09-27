import Data.List
import Data.Maybe

type Board = [Int] 
type Position = Int
type State = Int -- revise

{-  Create instance of class "Action", using Eq typeclass, with 4 memberfunctions each
    using the 'show' function to display a string depending on which function is called-}
data Action = L | U | R | D deriving Eq 
instance Show Action where 
    show L = "Left"
    show U = "Up"
    show R = "Right"
    show D = "Down"

solution :: Board
solution = [1,2,3,4,5,6,7,8,-1]

isSolved :: Board -> Bool
isSolved board = board == solution

findEmpty :: Board -> Position
findEmpty board = fromJust(elemIndex (-1) board)

replace :: Position -> Int -> Board -> Board
replace idx n board = leftPart ++ (n:rightPart) where (leftPart, (_:rightPart)) = splitAt idx board

swap :: Position -> Position -> Board -> Board
swap idx1 idx2 board = replace idx1 (board !! idx2) (replace idx2 (board !! idx1) board)

makeMove :: Board -> Action -> Board -- moves "-1" within the 3x3 plane
makeMove board move
    | let idx = findEmpty board, move == "Left" = swap (idx) (idx-1) board  
    | let idx = findEmpty board, move == "Up" = swap (idx) (idx-3) board  
    | let idx = findEmpty board, move == "Right" = swap (idx) (idx+1) board  
    | let idx = findEmpty board, move == "Down" = swap (idx) (idx+3) board
    -- continue 