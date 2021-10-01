import Data.List
import Data.Maybe

type Board = [Int] -- list from 0 to 8
type Position = Int -- index in list
type State = (Board, [Action])

{-  Create instance of class "Action", using Eq typeclass, with 4 memberfunctions each
    using the 'show' function to display a string depending on which function is called-}
data Action = L | U | R | D deriving Eq
instance Show Action where
    show L = "Left"
    show R = "Right"
    show U = "Up"
    show D = "Down"

solution :: Board
solution = [1,2,3,4,5,6,7,8,-1]

sT :: Board -- temp test
sT = [7,2,3,4,-1,6,1,8,5] 

isSolved :: Board -> Bool
isSolved board = board == solution

findEmpty :: Board -> Position
findEmpty board = fromJust(elemIndex (-1) board)

replace :: Position -> Int -> Board -> Board
replace idx n board = leftPart ++ (n:rightPart) where (leftPart, _:rightPart) = splitAt idx board

swap :: Position -> Position -> Board -> Board
swap idx1 idx2 board = replace idx1 (board !! idx2) (replace idx2 (board !! idx1) board)

makeMove :: Board -> Action -> Board -- moves "-1" within the 3x3 plane
makeMove board move = if idx `elem` idxs then [] else swap idx (idx + off) board
    where
        idx = findEmpty board
        (idxs, off) = case move of
            L -> ([0, 3, 6], -1)
            R -> ([2, 5, 8], 1)
            U -> ([0, 1, 2], -3)
            D -> ([6, 7, 8], 3)

allFutures :: State -> [State]
allFutures state = filter (not . null . fst) $ map func options
    where
        (currBoard, prevMoves) = state
        idx = findEmpty currBoard
        func ltr = (makeMove currBoard ltr, ltr : prevMoves)
        options = [L,R,U,D]

possibleSolutions :: Board -> [[State]]
possibleSolutions board = [(board,[])] : [concatMap allFutures x | x <- possibleSolutions board]

solve :: Board -> State
solve board = head $ take 1 $ concat [[x | x <- xs, isSolved $ fst x] | xs <- possibleSolutions board]