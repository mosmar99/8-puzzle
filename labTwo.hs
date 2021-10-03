import Data.List
import Data.Maybe

type Board = [Int] -- [0..8]
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

isSolved :: Board -> Bool
isSolved board = board == solution

findEmpty :: Board -> Position
findEmpty board = fromJust(elemIndex (-1) board)

replace :: Position -> Int -> Board -> Board
replace idx n board = leftPart ++ (n:rightPart) where (leftPart, _:rightPart) = splitAt idx board

swap :: Position -> Position -> Board -> Board
swap idx1 idx2 board = replace idx1 (board !! idx2) (replace idx2 (board !! idx1) board)

makeMove :: Board -> Action -> [Board] -- moves "-1" within the 3x3 plane
makeMove board move = [swap idx (idx + off) board | idx `notElem` idxs]
    where idx = findEmpty board
          (idxs, off) = case move of L -> ([0, 3, 6], -1)
                                     R -> ([2, 5, 8], 1)
                                     U -> ([0, 1, 2], -3)
                                     D -> ([6, 7, 8], 3)
             
allFutures :: State -> [State]
allFutures state@(board, list) = filter (/= state) . filter (not . null . fst) . map (\option -> if list /= [] && head list == option 
    then state else (concat $ makeMove board $ rev option, rev option : list)) $ [U, D, L, R]
    where
        rev option = case option of
            L -> R; R -> L; U -> D; D -> U

possibleSolutions :: Board -> [[State]]
possibleSolutions board = [(board,[])] : [concatMap allFutures x | x <- possibleSolutions board]

solve :: Board -> [Action]
solve board = snd $ head $ take 1 $ concat [[x | x <- xs, isSolved $ fst x] | xs <- possibleSolutions board] --returns the proper actions to the first solved board it sees