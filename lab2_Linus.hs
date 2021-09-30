import Data.List
import Data.Maybe

type Board = [Int]
type Position = Int --[0,8]
type State = (Board,[Action])

{-  Custom data type 'Action' which can be either U, D, L, R, using Eq typeclass to print
    their respective values using 'show'-}
data Action = L | U | R | D deriving Eq
--  Create instance of 'Action' and defining the 4 different possible values for 'Action'
instance Show Action where
    show U = "Up"
    show D = "Down"
    show L = "Left"
    show R = "Right"

solution :: Board
solution = [1,2,3,4,5,6,7,8,0]
testBoard :: Board  --temporary board to test functions
testBoard = [2,7,0,8,5,1,3,6,4]

isSolved :: Board -> Bool   --tested
isSolved board = board == solution

findEmpty :: Board -> Position  --tested
findEmpty board = fromJust $ findIndex (== 0) board --always within [0,8]

replace :: Position -> Int -> Board -> Board    --tested
replace ix newVal board = fst ++ newVal:ys where (fst,(_:ys)) = splitAt ix board

swap :: Position -> Position -> Board -> Board  --tested
swap ix1 ix2 board = replace ix2 (board !! ix1) $ replace ix1 (board !! ix2) board

makeMove :: Board -> Action -> [Board]  --tested
makeMove board action = if not $ elem ix validPositions then [] else (swap ix (ix + offset) board) : []
    where ix = findEmpty board
          (validPositions, offset) = case action of U -> ([3..8],(-3))  --all valid positions for their respective actions and the offset to make that action
                                                    D -> ([0..5],3)
                                                    L -> ([1,2,4,5,7,8],(-1))
                                                    R -> ([0,1,3,4,6,7],1)

allFutures :: State -> [State]  --tested
allFutures state =
    let state1 = (concat $ makeMove board U,U : list)
        state2 = (concat $ makeMove board D,D : list)
        state3 = (concat $ makeMove board L,L : list)
        state4 = (concat $ makeMove board R,R : list)
    in filter (\state -> fst state /= []) $ state1 : state2 : state3 : state4 : []
        where (board,list) = state

possibleSolutions :: Board -> [[State]]
possibleSolutions board
    | isSolved board = [[]]
    | otherwise = let starterState = (board,[])
                      futures = (allFutures starterState) : $ map (\s -> allFutures s) $ snd s
                  in futures