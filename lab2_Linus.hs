import Data.List
import Data.Maybe

type Board = [Int]
type Position = Int --[0,8]
type State = (Board,[Action])

{-  Custom data type 'Action' which can be either U, D, L, R, adding Eq typeclass properties, to print
    their respective values using 'show' with our own definitions-}
data Action = L | U | R | D deriving Eq
instance Show Action where
    show U = "Up"
    show D = "Down"
    show L = "Left"
    show R = "Right"

solution :: Board
solution = [1,2,3,4,5,6,7,8,0]

isSolved :: Board -> Bool
isSolved board = board == solution

findEmpty :: Board -> Position
findEmpty board = fromJust $ findIndex (== 0) board --always within [0,8]

replace :: Position -> Int -> Board -> Board
replace ix newVal board = fst ++ newVal:ys where (fst,(_:ys)) = splitAt ix board

swap :: Position -> Position -> Board -> Board
swap ix1 ix2 board = replace ix2 (board !! ix1) $ replace ix1 (board !! ix2) board

makeMove :: Board -> Action -> [Board]
makeMove board action = if not $ elem ix validPositions then [] else (swap ix (ix + offset) board) : []
    where ix = findEmpty board
          (validPositions, offset) = case action of U -> ([3..8],(-3))  --all valid positions for their respective actions and the offset to make that action
                                                    D -> ([0..5],3)
                                                    L -> ([1,2,4,5,7,8],(-1))
                                                    R -> ([0,1,3,4,6,7],1)

allFutures :: State -> [State]
allFutures state@(board,list) =
    let state1 = if list /= [] && head list == D then state else (concat $ makeMove board U,U : list)   --if else prevents moves going back-and-forth indefinitely
        state2 = if list /= [] && head list == U then state else (concat $ makeMove board D,D : list)
        state3 = if list /= [] && head list == R then state else (concat $ makeMove board L,L : list)
        state4 = if list /= [] && head list == L then state else (concat $ makeMove board R,R : list)
    in filter (\t -> t /= state) $ filter (\s -> fst s /= []) [state1,state2,state3,state4] --outmost filter removes states identical to the paramteter (if ... then state)

possibleSolutions :: Board -> [[State]]
possibleSolutions board = [(board,[])] : [concat (map (\state -> allFutures state) x) | x <- possibleSolutions board]

solve :: Board -> [Action]
solve board = snd $ head $ take 1 $ concat [[x | x <- xs, isSolved $ fst x] | xs <- possibleSolutions board] --returns the proper actions to the first solved board it sees

{-  Examples
[7,2,3,4,0,6,1,8,5] --> [Right,Right,Down,Down,Left,Left,Up,Right,Up,Right,Down,Left,Up,Left,Down,Down,Right,Right,Up,Left]
[0,8,2,1,4,3,7,6,5] --> [Down,Right,Up,Left,Down,Down,Right,Up,Right,Down]
[1,2,3,4,5,6,7,8,0] --> []
[0,1,2,3,4,5,6,7,8] --> [Right,Down,Left,Down,Right,Right,Up,Left,Left,Down,Right,Up,Up,Right,Down,Down,Left,Left,Up,Right,Right,Down]
[3,2,7,0,4,6,5,8,1] --> [Right,Down,Down,Right,Up,Up,Left,Down,Down,Left,Up,Right,Right,Down,Left,Up,Up,Left,Down,Down,Right,Up,Right]
[0,2,3,1,5,6,4,7,8] --> [Right,Right,Down,Down]
[1,2,3,4,0,6,7,5,8] --> [Right,Down]

[8,1,2,0,4,3,7,6,5] --> not possible
-}