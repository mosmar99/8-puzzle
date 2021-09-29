import Data.List
import Data.Maybe

type Board = [[Int]]
type Position = (Int,Int) -- (x,y)
type State = (Board,[Action])

{-  Custom data type 'Action' which can be either L, U, R or D, using Eq typeclass to print
    their respective values using 'show'-}
data Action = L | U | R | D deriving Eq
--  Create instance of 'Action' and defining the 4 different possible values for 'Action'
instance Show Action where
    show L = "Left"
    show U = "Up"
    show R = "Right"
    show D = "Down"

solution :: Board
solution = [[1,2,3],[4,5,6],[7,8,0]]

isSolved :: Board -> Bool   --tested
isSolved board = concat board == concat solution

findEmpty :: Board -> Position  --tested
findEmpty board
    | index <= 2 = (xPos,0)
    | index <= 5 = (xPos,1)
    | index <= 8 = (xPos,2) 
        where index = fromJust $ findIndex (== 0) (concat board) --always within [0,8]
              xPos = rem index 3 --similar method for yPos??

getValue :: Position -> Board -> Int
getValue (x,y) board = (concat board) !! (x + 3 * y)

replace :: Position -> Int -> Board -> Board    --tested
replace pos n board = reverseConcat (changeValue (getValue pos board) n (concat board))
    where 
        reverseConcat board =   --reverse the effect of concat
            let row1 = take 3 board
                row2 = take 3 $ drop 3 board
                row3 = take 3 $ drop 6 board
            in [row1,row2,row3]
        changeValue value newValue list     --change 'value' to 'newValue' in a list
            | first == [] = foldr (:) [newValue] $ init second
            | otherwise = (foldr (:) [newValue] (init first)) ++ second
                where tuple = splitAt value list
                      first = fst tuple
                      second = snd tuple

swap :: Position -> Position -> Board -> Board  --tested
swap pos1 pos2 board = replace pos2 (getValue pos1 board) $ replace pos1 (getValue pos2 board) board

makeMove :: Board -> Action -> [Board]
makeMove board action = case action of U -> swap zeroPos (zeroPosX,zeroPosY - 1) board   --up
                                       D -> swap zeroPos (zeroPosX,zeroPosY + 1) board   --down
                                       L -> swap zeroPos (zeroPosX - 1,zeroPosY) board   --left
                                       R -> swap zeroPos (zeroPosX + 1,zeroPosY) board   --right
                            where zeroPos@(zeroPosX,zeroPosY) = findEmpty board