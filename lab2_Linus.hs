type Board = [[Int]]
type Position = (Int,Int)
--type State = 

--- TODO explain
data Action = L | U | R | D deriving Eq
instance Show Action where
    show L = "Left"
    show U = "Up"
    show R = "Right"
    show D = "Down"