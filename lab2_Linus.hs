--Task 1
type Board = [[Int]]
type Position = (Int,Int)
--type State = 

--Task 2
--- TODO explain and understand
data Action = L | U | R | D deriving Eq
instance Show Action where
    show L = "Left"
    show U = "Up"
    show R = "Right"
    show D = "Down"