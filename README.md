# 8-puzzle
Creata a haskell representation of a eight puzzle

# Project Description
The project treats an eight-game. An eight-game is a simplified variant of a fifteen-game. The difference in comparison to the fifteen game is that an eight game consists of a 3x3 board with eight movable tiles while a fifteen game consists of a 4x4 board with fifteen movable pieces.

The goal of an eight game is to move around the 8 tiles so that they are sorted from 1 to 8, followed
by the empty space. One way to represent the movement of the tiles is to focus on how the empty box moves. In this way we can define a sequence of different tile moves by describing how the empty space moves during the game.

# Sample move
Below is a 3D representation of the game. We move the empty piece one square to the right. Then we get:

- X X X X X X 0 X X        

- X X X X X X X 0 X

# Game completion
In order to complete the game, we just have to move the empty brick on more square to the right. I.e:

- X X X X X X X 0 X          

- 7 X X X X X X X X 0

# File information
* The project is Lisenced under the MIT Lisence. 
* labTwo.hs is authored by mosmar99
* lab2_Linus.hs is authored by Linkan42