# N-Queens
Solving the N-queens problem using the hill-climbing algorithm 

Initially the objective was to solve the very common 8-queens problem which is to place 8 chess queens on an 8x8 chessboard where none of the queens threaten each other. However, there are over 90 solutions to this problem and we needed more of a challange so I made an N-Queens solver. 

Once you open the code you will see a (define N 8), the initial problem is the 8 Queens so we start with 8 but feel free to change this number as you please (anything above 16 will take very long and most likely fail a few times but it can still find a solution). N Queens will be placed on an NxN board and then a solution will be searched for. 

For the N-Queens_hillclimbing file:          
Run the file and simply type (run) in the terminal and press enter.
This will start the search for a solution and if it fails to find one it will decrease the N that was set before till it fails or it will return a solution. The solution will be a list of numbers that represents the position of the Queens in the solution board found. 

For the N-Queens_hillclimbing_withvisuals file:       
Run the file and simply type (run) in the terminal and press enter.
This will start the search for a solution and if it fails to find one it will decrease the N that was set before till it fails or return the image of the solution.
       
** The solution is easier to see and understand with visuals so it is reccomended that you use the withvisuals file. **


