# sudoku-solver
R scripts for solving sudoku puzzles

Open the sudoku-solver.Rmd in Rstudio to run the code.

The sudoku-solver-functions.R contains functions that are sourced and then accessed via two main functions:
* create_sudoku_state("metro_easy_220401.txt")
* solve_sudoku(sdko)

The result of these functions are both stored in a list object called sdko.

This should be able to solve any Metro sudoku but fails with the Evening Standard ones I have tried.

To add a new sudoku, open the 'blank' sudoku text file. You should have a 9x9 tab-separated values file containing a matrix of zeros. Change the known number (from 0 to 1:9) and save the file with a meaningful name. 

sdko = create_sudoku_state("metro_easy_220401.txt")
sdko = solve_sudoku(sdko)


