---
title: "Sudoku solver"
author: "james.campbell@crick.ac.uk"
date: '2022-03-31'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
setwd("~/Documents/git/sudoku-solver/")
```


# Ideas

* create a megalist at the start to hold all info
 - m
 - missing
 - blocks with blanks etc
 

# Strategies

Blocks of nine (3 by 3) cells are labeled A-I from top-left to bottom right.

For any block of nine, find the lowest missing number in the range 1:9.

Check each cell with 0 to see if that number exists in either of the rows or columns for other cells - if it does, eliminate it from the possibilities. If only a single 0 remains after all checks, fill in that missing number

If a row or column has fewer than three 0 values, check through the missing values starting with the lowest. If any of the 0 cells can be eliminated either because the row or column contains that number - or if any can be elimiated becuase the 3-by-3 block they are in contains that number, remove them from the options. If only one option remains, fill in the missing number.



```{r}
setwd("~/Documents/git/sudoku-solver/")
source("sudoku-solver-functions.R")

# Metro Easy 220401
sdko = create_sudoku_state("metro_easy_220401.txt")
sdko = solve_sudoku(sdko)
sdko$num_missing

# Metro Easy 220506
sdko = create_sudoku_state("metro_easy_220506.txt")
sdko = solve_sudoku(sdko)
sdko$num_missing

# Metro Challenging 220506
sdko = create_sudoku_state("metro_challenging_220506.txt")
sdko = solve_sudoku(sdko)
sdko$num_missing



sdko = create_sudoku_state("ES_220509.txt")
sdko = solve_sudoku(sdko)
sdko$num_missing
sdko$m

sdko = create_sudoku_state("ES_220413.txt")
sdko = solve_sudoku(sdko)
sdko$num_missing


sdko = create_sudoku_state("ES_220404.txt")
sdko = solve_sudoku(sdko)
sdko$num_missing
sdko$m

```


