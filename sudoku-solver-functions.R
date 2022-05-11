#
# Functions for sudoku-solver
# dr.j.campbell@gmail.com
# 31st March 2022
#


# Create a matrix of 9x9 0s and write out to a file
create_blank_sudoku <- function(file = "blank_sudoku.txt"){
  m = matrix(rep(0, times = 81), nrow = 9, ncol = 9)
  write.table(m, file = file, sep = "\t", row.names = FALSE, col.names = FALSE)
  return(m)
}

create_sudoku_state = function(in_file){
  # Create a list object with sub-lists to hold:
  # m - the sukdoku matrix to solve
  # blocks - a list of names a-i, each with sub-lists
  #   top_row, bottom_row, left_col, right_col - the boundaries of the block
  #   missing_rows, missing_cols - the rows/cols that need filling
  #   numbers_needed - the values needed to complete the block
  # cols - a list with nine names (1:9), each with sub lists
  #   missing_rows - the rows in the col with values to fill
  #   numbers_needed - the values needed to complete the col
  # rows -a list with nine names (1:9), each with sub lists
  #   missing_cols - the cols in the row with values to fill
  #   numbers_needed - the values needed to complete the row
  sdko = list(
    updated = FALSE,
    m = as.matrix(read.delim(file = in_file, header = FALSE)),
    blocks = list(
      a = list(
        top_row = 1,
        bottom_row = 3,
        left_col = 1,
        right_col = 3,
        missing_rows = NULL,
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      b = list(
        top_row = 1,
        bottom_row = 3,
        left_col = 4,
        right_col = 6,
        missing_rows = NULL,
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      c = list(
        top_row = 1,
        bottom_row = 3,
        left_col = 7,
        right_col = 9,
        missing_rows = NULL,
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      d = list(
        top_row = 4,
        bottom_row = 6,
        left_col = 1,
        right_col = 3,
        missing_rows = NULL,
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      e = list(
        top_row = 4,
        bottom_row = 6,
        left_col = 4,
        right_col = 6,
        missing_rows = NULL,
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      f = list(
        top_row = 4,
        bottom_row = 6,
        left_col = 7,
        right_col = 9,
        missing_rows = NULL,
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      g = list(
        top_row = 7,
        bottom_row = 9,
        left_col = 1,
        right_col = 3,
        missing_rows = NULL,
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      h = list(
        top_row = 7,
        bottom_row = 9,
        left_col = 4,
        right_col = 6,
        missing_rows = NULL,
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      i = list(
        top_row = 7,
        bottom_row = 9,
        left_col = 7,
        right_col = 9,
        missing_rows = NULL,
        missing_cols = NULL,
        numbers_needed = NULL
      )
    ),
    columns = list(
      "1" = list(
        missing_rows = NULL,
        numbers_needed = NULL
      ),
      "2" = list(
        missing_rows = NULL,
        numbers_needed = NULL
      ),
      "3" = list(
        missing_rows = NULL,
        numbers_needed = NULL
      ),
      "4" = list(
        missing_rows = NULL,
        numbers_needed = NULL
      ),
      "5" = list(
        missing_rows = NULL,
        numbers_needed = NULL
      ),
      "6" = list(
        missing_rows = NULL,
        numbers_needed = NULL
      ),
      "7" = list(
        missing_rows = NULL,
        numbers_needed = NULL
      ),
      "8" = list(
        missing_rows = NULL,
        numbers_needed = NULL
      ),
      "9" = list(
        missing_rows = NULL,
        numbers_needed = NULL
      )
    ),
    rows = list(
      "1" = list(
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      "2" = list(
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      "3" = list(
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      "4" = list(
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      "5" = list(
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      "6" = list(
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      "7" = list(
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      "8" = list(
        missing_cols = NULL,
        numbers_needed = NULL
      ),
      "9" = list(
        missing_cols = NULL,
        numbers_needed = NULL
      )
    )
  )
  
  return(sdko)
  
}

check_missing_per_row_col = function(sdko){
  # for each row and column, check wi=hich values need filling (0s)
  # and which numbers are needed
  for(rc in 1:9){
    sdko$rows[[rc]]$missing_cols = NULL
    sdko$rows[[rc]]$numbers_needed = NULL
    sdko$cols[[rc]]$missing_cols = NULL
    sdko$cols[[rc]]$numbers_needed = NULL
    sdko$rows[[rc]]$missing_cols = as.numeric(which(sdko$m[rc,] == 0))
    sdko$rows[[rc]]$numbers_needed = setdiff(1:9, as.numeric(sdko$m[rc,]))
    sdko$columns[[rc]]$missing_rows = as.numeric(which(sdko$m[,rc] == 0))
    sdko$columns[[rc]]$numbers_needed = setdiff(1:9, as.numeric(sdko$m[,rc]))
  }
  return(sdko)
}

check_missing_per_block = function(sdko){
  # check which cells are missing in each block
  # - update the missing_rows and missing_cols info
  # - update the numbers_needed info
  for(block in names(sdko$blocks)){
    sdko$blocks[[block]]$missing_rows = NULL
    sdko$blocks[[block]]$missing_cols = NULL
    sdko$blocks[[block]]$numbers_needed = NULL
    for(row in sdko$blocks[[block]]$top_row:sdko$blocks[[block]]$bottom_row){
      for(col in sdko$blocks[[block]]$left_col:sdko$blocks[[block]]$right_col){
        if(sdko$m[row,col] == 0){
          sdko$blocks[[block]]$missing_rows = c(sdko$blocks[[block]]$missing_rows, row)
          sdko$blocks[[block]]$missing_cols = c(sdko$blocks[[block]]$missing_cols, col)
        }
      }
    }
    # check which numbers are needed
    sdko$blocks[[block]]$numbers_needed = setdiff(1:9, as.numeric(as.matrix(sdko$m[sdko$blocks[[block]]$top_row:sdko$blocks[[block]]$bottom_row,sdko$blocks[[block]]$left_col:sdko$blocks[[block]]$right_col])))
  }
  return(sdko)
}

fill_blocks = function(sdko){
  # check each block for possible numbers to fill
  # - if a single number if needed - it can be filled withouit further checks
  # - skip if no rows (or cols, implied) need filling
  # - for each number_needed, check rows and columns for that number
  #   - if the number is absent from a row/col, add it to the possible row/cols
  # - if only one possible row/cell exists after all checks, fill it in
  # 
  for(block in names(sdko$blocks)){
    search_block_again = TRUE
    while(search_block_again){
      search_block_again = FALSE
      tr = sdko$blocks[[block]]$top_row
      br = sdko$blocks[[block]]$bottom_row
      lc = sdko$blocks[[block]]$left_col
      rc = sdko$blocks[[block]]$right_col
      # if only a single value is needed, we can fill it in and move on
      if(length(sdko$blocks[[block]]$numbers_needed) == 1){
        sdko$m[tr:br,lc:rc][which(sdko$m[tr:br,lc:rc] == 0)] = sdko$blocks[[block]]$numbers_needed
        sdko$blocks[[block]]$missing_rows = NULL
        sdko$blocks[[block]]$missing_cols = NULL
        sdko$blocks[[block]]$numbers_needed = NULL
      }
      if(length(sdko$blocks[[block]]$missing_rows) == 0){
        next
      }
      # T/F repeat the process - T if we find a value to update
      
      for(needed_number in sdko$blocks[[block]]$numbers_needed){
        # we should probably re-check the status of what is needed and empty here...
        sdko = check_missing_per_block(sdko)
        
        # print(paste0("needed number:", needed_number))
        blank_rows = sdko$blocks[[block]]$missing_rows
        blank_cols = sdko$blocks[[block]]$missing_cols
        possible_rows = NULL
        possible_cols = NULL
        # print(paste0("blank_rows is:", blank_rows, " blank_cols is:", blank_cols))
        if(is.null(blank_rows)){
          # print(paste0("blank_rows is null... skipping"))
          next
        }
        for(i in 1:length(blank_rows)){
          if(needed_number %in% sdko$m[blank_rows[i],] | needed_number %in% sdko$m[,blank_cols[i]]){
            #print(paste0("skipping:", blank_rows[i], " ", blank_cols[i], " - contains:", needed_number))
            next
          }else{
            #print(paste0("Adding possible number - block:", block, " i:", i, " blank_rows:", blank_rows[i], " blank_cols:", blank_cols[i]))
            possible_rows = c(possible_rows, blank_rows[i])
            possible_cols = c(possible_cols, blank_cols[i])
          }
        }
        if(!is.null(possible_rows)){
          if(length(possible_rows) == 1){
            if(!is.na(possible_rows) & !is.na(possible_cols)){
              print(paste0("Mising cell filled: ", "block:", block, " row:", possible_rows, " col:", possible_cols, " possibilities:", length(possible_rows), " filled value:", needed_number))
              #print(paste0("UPDATE MATRIX - block is:", block, " possible_rows is:", possible_rows, " possible_cols is:", possible_cols, " length of possible_rows is: ", length(possible_rows) ))
              # we found a missing value to fill in
              # update the matrix
              # remove from missing_rows, missing_cols, numbers_needed
              sdko$m[possible_rows, possible_cols] = needed_number
              
              sdko$blocks[[block]]$missing_rows = sdko$blocks[[block]]$missing_rows[-which(sdko$blocks[[block]]$missing_rows == possible_rows)]
              
              sdko$blocks[[block]]$missing_cols = sdko$blocks[[block]]$missing_cols[-which(sdko$blocks[[block]]$missing_cols == possible_cols)]
              
              sdko$blocks[[block]]$numbers_needed = sdko$blocks[[block]]$numbers_needed[-which(sdko$blocks[[block]]$numbers_needed == needed_number)]
              search_block_again = TRUE
            }
          }
        }
      }
    }
  }
  return(sdko)
}

fill_rows = function(sdko){
  # check rows for numbers to fill
  # - if none remain, skip or if one remains, fill it in
  # - for each number needed, check cols
  #   - if the missing cell intersects a block  with that number skip (set inc_exc to exclude)
  #   - if the number needed is not in the col, include it in the possibilities
  # - if one potential col remains, fill it in
  for(row in names(sdko$rows)){
    search_row_again = TRUE
    while(search_row_again){
      search_row_again = FALSE
      if(length(sdko$rows[[row]]$numbers_needed) == 0){
        next
      }
      if(length(sdko$rows[[row]]$numbers_needed) == 1){
        sdko$m[as.numeric(row),as.numeric(sdko$rows[[row]]$missing_cols)] = as.numeric(sdko$rows[[row]]$numbers_needed)
        print(paste0("Mising row filled as only possibility: ", " row:", row, " col:", sdko$rows[[row]]$missing_cols, " filled value:", sdko$rows[[row]]$numbers_needed))
        next
      }
      for(needed_number in sdko$rows[[row]]$numbers_needed){
        blank_cols = sdko$rows[[row]]$missing_cols
        potential_cols = NULL
        for(blank_col in blank_cols){
          # does the block this row/col intersects contain the
          # missing number? if so - exclude from potential_cols
          inc_exc = find_row_col_block_intersects(sdko, row, blank_col, needed_number)
          
          # is blank col missing the number?
          if(!(needed_number %in% sdko$m[,blank_col]) & inc_exc == "include"){
            potential_cols = c(potential_cols, blank_col)
          }
        }
        if(length(potential_cols) == 1){
          if(!is.na(potential_cols)){ # This is a horrible hack - why are there NAs?
            print(paste0("Mising row filled through exclusion: ", " row:", row, " col:", potential_cols, " filled value:", needed_number))
            sdko$m[as.numeric(row),as.numeric(potential_cols)] = needed_number
            sdko = check_missing_per_row_col(sdko)
            search_row_again = TRUE
          }
        }
      }
    }
  }
  return(sdko)
}

fill_cols = function(sdko){
  # check cols for numbers to fill
  # see comment at the start of fill_rows
  # these functions should be merged and an arg added
  #  to indicate if rows or cols are being checked
  for(col in names(sdko$columns)){
    search_row_again = TRUE
    while(search_row_again){
      search_row_again = FALSE
      if(length(sdko$cols[[col]]$numbers_needed) == 0){
        next
      }
      if(length(sdko$cols[[col]]$numbers_needed) == 1){
        sdko$m[as.numeric(sdko$cols[[col]]$missing_row), as.numeric(col)] = as.numeric(sdko$cols[[col]]$numbers_needed)
        print(paste0("Mising col filled as only possibility: ", " row:", sdko$cols[[col]]$missing_rows, " col:", col, " filled value:", sdko$cols[[col]]$numbers_needed))
        next
      }
      for(needed_number in sdko$cols[[col]]$numbers_needed){
        blank_row = sdko$cols[[col]]$missing_rows
        potential_rows = NULL
        for(blank_row in blank_rows){
          # does the block this row/col intersects contain the
          # missing number? if so - exclude from potential_cols
          inc_exc = find_row_col_block_intersects(sdko, blank_row, col, needed_number)
          
          # is blank row missing the number?
          if(!(needed_number %in% sdko$m[,blank_row]) & inc_exc == "include"){
            potential_rows = c(potential_rows, blank_row)
          }
        }
        if(length(potential_rows) == 1){
          if(!is.na(potential_rows)){ # This is a horrible hack - why are there NAs? - maybe because data wasn't updated after filling in values
            print(paste0("Mising col filled through exclusion: ", " row:", potential_rows, " col:", col, " filled value:", needed_number))
            sdko$m[as.numeric(potential_rows),as.numeric(col)] = needed_number
            sdko = check_missing_per_row_col(sdko)
            search_row_again = TRUE
          }
        }
      }
    }
  }
  return(sdko)
}

count_missing_cells = function(sdko){
  # Count missing cells in the sudoku matrix
  num_missing = length(which(sdko$m == 0))
  #print("Missing values: ", as.character(num_missing))
  sdko$num_missing = num_missing
  return(sdko)
}

find_row_col_block_intersects = function(sdko, row = NULL, column = NULL, needed = NULL){
  # take a row,column and find the square it belongs to
  # if that square contains the number we are looking for
  # That row cell can be excluded from the possibilities
  if(is.null(row) | is.null(column) | is.null(needed)){
    warn("You need to supply row, col and needed...")
    return(NULL)
  }
  # check everything is up to date block-wise
  #sdko = check_missing_per_block(sdko)
  for(block in names(sdko$blocks)){
    tr = sdko$blocks[[block]]$top_row
    br = sdko$blocks[[block]]$bottom_row
    lc = sdko$blocks[[block]]$left_col
    rc = sdko$blocks[[block]]$right_col
    if(row >= tr & row <= br & column >= lc & column <= rc){
      found_numbers = as.character(sdko$m[tr:br,lc:rc])
      if(needed %in% found_numbers){
        return("exclude")
      } else {
        return("include")
      }
    }
  }
}

solve_sudoku = function(sdko){
  # combine the functions to try to solve a sudoku
  # 
  try_again = 10
  while(try_again > 0){
    
    sdko = count_missing_cells(sdko)
    previous_missing_cells = sdko$num_missing
    sdko = check_missing_per_block(sdko)
    sdko = fill_blocks(sdko)
    sdko = check_missing_per_row_col(sdko)
    sdko = fill_rows(sdko)
    sdko = check_missing_per_row_col(sdko)
    sdko = fill_cols(sdko)
    sdko = count_missing_cells(sdko)
    try_again = try_again - 1
    if(sdko$num_missing < previous_missing_cells){
      try_again = 10
      print("reset try_again")
    }
    
    print(paste0("try again is now ", try_again))
  }
  return(sdko)
}

