hints <- permutations(3, 5, c('c', 'i', 'n'), repeats.allowed = TRUE)
potential_hints <- str_c(hints[,1], hints[,2], hints[,3], hints[,4], hints[,5])


worst_case_first <- "arise"
most_parts_first <- "trace"
expected_first <- "raise"
entropy_first <- "raise"

load("wordle/target.Rdata")
load("wordle/untarget.Rdata")


check_word <- function(guess_str, solution_str) {
  guess <- strsplit(guess_str, "")[[1]]
  solution <- strsplit(solution_str, "")[[1]]
  
  correct_positions <- guess == solution
  matches <- ifelse(correct_positions, "correct", "not-in-word")
  
  solution_filt <- solution[!correct_positions]
  
  for (i in which(!correct_positions)) {
    if (guess[i] %in% solution_filt) {
      matches[i] <- "in-word"
      solution_filt <- solution_filt[-which(solution_filt == guess[i])[1]]
    }
  }
  
  list(
    word = guess_str,
    letters = guess,
    matches = matches,
    win = all(matches == "correct")
  )
}


filter_solutions <- function(solutions, all_guesses) {
  indices <- seq_along(solutions)
  
  for (guess in all_guesses) {
    potential <- solutions
    
    for (i in which(guess$matches == "correct")) {
      pattern <- "....."
      str_sub(pattern, i, i) <- guess$letters[i]
      
      indices <- intersect(indices, which(str_detect(potential, pattern)))
      str_sub(potential, i, i)[str_detect(potential, pattern)] <- '.'
    }
    
    for (i in which(guess$matches == "in-word")) {
      pattern <- "....."
      letter <- guess$letters[i]
      str_sub(pattern, i, i) <- letter
      
      indices <- intersect(indices, which(!str_detect(potential, pattern)))
      indices <- intersect(indices, which(str_detect(potential, letter)))
      potential <- str_replace(potential, letter, '.')
    }
    
    for (i in which(guess$matches == "not-in-word")) {
      letter <- guess$letters[i]
      indices <- intersect(indices, which(!str_detect(potential, letter)))
    }
  }
  
  return(solutions[indices])
}


string_from_hints <- function(result) {
  paste(str_sub(result$matches, 1, 1), collapse="")
}

