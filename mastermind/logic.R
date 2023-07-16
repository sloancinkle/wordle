potential_hints <- c("0,0", "0,1", "0,2", "0,3", "0,4", "1,0", "1,1",
                     "1,2", "1,3", "2,0", "2,1", "2,2", "3,0", "4,0")


worst_case_first <- "bbgg"
most_parts_first <- "bbgk"
expected_first <- "bbgk"
entropy_first <- "bgkr"

load("mastermind/target.Rdata")
load("mastermind/untarget.Rdata")


check_word <- function(guess_str, solution_str) {
  guess <- strsplit(guess_str, "")[[1]]
  solution <- strsplit(solution_str, "")[[1]]
  
  correct_positions <- guess == solution
  correct <- sum(correct_positions)
  in_word <- 0
  
  guess_filt <- guess[!correct_positions]
  solution_filt <- solution[!correct_positions]
  
  for (i in seq_along(guess_filt)) {
    if (guess_filt[i] %in% solution_filt) {
      in_word <- in_word + 1
      solution_filt <- solution_filt[-which(solution_filt == guess_filt[i])[1]]
    }
  }
  
  list(
    word = guess_str,
    letters = guess,
    correct = correct,
    in_word = in_word,
    win = correct == 4
  )
}


filter_solutions <- function(solutions, all_guesses) {
  truth_values <- lapply(solutions, function(guess) {
    for (i in seq_along(all_guesses)) {
      valid <- check_word(all_guesses[[i]]$word, guess)
      if (valid$correct != all_guesses[[i]]$correct | 
          valid$in_word != all_guesses[[i]]$in_word) return(FALSE)
    }
    return(TRUE)
  })
  
  return(solutions[unlist(truth_values)])
}


string_from_hints <- function(result) {
  paste(result$correct, result$in_word, sep=",")
}

