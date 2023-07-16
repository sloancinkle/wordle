get_potential <- function(all_guesses, valid_guess) {
  # Determine whether we have converged on a solution
  potential_solutions <- filter_solutions(word_list, all_guesses)
  
  # Set potential guesses depending on valid_guess
  if (valid_guess) {potential_guesses <- potential_solutions} 
  else {potential_guesses <- word_list}
  
  return(list("guesses" = potential_guesses,
              "solutions" = potential_solutions))
}


random_guess <- function(all_guesses, valid_guess) {
  potential <- get_potential(all_guesses, valid_guess)
  return(sample(potential$guesses, 1))
}


partition <- function(potential_guesses, potential_solutions) {
  partition_list <- lapply(potential_guesses, function(guess) {
    results <- lapply(potential_solutions, function(solution) {
      result <- check_word(guess, solution)
      return(string_from_hints(result))
    })
    
    counts <- plyr::count(unlist(results))
    names(counts) <- c("x", guess)
    return(counts)
  })
  
  partition_table <- purrr::reduce(partition_list, merge, by="x", all=TRUE)
  rownames(partition_table) <- partition_table[["x"]]
  partition_table <- subset(partition_table, select=-x)
  partition_table[is.na(partition_table)] <- 0
  
  return(partition_table)
}


worst_case <- function(all_guesses, valid_guess=TRUE) {
  # Shortcut on the first iteration of the algorithm
  if (length(all_guesses) < 1) {
    return(worst_case_first)
  }
  
  # Shortcut on the second iteration of the unvalid_guessed algorithm
  if (length(all_guesses) < 2 & all_guesses[[1]]$word == worst_case_first) {
    hints <- string_from_hints(all_guesses[[1]])
    if (valid_guess) {
      return(unlist(target$worst_case[target$matches==hints]))
    } else {
      return(unlist(untarget$worst_case[untarget$matches==hints]))
    }
  }
  
  # Determine whether we have converged on a solution
  potential <- get_potential(all_guesses, valid_guess)
  if (length(potential$solutions) < 2) return(potential$solutions[1])
  
  # Worst case algorithm
  parts <- partition(potential$guesses, potential$solutions)
  worst <- apply(parts, 2, max)
  return(names(worst)[which.min(worst)])
}


most_parts <- function(all_guesses, valid_guess) {
  # Shortcut on the first iteration of the algorithm
  if (length(all_guesses) < 1) return(most_parts_first)
  
  # Shortcut on the second iteration of the unvalid_guessed algorithm
  if (length(all_guesses) < 2 & all_guesses[[1]]$word == most_parts_first) {
    hints <- string_from_hints(all_guesses[[1]])
    if (valid_guess) {
      return(unlist(target$most_parts[target$matches==hints]))
    } else {
      return(unlist(untarget$most_parts[untarget$matches==hints]))
    }
  }
  
  # Determine whether we have converged on a solution
  potential <- get_potential(all_guesses, valid_guess)
  if (length(potential$solutions) < 2) return(potential$solutions[1])
  
  # Most parts algorithm
  parts <- partition(potential$guesses, potential$solutions)
  zeros <- colSums(parts == 0)
  return(names(zeros)[which.min(zeros)])
}


expected <- function(all_guesses, valid_guess) {
  # Shortcut on the first iteration of the algorithm
  if (length(all_guesses) < 1) return(expected_first)
  
  # Shortcut on the second iteration of the unvalid_guessed algorithm
  if (length(all_guesses) < 2 & all_guesses[[1]]$word == expected_first) {
    hints <- string_from_hints(all_guesses[[1]])
    if (valid_guess) {
      return(unlist(target$expected[target$matches==hints]))
    } else {
      return(unlist(untarget$expected[untarget$matches==hints]))
    }
  }
  
  # Determine whether we have converged on a solution
  potential <- get_potential(all_guesses, valid_guess)
  if (length(potential$solutions) < 2) return(potential$solutions[1])
  
  # Expected algorithm
  parts <- partition(potential$guesses, potential$solutions)
  size <- apply(parts, 2, function(col) return(sum(col ^ 2 / sum(col))))
  return(names(size)[which.min(size)])
}


entropy <- function(all_guesses, valid_guess) {
  # Shortcut on the first iteration of the algorithm
  if (length(all_guesses) < 1) return(entropy_first)
  
  # Shortcut on the second iteration of the unvalid_guessed algorithm
  if (length(all_guesses) < 2 & all_guesses[[1]]$word == entropy_first) {
    hints <- string_from_hints(all_guesses[[1]])
    if (valid_guess) {
      return(unlist(target$entropy[target$matches==hints]))
    } else {
      return(unlist(untarget$entropy[untarget$matches==hints]))
    }
  }
  
  # Determine whether we have converged on a solution
  potential <- get_potential(all_guesses, valid_guess)
  if (length(potential$solutions) < 2) return(potential$solutions[1])
  
  # Entropy algorithm
  parts <- partition(potential$guesses, potential$solutions)
  ntrpy <- apply(parts, 2, function(col) {
    p_col <- col / sum(col)
    log_p_col <- p_col * log(p_col, base=2)
    return(-sum(log_p_col[!is.na(log_p_col)]))
  })
  return(names(ntrpy)[which.max(ntrpy)])
}

