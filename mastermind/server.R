word_list <- permutations(6, 4, c('r', 'b', 'y', 'g', 'w', 'k'), 
                          repeats.allowed = TRUE)
word_list <- str_c(word_list[,1], word_list[,2], word_list[,3], word_list[,4])


server <- function(input, output, session) {
  solution <- reactiveVal(sample(word_list, 1))
  all_guesses <- reactiveVal(list())
  current_guess <- reactiveVal(character())
  finished <- reactiveVal(FALSE)
  max_turns <- reactiveVal(10)
  
  details <- reactiveVal(FALSE)
  option <- reactiveVal(FALSE)
  exit_simulation <- reactiveVal(TRUE)
  
  algorithm <- reactiveVal(character())
  iteration <- reactiveVal(1)
  
  solution_count <- reactiveVal(length(word_list))
  game_spaces <- reactiveVal(integer())
  all_spaces <- reactiveVal(integer())
  
  all_scores <- reactiveVal(integer())
  
  reset_game <- function() {
    solution(sample(word_list, 1))
    all_guesses(list())
    finished(FALSE)
    current_guess(character())
    solution_count(c(solution_count(), length(word_list)))
    game_spaces(integer())
  }
  
  reset_simulation <- function() {
    reset_game()
    iteration(1)
    all_scores(integer())
    solution_count(length(word_list))
    all_spaces(integer())
  }
  
  observeEvent(input$Enter, {
    guess <- paste(current_guess(), collapse = "")
    
    if (! guess %in% word_list)
      return()
    
    check_result <- check_word(guess, solution())
    all_guesses(append(all_guesses(), list(check_result)))
    
    potential_solutions <- filter_solutions(word_list, all_guesses())
    last_solution_count <- solution_count()[length(solution_count())]
    reduced_space <- 1 - length(potential_solutions) / last_solution_count
    
    solution_count(c(solution_count(), length(potential_solutions)))
    game_spaces(c(game_spaces(), reduced_space))
    
    if (isTRUE(check_result$win) | length(all_guesses()) >= max_turns()) {
      finished(TRUE)
      all_spaces(c(all_spaces(), mean(game_spaces())))
    }
    
    current_guess(character(0))
  })
  
  output$previous_guesses <- renderUI({
    lapply(all_guesses(), function(guess) {
      row <- mapply(guess$letters, FUN=function(letter) {
        div("", class = paste("letter", letter))
      }, SIMPLIFY = FALSE, USE.NAMES = FALSE
      )
      hints <- paste(guess$correct, guess$in_word, sep="\n")
      div(class = "word", append(row, list(div(hints, class="hints"))))
    })
  })
  
  output$current_guess <- renderUI({
    if (finished()) return()
    
    letters <- current_guess()
    
    solution_length <- isolate(nchar(solution()))
    if (length(letters) < solution_length) {
      letters[(length(letters)+1) : solution_length] <- ""
    }
    
    blank_guess <- lapply(letters, function(letter) {
      div("", class=paste("letter", letter, "guess"))
    })
    div(class = "word", append(blank_guess, list(div("", class="hints"))))
  })
  
  output$new_game_ui <- renderUI({
    if (!finished() | option())
      return()
    actionButton("new_game", "New Game")
  })
  
  observeEvent(input$new_game, {
    reset_game()
  })
  
  
  keys <- list(
    c("r", "b", "y"),
    c("g", "w", "k"),
    c("Enter", "Back")
  )
  
  output$keyboard <- renderUI({
    keyboard <- lapply(keys, function(row) {
      row_keys <- lapply(row, function(key) {
        class <- "key"
        if (key %in% c("Enter", "Back")) {
          class <- c(class, "wide-key")
          label <- key
        } else {
          class <- c(class, key)
          label <- ""
        }
        actionButton(key, label, class = class)
      })
      div(class = "keyboard-row", row_keys)
    })
    
    div(class = "keyboard", keyboard)
  })
  
  # Add listeners for each key, except Enter and Back
  lapply(unlist(keys, recursive = FALSE), function(key) {
    if (key %in% c("Enter", "Back")) return()
    observeEvent(input[[key]], {
      if (finished())
        return()
      cur <- current_guess()
      if (length(cur) >= isolate(nchar(solution())))
        return()
      current_guess(c(cur, tolower(key)))
    })
  })
  
  observeEvent(input$Back, {
    if (length(current_guess()) > 0) {
      current_guess(current_guess()[-length(current_guess())])
    }
  })
  
  output$endgame <- renderUI({
    if (!finished() | option())
      return()
    
    line <- vapply(unlist(strsplit(solution(), "")), function(match) {
      switch(match, "r" = "🟥", "b" = "🟦", "y" = "🟨", 
             "g" = "🟩", "w" = "⬜", "k" = "⬛")
    }, character(1))
    
    div(class = "endgame-content", str_c(line, collapse=""))
  })
  
  output$game_details <- renderUI({
    if (!details()) return()
    
    if (length(all_guesses()) < 1) {
      if (option()) return()
      return(div(class="descriptive", HTML("Make a guess to show details.")))
    }
    
    last_solution_count <- solution_count()[length(solution_count())-1]
    this_solution_count <- solution_count()[length(solution_count())]
    reduced_count <- last_solution_count - this_solution_count
    
    line <- paste("<b>", this_solution_count, 
                  "</b> potential solutions remain.<br>",
                  "Eliminated <b>", reduced_count, "</b> of <b>", 
                  last_solution_count, "</b> solutions.",
                  "<br>Reduced solution space: <b>",
                  round(100 * game_spaces()[length(game_spaces())], 1),
                  "%</b>", sep="")
    
    div(class = "descriptive", HTML(line))
  })
  
  observeEvent(input$reset_game, {
    reset_game()
  })
  
  output$score_statistics <- renderUI({
    if (length(all_scores()) < 1) return()
    
    line <- paste("Median score: <b>", round(median(all_scores()), 2), 
                  "</b><br/>Average score: <b>", round(mean(all_scores()), 2), 
                  "</b>", sep="")
    
    if (length(all_scores()) > 1)
      line <- paste(line, "<br/>Standard deviation: <b>", 
                    round(sd(all_scores()), 2), "</b>", sep="")
    
    div(class = "descriptive", HTML(line))
  })
  
  output$space_statistics <- renderUI({
    if (length(all_spaces()) < 1) return()
    
    line <- paste("Median reduced space: <b>", 
                  round(100 * median(all_spaces()), 1), 
                  "%</b><br/>Average reduced space: <b>", 
                  round(100 * mean(all_spaces()), 1), "%</b>", sep="")
    
    if (length(all_spaces()) > 1)
      line <- paste(line, "<br/>Standard deviation: <b>", 
                    round(100 * sd(all_spaces()), 1), "%</b>", sep="")
    
    div(class = "descriptive", HTML(line))
  })
  
  output$score_histogram <- renderPlot({
    if (iteration() < 2) return()
    
    score_df <- data.frame("score" = all_scores())
    
    score_plot <- ggplot(data=score_df, mapping=aes(score)) +
      geom_histogram(aes(y=..density..), breaks=-1:max_turns()+1, 
                     fill ="white", col="black") +
      scale_x_continuous(breaks=0:max_turns()+.5,
                         labels=c(1:max_turns(), "L")) +
      scale_y_continuous("Density", sec.axis=sec_axis(
        trans = ~ . * nrow(score_df), name = "Counts")) +
      labs(x="Score", y="Frequency",
           title=paste("Scores for", iteration() - 1, 
                       ifelse(iteration() == 2, "simulation", "simulations"),
                       "of the", algorithm(), "algorithm on Mastermind")) +
      theme(plot.margin = unit(c(1,0,0,0), "cm")) 
    
    if (iteration() < 3) return(score_plot)
    
    color_scale_values <- c()
    
    if ("Chi-Square" %in% input$distribution) {
      chi_sq <- fitdist(score_df$score, "chisq", start=list(df=1))
      score_plot <- score_plot +
        stat_function(mapping = aes(col="Chi-Square"), fun = dchisq, 
                      args = list(df=chi_sq$estimate["df"]))
      color_scale_values <- c(color_scale_values, "Chi-Square"="palevioletred")
    }
    
    if ("Exponential" %in% input$distribution) {
      exponential <- fitdist(score_df$score, "exp")
      score_plot <- score_plot +
        stat_function(mapping = aes(col="Exponential"), fun = dexp, 
                      args = list(rate=exponential$estimate["rate"]))
      color_scale_values <- c(color_scale_values, "Exponential"="darkslateblue")
    }
    
    if ("F" %in% input$distribution) {
      f_dist <- fitdist(score_df$score, "f", start=list(df1=1, df2=1))
      score_plot <- score_plot +
        stat_function(mapping = aes(col="F"), fun = df, 
                      args = list(df1=f_dist$estimate["df1"],
                                  df2=f_dist$estimate["df2"]))
      color_scale_values <- c(color_scale_values, "F"="khaki2")
    }
    
    if ("Gamma" %in% input$distribution) {
      gamma <- fitdist(score_df$score, "gamma")
      score_plot <- score_plot +
        stat_function(mapping = aes(col="Gamma"), fun = dgamma, 
                      args = list(shape=gamma$estimate["shape"],
                                  rate=gamma$estimate["rate"]))
      color_scale_values <- c(color_scale_values, "Gamma"="chartreuse4")
    }
    
    if ("Logistic" %in% input$distribution) {
      logis <- fitdist(score_df$score, "logis")
      score_plot <- score_plot +
        stat_function(mapping = aes(col="Logistic"), fun = dlogis, 
                      args = list(location=logis$estimate["location"],
                                  scale=logis$estimate["scale"]))
      color_scale_values <- c(color_scale_values, "Logistic"="darkturquoise")
    }
    
    if ("Log-Normal" %in% input$distribution) {
      lognorm <- fitdist(score_df$score, "lnorm")
      score_plot <- score_plot +
        stat_function(mapping = aes(col="Log-Normal"), fun = dlnorm, 
                      args = list(meanlog=lognorm$estimate["meanlog"],
                                  sdlog=lognorm$estimate["sdlog"]))
      color_scale_values <- c(color_scale_values, "Log-Normal"="deeppink4")
    }
    
    if ("Normal" %in% input$distribution) {
      normal <- fitdist(score_df$score, "norm")
      score_plot <- score_plot +
        stat_function(mapping = aes(col="Normal"), fun = dnorm, 
                      args = list(mean=normal$estimate["mean"],
                                  sd=normal$estimate["sd"]))
      color_scale_values <- c(color_scale_values, "Normal"="aquamarine4")
    }
    
    if ("Uniform" %in% input$distribution) {
      uniform <- fitdist(score_df$score, "unif")
      score_plot <- score_plot +
        stat_function(mapping = aes(col="Uniform"), fun = dunif, 
                      args = list(min=uniform$estimate["min"],
                                  max=uniform$estimate["max"]))
      color_scale_values <- c(color_scale_values, "Uniform"="yellow4")
    }
    
    if ("Weibull" %in% input$distribution) {
      weibull <- fitdist(score_df$score, "weibull")
      score_plot <- score_plot +
        stat_function(mapping = aes(col="Weibull"), fun = dweibull, 
                      args = list(shape=weibull$estimate["shape"],
                                  scale=weibull$estimate["scale"]))
      color_scale_values <- c(color_scale_values, "Weibull"="mediumorchid")
    }
    
    if (length(color_scale_values) > 0) {
      score_plot <- score_plot +
        scale_color_manual(name="Distribution", values=color_scale_values)
    }
    
    score_plot
  })
  
  output$space_histogram <- renderPlot({
    if (iteration() < 2) return()
    space_df <- data.frame("space" = all_spaces())
    
    space_plot <- ggplot(data=space_df, mapping=aes(space)) +
      geom_histogram(aes(y=..density..), bins=14, 
                     fill ="white", col="black") +
      scale_x_continuous(labels = scales::percent) +
      scale_y_continuous("Density", sec.axis=sec_axis(
        trans = ~ . * nrow(space_df), name = "Counts")) +
      labs(x="Average reduced solution space per guess", y="Frequency",
           title=paste("Reduced solution space for", iteration() - 1, 
                       ifelse(iteration() == 2, "simulation", "simulations"),
                       "of the", algorithm(), "algorithm on Mastermind")) +
      theme(plot.margin = unit(c(1,0,0,0), "cm")) 
    
    if (iteration() < 3) return(space_plot)
    
    color_scale_values <- c()
    
    if ("Chi-Square" %in% input$distribution) {
      chi_sq <- fitdist(space_df$space, "chisq", start=list(df=1))
      space_plot <- space_plot +
        stat_function(mapping = aes(col="Chi-Square"), fun = dchisq, 
                      args = list(df=chi_sq$estimate["df"]))
      color_scale_values <- c(color_scale_values, "Chi-Square"="palevioletred")
    }
    
    if ("Exponential" %in% input$distribution) {
      exponential <- fitdist(space_df$space, "exp")
      space_plot <- space_plot +
        stat_function(mapping = aes(col="Exponential"), fun = dexp, 
                      args = list(rate=exponential$estimate["rate"]))
      color_scale_values <- c(color_scale_values, "Exponential"="darkslateblue")
    }
    
    if ("F" %in% input$distribution) {
      f_dist <- fitdist(space_df$space, "f", start=list(df1=1, df2=1))
      space_plot <- space_plot +
        stat_function(mapping = aes(col="F"), fun = df, 
                      args = list(df1=f_dist$estimate["df1"],
                                  df2=f_dist$estimate["df2"]))
      color_scale_values <- c(color_scale_values, "F"="khaki2")
    }
    
    if ("Gamma" %in% input$distribution) {
      gamma <- fitdist(space_df$space, "gamma")
      space_plot <- space_plot +
        stat_function(mapping = aes(col="Gamma"), fun = dgamma, 
                      args = list(shape=gamma$estimate["shape"],
                                  rate=gamma$estimate["rate"]))
      color_scale_values <- c(color_scale_values, "Gamma"="chartreuse4")
    }
    
    if ("Logistic" %in% input$distribution) {
      logis <- fitdist(space_df$space, "logis")
      space_plot <- space_plot +
        stat_function(mapping = aes(col="Logistic"), fun = dlogis, 
                      args = list(location=logis$estimate["location"],
                                  scale=logis$estimate["scale"]))
      color_scale_values <- c(color_scale_values, "Logistic"="darkturquoise")
    }
    
    if ("Log-Normal" %in% input$distribution) {
      lognorm <- fitdist(space_df$space, "lnorm")
      space_plot <- space_plot +
        stat_function(mapping = aes(col="Log-Normal"), fun = dlnorm, 
                      args = list(meanlog=lognorm$estimate["meanlog"],
                                  sdlog=lognorm$estimate["sdlog"]))
      color_scale_values <- c(color_scale_values, "Log-Normal"="deeppink4")
    }
    
    if ("Normal" %in% input$distribution) {
      normal <- fitdist(space_df$space, "norm")
      space_plot <- space_plot +
        stat_function(mapping = aes(col="Normal"), fun = dnorm, 
                      args = list(mean=normal$estimate["mean"],
                                  sd=normal$estimate["sd"]))
      color_scale_values <- c(color_scale_values, "Normal"="aquamarine4")
    }
    
    if ("Uniform" %in% input$distribution) {
      uniform <- fitdist(space_df$space, "unif")
      space_plot <- space_plot +
        stat_function(mapping = aes(col="Uniform"), fun = dunif, 
                      args = list(min=uniform$estimate["min"],
                                  max=uniform$estimate["max"]))
      color_scale_values <- c(color_scale_values, "Uniform"="yellow4")
    }
    
    if ("Weibull" %in% input$distribution) {
      weibull <- fitdist(space_df$space, "weibull")
      space_plot <- space_plot +
        stat_function(mapping = aes(col="Weibull"), fun = dweibull, 
                      args = list(shape=weibull$estimate["shape"],
                                  scale=weibull$estimate["scale"]))
      color_scale_values <- c(color_scale_values, "Weibull"="mediumorchid")
    }
    
    if (length(color_scale_values) > 0) {
      space_plot <- space_plot +
        scale_color_manual(name="Distribution", values=color_scale_values)
    }
    
    space_plot
  })
  
  output$score_params <- renderUI({
    if (length(all_scores()) < 3 | length(input$distribution) < 1) return()
    
    line <- ""
    
    if ("Chi-Square" %in% input$distribution) {
      chi_sq <- fitdist(all_scores(), "chisq", start=list(df=1))
      line <- paste(line, "Chi-Square df:", round(chi_sq$estimate["df"], 2), "<br>",
                    "Chi-Square BIC:", round(chi_sq$bic, 3), "<br><br>")
    }
    
    if ("Exponential" %in% input$distribution) {
      exponential <- fitdist(all_scores(), "exp")
      line <- paste(line, "Exponential rate:", round(exponential$estimate["rate"], 2), "<br>",
                    "Exponential BIC:", round(exponential$bic, 3), "<br><br>")
    }
    
    if ("F" %in% input$distribution) {
      f_dist <- fitdist(all_scores(), "f", start=list(df1=1, df2=1))
      line <- paste(line, "F df1:", round(f_dist$estimate["df1"], 2), "<br>",
                    "F df2:", round(f_dist$estimate["df2"], 2), "<br>",
                    "F BIC:", round(f_dist$bic, 3), "<br><br>")
    }
    
    if ("Gamma" %in% input$distribution) {
      gamma <- fitdist(all_scores(), "gamma")
      line <- paste(line, "Gamma shape:", round(gamma$estimate["shape"], 2), "<br>",
                    "Gamma rate:", round(gamma$estimate["rate"], 2), "<br>",
                    "Gamma BIC:", round(gamma$bic, 3), "<br><br>")
    }
    
    if ("Logistic" %in% input$distribution) {
      logis <- fitdist(all_scores(), "logis")
      line <- paste(line, "Logistic location:", round(logis$estimate["location"], 2), "<br>",
                    "Logistic scale:", round(logis$estimate["scale"], 2), "<br>",
                    "Logistic BIC:", round(logis$bic, 3), "<br><br>")
    }
    
    if ("Log-Normal" %in% input$distribution) {
      lognorm <- fitdist(all_scores(), "lnorm")
      line <- paste(line, "Log-Normal meanlog:", round(lognorm$estimate["meanlog"], 2), "<br>",
                    "Log-Normal sdlog:", round(lognorm$estimate["sdlog"], 2), "<br>",
                    "Log-Normal BIC:", round(lognorm$bic, 3), "<br><br>")
    }
    
    if ("Normal" %in% input$distribution) {
      normal <- fitdist(all_scores(), "norm")
      line <- paste(line, "Normal mean:", round(normal$estimate["mean"], 2), "<br>",
                    "Normal sd:", round(normal$estimate["sd"], 2), "<br>",
                    "Normal BIC:", round(normal$bic, 3), "<br><br>")
    }
    
    if ("Uniform" %in% input$distribution) {
      uniform <- fitdist(all_scores(), "unif")
      line <- paste(line, "Uniform min:", round(uniform$estimate["min"], 2), "<br>",
                    "Uniform max:", round(uniform$estimate["max"], 2), "<br>",
                    "Uniform BIC:", round(uniform$bic, 3), "<br><br>")
    }
    
    if ("Weibull" %in% input$distribution) {
      weibull <- fitdist(all_scores(), "weibull")
      line <- paste(line, "Weibull shape:", round(weibull$estimate["shape"], 2), "<br>",
                    "Weibull scale:", round(weibull$estimate["scale"], 2), "<br>",
                    "Weibull BIC:", round(weibull$bic, 3))
    }
    
    div(class = "descriptive", HTML(line))
  })
  
  output$space_params <- renderUI({
    if (length(all_spaces()) < 3 | length(input$distribution) < 1) return()
    
    line <- ""
    
    if ("Chi-Square" %in% input$distribution) {
      chi_sq <- fitdist(all_spaces(), "chisq", start=list(df=1))
      line <- paste(line, "Chi-Square df:", round(chi_sq$estimate["df"], 2), "<br>",
                    "Chi-Square BIC:", round(chi_sq$bic, 3), "<br><br>")
    }
    
    if ("Exponential" %in% input$distribution) {
      exponential <- fitdist(all_spaces(), "exp")
      line <- paste(line, "Exponential rate:", round(exponential$estimate["rate"], 2), "<br>",
                    "Exponential BIC:", round(exponential$bic, 3), "<br><br>")
    }
    
    if ("F" %in% input$distribution) {
      f_dist <- fitdist(all_spaces(), "f", start=list(df1=1, df2=1))
      line <- paste(line, "F df1:", round(f_dist$estimate["df1"], 2), "<br>",
                    "F df2:", round(f_dist$estimate["df2"], 2), "<br>",
                    "F BIC:", round(f_dist$bic, 3), "<br><br>")
    }
    
    if ("Gamma" %in% input$distribution) {
      gamma <- fitdist(all_spaces(), "gamma")
      line <- paste(line, "Gamma shape:", round(gamma$estimate["shape"], 2), "<br>",
                    "Gamma rate:", round(gamma$estimate["rate"], 2), "<br>",
                    "Gamma BIC:", round(gamma$bic, 3), "<br><br>")
    }
    
    if ("Logistic" %in% input$distribution) {
      logis <- fitdist(all_spaces(), "logis")
      line <- paste(line, "Logistic location:", round(logis$estimate["location"], 2), "<br>",
                    "Logistic scale:", round(logis$estimate["scale"], 2), "<br>",
                    "Logistic BIC:", round(logis$bic, 3), "<br><br>")
    }
    
    if ("Log-Normal" %in% input$distribution) {
      lognorm <- fitdist(all_spaces(), "lnorm")
      line <- paste(line, "Log-Normal meanlog:", round(lognorm$estimate["meanlog"], 2), "<br>",
                    "Log-Normal sdlog:", round(lognorm$estimate["sdlog"], 2), "<br>",
                    "Log-Normal BIC:", round(lognorm$bic, 3), "<br><br>")
    }
    
    if ("Normal" %in% input$distribution) {
      normal <- fitdist(all_spaces(), "norm")
      line <- paste(line, "Normal mean:", round(normal$estimate["mean"], 2), "<br>",
                    "Normal sd:", round(normal$estimate["sd"], 2), "<br>",
                    "Normal BIC:", round(normal$bic, 3), "<br><br>")
    }
    
    if ("Uniform" %in% input$distribution) {
      uniform <- fitdist(all_spaces(), "unif")
      line <- paste(line, "Uniform min:", round(uniform$estimate["min"], 2), "<br>",
                    "Uniform max:", round(uniform$estimate["max"], 2), "<br>",
                    "Uniform BIC:", round(uniform$bic, 3), "<br><br>")
    }
    
    if ("Weibull" %in% input$distribution) {
      weibull <- fitdist(all_spaces(), "weibull")
      line <- paste(line, "Weibull shape:", round(weibull$estimate["shape"], 2), "<br>",
                    "Weibull scale:", round(weibull$estimate["scale"], 2), "<br>",
                    "Weibull BIC:", round(weibull$bic, 3))
    }
    
    div(class = "descriptive", HTML(line))
  })
  
  observeEvent(input$show_details, {
    details(!details())
    
    if (details()) {
      updateActionButton(session, "show_details", "Hide details")
    } else {
      updateActionButton(session, "show_details", "Show details")
    }
  })
  
  observeEvent(input$option_button, {
    option(!option())
    
    toggle("reset_game")
    toggle("keyboard")
    
    toggle("algorithm")
    toggle("iterations")
    toggle("target")
    toggle("distribution")
    
    toggle("show_details")
    toggle("one_guess")
    toggle("simulation_button")
    toggle("reset_simulation")
    
    toggle("score_histogram")
    toggle("score_statistics")
    toggle("score_params")
    
    toggle("space_histogram")
    toggle("space_statistics")
    toggle("space_params")
    
    updateActionButton(session, "show_details", "Show details")
    updateActionButton(session, "reset_simulation", "Reset")
    
    if (option()) {
      details(TRUE)
      updateActionButton(session, "option_button", "Return to game")
    } else {
      details(FALSE)
      updateActionButton(session, "option_button", "Run simulations")
      exit_simulation(TRUE)
    }
  })
  
  algorithm_guess <- function() {
    target <- input$target <= length(all_guesses()) + 1
    
    if (algorithm() == "Random Guesses") {
      result <- random_guess(all_guesses(), target)
    } else if (algorithm() == "Knuth's Worst Case") {
      result <- worst_case(all_guesses(), target)
    } else if (algorithm() == "Kooi's Most Parts") {
      result <- most_parts(all_guesses(), target)
    } else if (algorithm() == "Irving's Expected") {
      result <- expected(all_guesses(), target)
    } else if (algorithm() == "Neuwirth's Entropy") {
      result <- entropy(all_guesses(), target)
    } else return()
    
    check_result <- check_word(result, solution())
    all_guesses(append(all_guesses(), list(check_result)))
    
    potential_solutions <- filter_solutions(word_list, all_guesses())
    last_solution_count <- solution_count()[length(solution_count())]
    reduced_space <- 1 - length(potential_solutions) / last_solution_count
    
    solution_count(c(solution_count(), length(potential_solutions)))
    game_spaces(c(game_spaces(), reduced_space))
    
    if (isTRUE(check_result$win) | length(all_guesses()) >= max_turns()) {
      finished(TRUE)
      iteration(iteration() + 1)
      all_spaces(c(all_spaces(), mean(game_spaces())))
      
      if (isTRUE(check_result$win)) 
        all_scores(c(all_scores(), length(all_guesses())))
      else all_scores(c(all_scores(), max_turns()+1))
    }
  }
  
  observeEvent(input$one_guess, {
    if (finished()) reset_game()
    algorithm(input$algorithm)
    
    algorithm_guess()
  })
  
  observeEvent(input$simulation_button, {
    reset_simulation()
    
    algorithm(input$algorithm)
    exit_simulation(FALSE)
    
    updateActionButton(session, "reset_simulation", "Stop")
    click("recursive")
  })
  
  observeEvent(input$reset_simulation, {
    if (exit_simulation()) reset_simulation()
    exit_simulation(TRUE)
    updateActionButton(session, "reset_simulation", "Reset")
  })
  
  observeEvent(input$recursive, {
    if (exit_simulation()) return()
    
    if (iteration() > input$iterations) {
      updateActionButton(session, "reset_simulation", "Reset")
      exit_simulation(TRUE)
      return()
    }
    
    if (finished()) reset_game()
    
    algorithm_guess()
    click("recursive")
  })
}
