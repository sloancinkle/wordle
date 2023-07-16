ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(version = 4),
  title = "Shiny Wordle",
  
  tags$style(HTML("
  .container-fluid {
      text-align: center;
      height: 1050px;
      display: grid;
      grid-template-rows: 1fr auto;
  }
  .guesses {
      overflow-y: visible;
      height: 600px;
  }
  .guesses .word {
      margin: 5px;
  }
  .guesses .word > .letter {
      display: inline-block;
      width: 50px;
      height: 50px;
      text-align: center;
      vertical-align: middle;
      border-radius: 3px;
      line-height: 50px;
      font-size: 32px;
      font-weight: bold;
      vertical-align: middle;
      user-select: none;
      color: white;
      font-family: 'Clear Sans', 'Helvetica Neue', Arial, sans-serif;
  }
  .guesses .word > .correct {
      background-color: #6a5;
  }
  .guesses .word > .in-word {
      background-color: #db5;
  }
  .guesses .word > .not-in-word {
      background-color: #888;
  }
  .guesses .word > .guess {
      color: black;
      background-color: white;
      border: 1px solid black;
  }
  .keyboard {
      height: 150px;
      user-select: none;
  }
  .keyboard .keyboard-row {
      margin: 3px;
  }
  .keyboard .keyboard-row .key {
      display: inline-block;
      padding: 0;
      width: 30px;
      height: 50px;
      text-align: center;
      vertical-align: middle;
      border-radius: 3px;
      line-height: 50px;
      font-size: 18px;
      font-weight: bold;
      vertical-align: middle;
      color: black;
      font-family: 'Clear Sans', 'Helvetica Neue', Arial, sans-serif;
      background-color: #ddd;
      touch-action: none;
  }
  .keyboard .keyboard-row .key:focus {
      outline: none;
  }
  .keyboard .keyboard-row .key.wide-key {
      font-size: 15px;
      width: 50px;
  }
  .keyboard .keyboard-row .key.correct {
      background-color: #6a5;
      color: white;
  }
  .keyboard .keyboard-row .key.in-word {
      background-color: #db5;
      color: white;
  }
  .keyboard .keyboard-row .key.not-in-word {
      background-color: #888;
      color: white;
  }
  .endgame-content {
      font-family: Helvetica, Arial, sans-serif;
      display: inline-block;
      line-height: 1.4;
      letter-spacing: .2em;
      margin: 20px 8px;
      width: fit-content;
      padding: 20px;
      border-radius: 5px;
      box-shadow: 4px 4px 19px rgb(0 0 0 / 17%);
  }
  .descriptive {
      font-family: Helvetica, Arial, sans-serif;
      display: inline-block;
      line-height: 1.4;
      letter-spacing: .2em;
      margin: 20px 8px;
      width: fit-content;
      padding: 20px;
      border-radius: 5px;
      box-shadow: 4px 4px 19px rgb(0 0 0 / 17%);
  }
  .details {
      height: 140px;
  }
")),
  div(
    class = "guesses",
    h3("Shiny Wordle"),
    uiOutput("previous_guesses"),
    uiOutput("current_guess"),
    uiOutput("endgame"),
    uiOutput("new_game_ui")
  ),
  column(12, align="center",
    uiOutput("keyboard"),
    
    splitLayout(cellWidths = list("350px", "200px"),
      column(12, 
        splitLayout(cellWidths = list("200px", "100px"),
          hidden(selectInput("algorithm", "Algorithm",
                            choices = list("Random Guesses",
                                           "Knuth's Worst Case",
                                           "Kooi's Most Parts",
                                           "Irving's Expected",
                                           "Neuwirth's Entropy"),
                            selectize=FALSE, size=5)),
        
        
          hidden(numericInput("iterations", "Iterations", 1, min=1))),
        
        hidden(sliderInput("target", "Target score", min=2, max=7,
                           value=2, width="300px"))),
      
      tags$div(align = 'left', class = 'multicol', 
                hidden(checkboxGroupInput("distribution", "Fit distributions", 
                              choices=c("Chi-Square", "Exponential", "F", "Gamma", "Logistic",
                                        "Log-Normal", "Normal", "Uniform", "Weibull"))))
      ),
    
    actionButton("reset_game", "Reset game"),
    actionButton("show_details", "Show details"),
    actionButton("option_button", "Run simulations"),
    
    hidden(actionButton("one_guess", "Make a guess")),
    hidden(actionButton("simulation_button", "Run simulations")),
    hidden(actionButton("reset_simulation", "Reset")),
    hidden(actionButton("recursive", "")),
    
    div(class = "details", uiOutput("game_details")),
    
    hidden(plotOutput("score_histogram", width="900px", height="500px")),
    hidden(uiOutput("score_statistics")),
    hidden(uiOutput("score_params")),
    
    hidden(plotOutput("space_histogram", width="900px", height="500px")),
    hidden(uiOutput("space_statistics")),
    hidden(uiOutput("space_params"))
  ),
  tags$script(HTML("
    const letters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 
    'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'];
    const all_key_ids = [ ...letters, 'Enter', 'Back'];
    document.addEventListener('keydown', function(e) {
      let key = e.code.replace(/^Key/, '');
      if (letters.includes(key)) {
        document.getElementById(key).click();
      } else if (key == 'Enter') {
        document.getElementById('Enter').click();
      } else if (key == 'Backspace') {
        document.getElementById('Back').click();
      }
    });
  "))
)
