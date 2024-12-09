library(shiny)

ui <- fluidPage(
  titlePanel("Rock-Paper-Scissors Game"),
 
  sidebarLayout(
    sidebarPanel(
      h3("Choose your move:"),
      radioButtons("user_choice", "Your choice:",
                   choices = list("Rock", "Paper", "Scissor"), 
                   selected = "Rock"),
      actionButton("play", "Play Game")
    ),
    
    mainPanel(
      h4("Game Result:"),
      textOutput("result"),
      br(),
      textOutput("computer_choice"),
      br(),
      textOutput("score")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Initialize score
  score <- reactiveVal(list(user = 0, computer = 0))
  
  # Function to get computer's choice randomly
  computer_choice <- function() {
    sample(c("Rock", "Paper", "Scissors"), 1)
  }
  
  # Function to determine the winner
  determine_winner <- function(user, computer) {
    if (user == computer) {
      return("It's a tie!")
    }
    if ((user == "Rock" && computer == "Scissors") ||
        (user == "Paper" && computer == "Rock") ||
        (user == "Scissors" && computer == "Paper")) {
      return("You win!")
    } else {
      return("Computer wins!")
    }
  }
  
  # Function to update the score
  update_score <- function(winner) {
    current_score <- score()
    if (winner == "You win!") {
      current_score$user <- current_score$user + 1
    } else if (winner == "Computer wins!") {
      current_score$computer <- current_score$computer + 1
    }
    score(current_score)
  }
  
  # Observe the play button
  observeEvent(input$play, {
    # Get user and computer choices
    user_move <- input$user_choice
    comp_move <- computer_choice()
    
    # Determine the winner
    game_result <- determine_winner(user_move, comp_move)
    
    # Update the score
    update_score(game_result)
    
    # Update UI outputs
    output$result <- renderText({ game_result })
    output$computer_choice <- renderText({ paste("Computer chose:", comp_move) })
    output$score <- renderText({
      paste("Score - You: ", score()$user, " Computer: ", score()$computer)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)