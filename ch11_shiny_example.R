
library(shiny)
library(ggplot2)
library(gridExtra)

# Define UI for application 
ui <- fluidPage(
  titlePanel("Bayesian Bandit Simulation"),
  sidebarLayout(
    sidebarPanel(
      helpText("Simulate bandit arms with 
               Bayesian updates on 
               reward probabilities."),
      numericInput("bandit1", 
                   "Probability of 
                   reward for Bandit 1:", 
                   0.2, 
                   min = 0, 
                   max = 1),
      numericInput("bandit2", 
                   "Probability of
                   reward for Bandit 2:", 
                   0.5,
                   min = 0, 
                   max = 1),
      numericInput("bandit3", 
                   "Probability of 
                   reward for Bandit 3:",
                   0.7, 
                   min = 0, 
                   max = 1),
      sliderInput("pulls", "Number of pulls:", 
                  min = 1, 
                  max = 500, 
                  value = 100),
      actionButton("simulate", "Simulate")
    ),
    mainPanel(
      plotOutput("plots")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  observeEvent(input$simulate, {
    isolate({
      params <- c(input$bandit1, 
                  input$bandit2,
                  input$bandit3)
      num_bandits <- length(params)
      num_rewards <- numeric(num_bandits)
      num_trials <- numeric(num_bandits)
      
      pull <- function(arm) {
        function() {
          rbinom(1, 1, params[arm + 1])
        }
      }
      
      for (i in 1:input$pulls) {
        choice <- which.max(rbeta(
          num_bandits, 
          2 + num_rewards, 
          2 + num_trials - num_rewards)) - 1
        reward <- pull(choice)()
        num_rewards[choice + 1] <- 
          num_rewards[choice + 1] + reward
        num_trials[choice + 1] <- 
          num_trials[choice + 1] + 1
      }
      
      plots <- 
        lapply(seq_len(num_bandits), 
               function(bandit) {
                 data <- data.frame(
                   x = seq(0, 1, length.out = 100),
                   y = dbeta(seq(0, 1,
                                 length.out = 100), 
                             num_rewards[bandit] + 2,
                             num_trials[bandit] - 
                               num_rewards[bandit] + 2)
                 )
                 ggplot(data, aes(x = x, y = y)) +
                   geom_line() +
                   geom_area(fill = "gray", alpha = 0.3) +
                   labs(title = sprintf(
                     "Posterior for Bandit %d after %d pulls",
                     bandit, input$pulls),
                     x = 'Probability', y = 'Density') +
                   theme_minimal()
               })
      
      output$plots <- renderPlot({
        do.call(gridExtra::grid.arrange, 
                c(plots, ncol = 1))
      })
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




