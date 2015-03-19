# ui.R

library(shiny)

shinyUI(fluidPage(

  includeCSS("styles.css"),

  # Application title
  titlePanel("Simple Slot Machines - What If Simulator"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("costPerPlay", 
                  "Select Machine Type:", 
                  choices = c("$5 Machine" = 5,"$1 Machine" = 1, "$0.25 Machine" = 0.25, "$0.10 Machine" = 0.10),
                  selected = 1),
      sliderInput("numPlay",
                  "Number of Consecutive Plays on Machine #1:",
                  min = 5,
                  max = 3000,
                  value = 100),
      sliderInput("numMachine",
                  "Number of Machines:",
                  min = 5,
                  max = 2000,
                  value = 10),
      br(),
      hr(),
      h4("Note:"),
      p("Fair 3-reel slot machines with 8 possible outcomes [1, 2, 3, 4, 5, 6, 7, 8] per each reel."),
      p("Cost per play = $1.00"),
      h4("How to win (paylines): "),
      p("1. Triple 7's [7][7][7] wins the jackpok.  Current jackpot is 300 x the cost per play. Probability = 1/512 = 0.195%"),
      p("2. Triple same non-7's (ex. [2][2][2]) wins 20 x the cost per play. Probability = 1/512 * 7 = 1.367%")
    ),

    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("netPlot"),
        h3("Playin' Summary on Machine #1"),
        htmlOutput("playSummary"),
        hr(),
        plotOutput("manyMachinesPlot"),
        h3("Playin' Summary on All Selected Machines"),
        htmlOutput("payoutSummary"),
        br(),
        br(),
        br()
    )
  )
))


