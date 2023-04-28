source("global.R")

df <- read.csv("data/all.csv", stringsAsFactors = FALSE)


ui <- fluidPage(
  titlePanel("Analysis of Street Fighter V Attacks"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("sidebar_options")
    ),
    mainPanel(
      tabsetPanel(
        id = "tab",
        type = "tabs",
        tabPanel("Information"),
        tabPanel("Visualization"),
        tabPanel("Prediction")
      ),
    )
  )
)

information_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("character"), "Select character:",
      choices = c("All", unique(df$Character))
    ),
    selectInput(ns("examine_var"), "Variable to examine",
      choices = c("Frames on block", "Damage", "Stun")
    )
  )
}

visualization_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("character"), "Select character:",
      choices = c("All", unique(df$Character))
    ),
    selectInput(ns("vislize_var"), "Variable(s)",
      choices = c("Both", "Damage", "Stun")
    )
  )
}

prediction_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("response_var"), "Want to predict:",
      choices = c("Damage", "Stun")
    )
  )
}

information_server <- function(input, output, session) {
  # how do the distributions look?
}

visualization_server <- function(input, output, session) {
  # are onBlock and damage+stun correlated?
}

prediction_server <- function(input, output, session) {
  # which regression will win: simple linear or elastic net?
}

server <- function(input, output) {
  output$sidebar_options <- renderUI({
    tab <- switch(as.character(input$tab),
      "Information" = information_ui("Information"),
      "Visualization" = visualization_ui("Visualization"),
      "Prediction" = prediction_ui("Prediction")
    )
    tab
  })

  callModule(information_server, "Information")
  callModule(visualization_server, "Visualization")
  callModule(prediction_server, "Prediction")
}


shinyApp(ui, server)
