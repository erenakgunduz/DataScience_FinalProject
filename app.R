source("global.R")

df <- read.csv("data/all.csv", stringsAsFactors = FALSE)


ui <- fluidPage(
  tags$head(tags$style(
    type = "text/css",
    "img {max-width: 100%; width: 100%; height: auto}"
  )),
  titlePanel("Analysis of Street Fighter V Attacks"),
  br(),
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
      uiOutput("main_display")
    )
  )
)

information_ui <- function(id) {
  ns <- NS(id)
  sidebar <- tagList(
    selectInput(ns("character"), "Select character:",
      choices = c("All", unique(df$Character))
    ),
    selectInput(ns("examine_var"), "Variable to examine",
      choices = c("Frames on block", "Damage", "Stun")
    )
  )
  main <- tagList(
    h2("Chun-Li"),
    br(),
    fluidRow(
      column(
        6,
        img(src = "img/chun-li.png")
      ),
      column(
        6,
        h3(style = "margin-top: 0px", "Tables")
      )
    ),
    br(),
    fluidRow(
      column(
        12,
        h3("Kernel density estimate")
      )
    )
  )
  list(sidebar = sidebar, main = main)
}

visualization_ui <- function(id) {
  ns <- NS(id)
  sidebar <- tagList(
    selectInput(ns("character"), "Select character:",
      choices = c("All", unique(df$Character))
    ),
    selectInput(ns("vislize_var"), "Variable(s)",
      choices = c("Both", "Damage", "Stun")
    )
  )
  main <- tagList(
    h2("Chun-Li"),
    br(),
    fluidRow(
      column(12,
        align = "center", offset = 0,
        h3("Heatmap goes here")
      )
    ),
    if (TRUE) {
      fluidRow(
        column(12,
          align = "center", offset = 0,
          h3("Scatterplot goes here")
        )
      )
    } else {
      fluidRow(
        column(6,
          align = "center", offset = 0,
          h3("Scatterplot goes here")
        ),
        column(6,
          align = "center", offset = 0,
          h3("Scatterplot goes here")
        )
      )
    }
  )
  list(sidebar = sidebar, main = main)
}

prediction_ui <- function(id) {
  ns <- NS(id)
  sidebar <- tagList(
    selectInput(ns("response_var"), "Want to predict:",
      choices = c("Damage", "Stun")
    )
  )
  main <- tagList(
    fluidRow(
      column(6,
        align = "center", offset = 0,
        h3("Simple linear regression")
      ),
      column(6,
        align = "center", offset = 0,
        h3("Elastic net")
      )
    )
  )
  list(sidebar = sidebar, main = main)
}

information_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # how do the distributions look?
    }
  )
}

visualization_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # are onBlock and damage+stun correlated?
    }
  )
}

prediction_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # which regression will win: simple linear or elastic net?
    }
  )
}

server <- function(input, output, session) {
  output$sidebar_options <- renderUI({
    tab <- switch(input$tab,
      "Information" = information_ui("Information")$sidebar,
      "Visualization" = visualization_ui("Visualization")$sidebar,
      "Prediction" = prediction_ui("Prediction")$sidebar
    )
    tab
  })

  output$main_display <- renderUI({
    tab <- switch(input$tab,
      "Information" = information_ui("Information")$main,
      "Visualization" = visualization_ui("Visualization")$main,
      "Prediction" = prediction_ui("Prediction")$main
    )
    tab
  })

  information_server("info")
  visualization_server("visl")
  prediction_server("pred")
}


shinyApp(ui, server)
