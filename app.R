source("global.R")


ui <- fluidPage(
    titlePanel("Analysis of Street Fighter V Attacks"),
    tabsetPanel(
        tabPanel("Information"),
        tabPanel("Visualization"),
        tabPanel("Prediction")
    )
)

information <- function(input, output, session) {
    # how do the distributions look?
}

visualization <- function(input, output, session) {
    # are onBlock and damage+stun correlated?
}

prediction <- function(input, output, session) {
    # which regression will win: simple linear or elastic net?
}

server <- function(input, output, session) {
    callModule(information, "Information")
    callModule(visualization, "Visualization")
    callModule(prediction, "Prediction")
}


shinyApp(ui = ui, server = server)
