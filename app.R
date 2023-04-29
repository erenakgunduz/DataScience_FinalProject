source("global.R")

df <- read.csv("data/all.csv", stringsAsFactors = FALSE)

# Generate summary table for attack categorical features
summary_tb <- function(feature) {
  df %>%
    group_by({{ feature }}) %>%
    summarise(
      count = n(),
      average_damage = mean(Damage),
      average_stun = mean(Stun)
    ) %>%
    arrange(desc(average_damage))
}

all_kde <- function(variable) {
  sprintf(
    "Median: %.2f  |  Mean: %.2f  |  Standard deviation: %.2f",
    median(variable),
    mean(variable),
    sd(variable)
  )
  ggplot(df, aes(variable)) +
    geom_density(adjust = 1, linewidth = 1, lineend = "round")
}

character_kde <- function(variable, kde_color) {
  sprintf(
    "Median: %.2f  |  Mean: %.2f  |  Standard deviation: %.2f",
    median(variable),
    mean(variable),
    sd(variable)
  )
  # user will be able to adjust bandwidth with slider
  ggplot(df, aes(variable)) +
    geom_density(
      alpha = 0.2, adjust = 1, linewidth = 1.6,
      lineend = "round", fill = kde_color, color = kde_color
    )
}

df_tile <- df %>%
  # convert to factor and reverse level order so it looks right on plot
  mutate(character_name = factor(Character,
    levels = rev(sort(unique(Character)))
  )) %>%
  # create intervals for plotting/colormap and similar to before, reverse order
  mutate(oB_bins = cut(onBlock,
    breaks = seq(from = -100, to = max(onBlock, na.rm = TRUE) + 2, by = 25)
  )) %>%
  mutate(oB_bins = factor(as.character(oB_bins), levels = rev(levels(oB_bins))))

# make sure the set of colors matches the number of bins
cmap_bins <- length(levels(df_tile$oB_bins))

tilemap <- function(responses, cn, custompal) {
  print(ggplot(df_tile, aes({{ responses }}, {{ cn }}, fill = oB_bins)) +
    geom_tile(width = 10, color = "white", linewidth = 0.2) +
    coord_fixed(ratio = 10) +
    guides(fill = guide_legend(title = "Frames\non block")) +
    labs(y = "") +
    # removes extra space
    scale_y_discrete(expand = c(0, 0)) +
    # define new breaks on x-axis
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(-10, 410),
      breaks = seq(from = 0, to = 400, by = 50)
    ) +
    scale_fill_manual(values = rev(brewer.pal(cmap_bins, custompal))) +
    # set a base size for all fonts
    theme_grey(base_size = 9) +
    # styling to make it my own
    theme(
      legend.title = element_text(color = "#292F36"),
      legend.text = element_text(size = 7, color = "#292F36"),
      legend.key.height = grid::unit(0.8, "cm"),
      legend.key.width = grid::unit(0.3, "cm"),
      axis.title.x = element_text(margin = margin(t = 8), color = "#292F36"),
      axis.text.x = element_text(color = "#292F36"),
      axis.text.y = element_text(color = "#292F36"),
      axis.ticks = element_line(linewidth = 0.4), # thickness of axis ticks
      plot.background = element_blank(), # rm background
      panel.border = element_blank() # rm outer border
    ))
}

scatterplot <- function(datafile, char_name, clr) {
  df <- read.csv(datafile, stringsAsFactors = FALSE)

  print(cor.test(df$onBlock, df$Damage))
  print(lm(Damage ~ onBlock, data = df))
  print(ggplot(df, aes(onBlock, Damage)) +
    geom_point(aes(color = Damage)) +
    scale_color_gradientn(colors = viridis(max(df$Damage), option = "C")) +
    geom_smooth(
      method = "lm", formula = y ~ x,
      color = clr, fill = clr, alpha = 0.15
    ))

  print(cor.test(df$onBlock, df$Stun))
  print(lm(Stun ~ onBlock, data = df))
  print(ggplot(df, aes(onBlock, Stun)) +
    geom_point(aes(color = Stun)) +
    scale_color_gradientn(colors = viridis(max(df$Stun), option = "D")) +
    geom_smooth(
      method = "lm", formula = y ~ x,
      color = clr, fill = clr, alpha = 0.15
    ))
}


ui <- fluidPage(
  tags$head(tags$style(
    type = "text/css",
    "img {max-width: 100%; width: 100%; height: auto} h4 {font-weight: normal}"
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
    selectInput(ns("examine_var"), "Variable to examine:",
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
    if (FALSE) {
      fluidRow(
        column(12,
          align = "center", offset = 0,
          h3("Heatmap goes here")
          # Gotta use unique(as.character(character_name))[index] for each
        )
      )
    } else {
      fluidRow(
        column(6,
          align = "center", offset = 0,
          h3("Heatmap goes here")
        ),
        column(6,
          align = "center", offset = 0,
          h3("Heatmap goes here")
        )
      )
    },
    if (FALSE) {
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
    ),
    selectInput(ns("scorer"), "Scoring metric",
      choices = c("Mean squared error", "R-squared")
    ),
    checkboxInput("seed_test", "Set seed for train-test split", TRUE),
    checkboxInput("seed_cv", "Set seed for cross-validation folds", TRUE)
  )
  main <- tagList(
    fluidRow(
      column(6,
        align = "center", offset = 0,
        h3("Simple linear regression"),
        h4("It's literally just y = a + bx, equation should look familiar ;D"),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        p("This will be a scatterplot"),
        actionButton("lr_coef", label = "Show coefficients"),
        h4("The y = a + bx formula"),
        p("CV score"),
        actionButton("lr_cv", label = "Get cross-validation score"),
        br()
      ),
      column(6,
        align = "center", offset = 0,
        h3("Elastic net regression"),
        h4("16 predictors, use penalties to help figure out which matter more"),
        sliderInput("a_slider_c",
          label = h4("Select alpha value:"), min = 0,
          max = 1, value = 0.4, step = 0.2
        ),
        p("This will be the parameter plot, cycleable with slider"),
        actionButton("en_coef", label = "Show coefficients"),
        h4(br()),
        p("CV scores plot"),
        actionButton("en_cv", label = "Get cross-validation score"),
        p(strong("Best tuning parameters:"), "l_optimal, a_optimal")
      )
    ),
    fluidRow(
      column(12,
        align = "center", offset = 0,
        h4("Choose elastic net tuning parameters for testing:"),
        checkboxInput("best", "Choose best based on CV", TRUE),
        sliderInput("l_slider",
          label = p("Lambda (amount of penalty to apply):"), min = -1e4,
          max = 1e4, value = 1, step = 1000
        ),
        sliderInput("a_slider_t",
          label = p("Alpha (L1 ratio; 0 pure ridge, 1 pure lasso):"),
          min = 0, max = 1, value = 0.4, step = 0.2
        ),
        actionButton("test", label = "Test em!"),
      )
    ),
    fluidRow(
      column(6,
        align = "center", offset = 0,
        h4("Test score: ~~~~")
      ),
      column(6,
        align = "center", offset = 0,
        h4("Test score: ~~~~")
      )
    ),
    fluidRow(
      column(12,
        align = "center", offset = 0,
        checkboxInput("show_results", "Show testing results", FALSE)
      )
    ),
    fluidRow(
      column(6,
        align = "center", offset = 0,
        h4("Simple LR prediction table")
      ),
      column(6,
        align = "center", offset = 0,
        h4("Elastic net prediction table")
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

  information_server("Information")
  visualization_server("Visualization")
  prediction_server("Prediction")
}


shinyApp(ui, server)
