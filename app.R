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
  ggplot(df, aes(variable)) +
    geom_density(adjust = 1, linewidth = 1, lineend = "round")
}

kde <- function(df, variable, kde_color) {
  ggplot(df, aes({{ variable }})) +
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
  if (responses == "Damage") {
    tm <- ggplot(df_tile, aes(Damage, {{ cn }}, fill = oB_bins))
  } else {
    tm <- ggplot(df_tile, aes(Stun, {{ cn }}, fill = oB_bins))
  }
  tm +
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
    )
}

correlation <- function(df, variable) {
  if (variable == "Damage") {
    print(cor.test(df$onBlock, df$Damage))
    lm(Damage ~ onBlock, data = df)
  } else {
    print(cor.test(df$onBlock, df$Stun))
    lm(Stun ~ onBlock, data = df)
  }
}

scatterplot <- function(df, variable, clr) {
  if (variable == "Damage") {
    ggplot(df, aes(onBlock, Damage)) +
      geom_point(aes(color = Damage)) +
      scale_color_gradientn(colors = viridis(max(df$Damage), option = "C")) +
      geom_smooth(
        method = "lm", formula = y ~ x,
        color = clr, fill = clr, alpha = 0.15
      )
  } else {
    ggplot(df, aes(onBlock, Stun)) +
      geom_point(aes(color = Stun)) +
      scale_color_gradientn(colors = viridis(max(df$Stun), option = "D")) +
      geom_smooth(
        method = "lm", formula = y ~ x,
        color = clr, fill = clr, alpha = 0.15
      )
  }
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
      choices = c("All", unique(df$Character)), selected = "Chun-Li"
    ),
    selectInput(ns("examine_var"), "Variable to examine:",
      choices = c("Frames on block", "Damage", "Stun")
    )
  )
  main <- tagList(
    h2(textOutput(ns("c_name"))),
    br(),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'All'", ns("character")),
      h3(style = "margin-top: 0px", "Tables"),
      fluidRow(
        column(
          6,
          h4("Attack categorical features summary tables"),
          div(verbatimTextOutput(ns("summary_plnCmd")),
            style = "height:500px; overflow-y:scroll"
          ),
          verbatimTextOutput(ns("summary_moveType")),
          verbatimTextOutput(ns("summary_airmove")),
          verbatimTextOutput(ns("summary_followUp")),
          verbatimTextOutput(ns("summary_projectile"))
        ),
        column(
          6,
          h4("Character numerical attributes on average"),
          verbatimTextOutput(ns("char_summary"))
        )
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] != 'All'", ns("character")),
      fluidRow(
        column(
          6,
          uiOutput(ns("c_img"))
        ),
        column(
          6,
          h3(style = "margin-top: 0px", "Tables"),
          h4("Character stats used for predictions"),
          verbatimTextOutput(ns("char_stats")),
          h4("All attacks and their specific features"),
          div(verbatimTextOutput(ns("attack_fts")),
            style = "height:300px; overflow-y:scroll"
          )
        )
      )
    ),
    br(),
    h3("Kernel density estimate"),
    verbatimTextOutput(ns("description")),
    plotOutput(ns("full_kde"))
  )
  list(sidebar = sidebar, main = main)
}

visualization_ui <- function(id) {
  ns <- NS(id)
  sidebar <- tagList(
    selectInput(ns("char"), "Select character:",
      choices = c("All", unique(df$Character))
    ),
    selectInput(ns("vislize_var"), "Variable(s)",
      choices = c("Damage", "Stun")
    ),
    selectInput(ns("palcolor"), "Tilemap color palette",
      choices = c("YlGnBu", "YlOrRd")
    ),
  )
  main <- tagList(
    h2("Chun-Li"),
    br(),
    fluidRow(
      column(12,
        align = "center", offset = 0,
        h4("Tilemap"),
        plotOutput(ns("tilemaps"))
      )
    ),
    fluidRow(
      column(12,
        align = "center", offset = 0,
        h4("Scatter plots fitted with simple linear regression"),
        plotOutput(ns("scatterplots")),
        verbatimTextOutput(ns("correlate"))
      )
    )
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
        checkboxInput("best", "Choose best based on cross-validation", TRUE),
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
      char_name <- reactive({
        if (input$character == "All") {
          return("All characters")
        } else {
          return(input$character)
        }
      })
      char_data <- reactive({
        filename <- gsub("\\.", "_", tolower(input$character))
        if (filename == "all") {
          return(df)
        } else if (filename == "zeku (old)") {
          char_df <- read.csv("data/characters/zeku_old.csv",
            stringsAsFactors = FALSE
          )
        } else if (filename == "zeku (young)") {
          char_df <- read.csv("data/characters/zeku_young.csv",
            stringsAsFactors = FALSE
          )
        } else {
          char_file <- paste0("data/characters/", filename, ".csv")
          char_df <- read.csv(char_file, stringsAsFactors = FALSE)
          return(char_df)
        }
      })
      char_color <- reactive({
        switch(input$character,
          "All" = "#000000",
          "Abigail" = "#af33b4",
          "Akuma" = "#a52e28",
          "Alex" = "#48683d",
          "Balrog" = "#235dbb",
          "Birdie" = "#d7a23e",
          "Blanka" = "#b36732",
          "Cammy" = "#67b600",
          "Chun-Li" = "#37659b",
          "Cody" = "#521f15",
          "Dan" = "#bf6072",
          "Dhalsim" = "#e0602b",
          "E.Honda" = "#5197a3",
          "Ed" = "#51bcd0",
          "F.A.N.G" = "#3040a0",
          "Falke" = "#191b44",
          "G" = "#3d453a",
          "Gill" = "#bf423b",
          "Guile" = "#597c00",
          "Ibuki" = "#4b1d40",
          "Juri" = "#983190",
          "Kage" = "#553fad",
          "Karin" = "#e93824",
          "Ken" = "#ff0000",
          "Kolin" = "#394057",
          "Laura" = "#80dc00",
          "Lucia" = "#3e77b1",
          "M.Bison" = "#480e07",
          "Menat" = "#3d224b",
          "Nash" = "#3a8455",
          "Necalli" = "#581e0d",
          "Poison" = "#d14461",
          "R.Mika" = "#68e6f7",
          "Rashid" = "#375963",
          "Ryu" = "#457272",
          "Sagat" = "#a16a46",
          "Sakura" = "#ff83fa",
          "Seth" = "#6d7293",
          "Urien" = "#714661",
          "Vega" = "#ed5971",
          "Zangief" = "#e52a0e",
          "Zeku (Old)" = "#545345",
          "Zeku (Young)" = "#351f0a",
        )
      })

      output$c_img <- renderUI({
        filename <- gsub("\\.", "_", tolower(input$character))
        if (filename == "all") {
          return(NULL)
        } else if (filename == "zeku (old)") {
          return(tags$img(src = "img/zeku_old.png"))
        } else if (filename == "zeku (young)") {
          return(tags$img(src = "img/zeku_young.png"))
        } else {
          return(tags$img(src = paste0("img/", filename, ".png")))
        }
      })
      output$c_name <- renderText(char_name())
      output$test <- renderPrint(head(char_data()))
      output$char_summary <- renderPrint(colMeans(df[9:18]))
      output$summary_plnCmd <- renderPrint(print(summary_tb(plnCmd), n = Inf))
      output$summary_moveType <- renderPrint(summary_tb(moveType))
      output$summary_airmove <- renderPrint(summary_tb(airmove))
      output$summary_followUp <- renderPrint(summary_tb(followUp))
      output$summary_projectile <- renderPrint(summary_tb(projectile))
      output$description <- renderPrint({
        switch(input$examine_var,
          "Frames on block" = sprintf(
            "Median: %.2f  |  Mean: %.2f  |  Standard deviation: %.2f",
            median(char_data()$onBlock),
            mean(char_data()$onBlock),
            sd(char_data()$onBlock)
          ),
          "Damage" = sprintf(
            "Median: %.2f  |  Mean: %.2f  |  Standard deviation: %.2f",
            median(char_data()$Damage),
            mean(char_data()$Damage),
            sd(char_data()$Damage)
          ),
          "Stun" = sprintf(
            "Median: %.2f  |  Mean: %.2f  |  Standard deviation: %.2f",
            median(char_data()$Stun),
            mean(char_data()$Stun),
            sd(char_data()$Stun)
          )
        )
      })
      output$full_kde <- renderPlot({
        switch(input$examine_var,
          "Frames on block" = kde(char_data(), onBlock, char_color()) +
            labs(title = "Frames on block") +
            theme(plot.title = element_text(size = 16)),
          "Damage" = kde(char_data(), Damage, char_color()) +
            labs(title = "Damage") +
            theme(plot.title = element_text(size = 16)),
          "Stun" = kde(char_data(), Stun, char_color()) +
            labs(title = "Stun") +
            theme(plot.title = element_text(size = 16))
        )
      })
      output$char_stats <- renderPrint(print(t(char_data()[1, 8:17])))
      output$attack_fts <- renderPrint(print(char_data()[c(1:7, 18:19)]))
      # observeEvent(input$character, {
      #   print(input$character)
      # })
    }
  )
}

visualization_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ch_name <- reactive({
        if (input$char == "All") {
          return("All characters")
        } else {
          return(input$char)
        }
      })
      ch_data <- reactive({
        filename <- gsub("\\.", "_", tolower(input$char))
        if (filename == "all") {
          return(df)
        } else if (filename == "zeku (old)") {
          char_df <- read.csv("data/characters/zeku_old.csv",
            stringsAsFactors = FALSE
          )
        } else if (filename == "zeku (young)") {
          char_df <- read.csv("data/characters/zeku_young.csv",
            stringsAsFactors = FALSE
          )
        } else {
          char_file <- paste0("data/characters/", filename, ".csv")
          char_df <- read.csv(char_file, stringsAsFactors = FALSE)
          return(char_df)
        }
      })
      ch_color <- reactive({
        switch(input$char,
          "All" = "#000000",
          "Abigail" = "#af33b4",
          "Akuma" = "#a52e28",
          "Alex" = "#48683d",
          "Balrog" = "#235dbb",
          "Birdie" = "#d7a23e",
          "Blanka" = "#b36732",
          "Cammy" = "#67b600",
          "Chun-Li" = "#37659b",
          "Cody" = "#521f15",
          "Dan" = "#bf6072",
          "Dhalsim" = "#e0602b",
          "E.Honda" = "#5197a3",
          "Ed" = "#51bcd0",
          "F.A.N.G" = "#3040a0",
          "Falke" = "#191b44",
          "G" = "#3d453a",
          "Gill" = "#bf423b",
          "Guile" = "#597c00",
          "Ibuki" = "#4b1d40",
          "Juri" = "#983190",
          "Kage" = "#553fad",
          "Karin" = "#e93824",
          "Ken" = "#ff0000",
          "Kolin" = "#394057",
          "Laura" = "#80dc00",
          "Lucia" = "#3e77b1",
          "M.Bison" = "#480e07",
          "Menat" = "#3d224b",
          "Nash" = "#3a8455",
          "Necalli" = "#581e0d",
          "Poison" = "#d14461",
          "R.Mika" = "#68e6f7",
          "Rashid" = "#375963",
          "Ryu" = "#457272",
          "Sagat" = "#a16a46",
          "Sakura" = "#ff83fa",
          "Seth" = "#6d7293",
          "Urien" = "#714661",
          "Vega" = "#ed5971",
          "Zangief" = "#e52a0e",
          "Zeku (Old)" = "#545345",
          "Zeku (Young)" = "#351f0a",
        )
      })

      output$correlate <- renderPrint({
        correlation(ch_data(), input$vislize_var)
      })
      output$scatterplots <- renderPlot({
        scatterplot(ch_data(), input$vislize_var, ch_color())
      })
      output$tilemaps <- renderPlot({
        switch(input$char,
          "All" = tilemap(input$vislize_var, character_name, input$palcolor),
          "Abigail" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[1], input$palcolor
          ),
          "Akuma" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[2], input$palcolor
          ),
          "Alex" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[3], input$palcolor
          ),
          "Balrog" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[4], input$palcolor
          ),
          "Birdie" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[5], input$palcolor
          ),
          "Blanka" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[6], input$palcolor
          ),
          "Cammy" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[7], input$palcolor
          ),
          "Chun-Li" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[8], input$palcolor
          ),
          "Cody" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[9], input$palcolor
          ),
          "Dan" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[10], input$palcolor
          ),
          "Dhalsim" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[11], input$palcolor
          ),
          "E.Honda" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[12], input$palcolor
          ),
          "Ed" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[13], input$palcolor
          ),
          "F.A.N.G" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[15], input$palcolor
          ),
          "Falke" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[14], input$palcolor
          ),
          "G" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[16], input$palcolor
          ),
          "Gill" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[17], input$palcolor
          ),
          "Guile" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[18], input$palcolor
          ),
          "Ibuki" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[19], input$palcolor
          ),
          "Juri" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[20], input$palcolor
          ),
          "Kage" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[21], input$palcolor
          ),
          "Karin" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[22], input$palcolor
          ),
          "Ken" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[23], input$palcolor
          ),
          "Kolin" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[24], input$palcolor
          ),
          "Laura" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[25], input$palcolor
          ),
          "Lucia" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[26], input$palcolor
          ),
          "M.Bison" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[27], input$palcolor
          ),
          "Menat" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[28], input$palcolor
          ),
          "Nash" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[29], input$palcolor
          ),
          "Necalli" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[30], input$palcolor
          ),
          "Poison" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[31], input$palcolor
          ),
          "R.Mika" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[32], input$palcolor
          ),
          "Rashid" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[33], input$palcolor
          ),
          "Ryu" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[34], input$palcolor
          ),
          "Sagat" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[35], input$palcolor
          ),
          "Sakura" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[36], input$palcolor
          ),
          "Seth" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[37], input$palcolor
          ),
          "Urien" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[38], input$palcolor
          ),
          "Vega" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[39], input$palcolor
          ),
          "Zangief" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[40], input$palcolor
          ),
          "Zeku (Old)" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[41], input$palcolor
          ),
          "Zeku (Young)" = tilemap(
            input$vislize_var,
            unique(as.character(character_name))[42], input$palcolor
          ),
        )
      })
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
