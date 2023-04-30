source("global.R")
use_virtualenv("sfv_attacks", required = TRUE)

df <- read.csv("data/all.csv", stringsAsFactors = FALSE)

# ----- Functions -----
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

tilemap <- function(response, character, custompal) {
  df_char <- df_tile %>%
    filter(Character == character)
  if (character == "All") {
    if (response == "Damage") {
      tm <- ggplot(df_tile, aes(Damage, character_name, fill = oB_bins))
    } else {
      tm <- ggplot(df_tile, aes(Stun, character_name, fill = oB_bins))
    }
  } else {
    if (response == "Damage") {
      tm <- ggplot(df_char, aes(Damage, character, fill = oB_bins))
    } else {
      tm <- ggplot(df_char, aes(Stun, character, fill = oB_bins))
    }
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
    sp <- ggplot(df, aes(onBlock, Damage)) +
      geom_point(aes(color = Damage)) +
      scale_color_gradientn(colors = viridis(max(df$Damage), option = "C"))
  } else {
    sp <- ggplot(df, aes(onBlock, Stun)) +
      geom_point(aes(color = Stun)) +
      scale_color_gradientn(colors = viridis(max(df$Stun), option = "D"))
  }
  sp + geom_smooth(
    method = "lm", formula = y ~ x,
    color = clr, fill = clr, alpha = 0.15
  )
}

# ----- UIs -----
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
    h2(textOutput(ns("ch_nombre"))),
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
    checkboxInput(ns("seed_test"), "Set seed for train-test split", TRUE),
    checkboxInput(ns("seed_cv"), "Set seed for cross-validation folds", TRUE)
  )
  main <- tagList(
    fluidRow(
      column(6,
        style = "padding-right: 0px",
        align = "center", offset = 0,
        h3("Simple linear regression"),
        h4("Nothing more to it than y = a + bx, equation may look familiar ;D"),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        plotOutput(ns("scattrp")),
        br(),
        br(),
        actionButton(ns("lr_coef"), label = "Show coefficients"),
        h3(""),
        h4(textOutput(ns("lr_formula"))),
        br(),
        h4(textOutput(ns("cv_score"))),
        br(),
        actionButton(ns("lr_cv"), label = "Get cross-validation score"),
        br()
      ),
      column(6,
        align = "center", offset = 0,
        h3("Elastic net regression"),
        h4("16 predictors, use penalties to help figure out which matter more"),
        sliderInput(ns("a_slider_c"),
          label = h4("Select alpha value:"), min = 0,
          max = 1, value = 0.4, step = 0.2
        ),
        uiOutput(ns("coef_plot")),
        actionButton(ns("en_coef"), label = "Show coefficients"),
        uiOutput(ns("cv_plot")),
        actionButton(ns("en_cv"), label = "Get cross-validation scores"),
        h3(""),
        p(strong("Best tuning parameters:"), uiOutput(ns("best_params")))
      )
    ),
    fluidRow(
      column(12,
        align = "center", offset = 0,
        h4("Choose elastic net tuning parameters for testing:"),
        checkboxInput(
          ns("best"),
          'Go with the "best" (highest CV scores)', TRUE
        ),
        conditionalPanel(
          condition = sprintf("!input['%s']", ns("best")),
          sliderTextInput(ns("l_slider"),
            label = p("Lambda (amount of penalty to apply):"),
            grid = TRUE, choices = sprintf("%g", 10^seq(-4, 4, length.out = 9))
          ),
          sliderInput(ns("a_slider_t"),
            label = p("Alpha (L1 ratio; 0 pure ridge, 1 pure lasso):"),
            min = 0, max = 1, value = 0.4, step = 0.2
          ),
        ),
        actionButton(ns("test"), label = "Test em!"),
      )
    ),
    fluidRow(
      br(),
      column(6,
        align = "center", offset = 0,
        br(),
        h4(textOutput(ns("lr_test_score"))),
        br()
      ),
      column(6,
        align = "center", offset = 0,
        div(
          style = "background-color: #ccff66; border-radius: 11px",
          br(),
          h4(textOutput(ns("en_test_score"))),
          br()
        )
      )
    ),
    fluidRow(
      column(12,
        align = "center", offset = 0,
        checkboxInput(ns("show_results"), "Show testing results", FALSE),
        verbatimTextOutput(ns("wtf"))
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s']", ns("show_results")),
      fluidRow(
        column(6,
          align = "center", offset = 0,
          tableOutput(ns("lr_pred_tb"))
        ),
        column(6,
          align = "center", offset = 0,
          tableOutput(ns("en_pred_tb"))
        )
      )
    )
  )
  list(sidebar = sidebar, main = main)
}

# ----- Servers -----
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

      output$ch_nombre <- renderText(ch_name())
      output$correlate <- renderPrint({
        correlation(ch_data(), input$vislize_var)
      })
      output$scatterplots <- renderPlot({
        scatterplot(ch_data(), input$vislize_var, ch_color())
      })
      output$tilemaps <- renderPlot({
        tilemap(input$vislize_var, input$char, input$palcolor)
      })
    }
  )
}

prediction_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      regression <- import("regression")
      np <- import("numpy")

      v <- reactive({
        regression$data_splits(
          "all.csv", tolower(input$response_var),
          input$seed_test, input$seed_cv
        )
      })
      score_dem <- reactive({
        ifelse(input$scorer == "R-squared", return("r2"), return("mse"))
      })

      train <- reactiveValues(X = NULL, x = NULL, y = NULL)
      slr <- reactiveValues(coefs = NULL, cv = NULL)
      tuning_params <- reactiveValues(t = NULL)

      observeEvent(input$en_coef, {
        vec <- v()
        vec[[3]][[2]] <- np$array(as.numeric(vec[[3]][[2]]))
        vec[[4]][[2]] <- np$array(as.numeric(vec[[4]][[2]]))
        train$X <- np$array(vec[[3]][[1]])
        train$y <- vec[[3]][[2]]
        cols <- np$array(vec[[1]][3:19])

        regression$plot_parameters(train$X, train$y, cols)

        output$coef_plot <- renderUI({
          img_path <- paste0("img/regr/", input$response_var, "_params_")
          img_path <- paste0(img_path, input$a_slider_c * 5, ".png")
          print(img_path)
          img(src = img_path)
        })
      })

      observeEvent(input$lr_coef, {
        vec <- v()
        og_x <- np$array(vec[[2]][, 3])
        og_y <- np$array(vec[[2]][, 19])

        slr$coefs <- regression$simple_lr(og_x, og_y)

        output$lr_formula <- renderText({
          paste0(
            "Predicted y = ", round(slr$coefs[[1]], 3),
            " + ", round(slr$coefs[[2]], 3), "x"
          )
        })
      })

      observeEvent(input$lr_cv, {
        vec <- v()
        og_x <- np$array(vec[[2]][, 3])
        og_y <- np$array(vec[[2]][, 19])

        slr$cv <- regression$simple_lr(
          og_x, og_y,
          vec[[5]], TRUE, score_dem()
        )

        output$cv_score <- renderText({
          as.character(round(slr$cv, 3))
        })
      })

      observeEvent(input$en_cv, {
        vec <- v()
        vec[[3]][[2]] <- np$array(as.numeric(vec[[3]][[2]]))
        vec[[4]][[2]] <- np$array(as.numeric(vec[[4]][[2]]))
        train$X <- np$array(vec[[3]][[1]])
        train$y <- vec[[3]][[2]]
        cols <- np$array(vec[[1]][3:19])

        t <- regression$cross_validation(
          train$X, train$y,
          vec[[5]], cols, score_dem()
        )
        tuning_params$t <- t

        output$cv_plot <- renderUI({
          img(src = paste0("img/regr/", input$response_var, "_cv.png"))
        })

        output$best_params <- renderUI({
          pre(sprintf(
            "Lambda: %.4f  |  Alpha: %.1f",
            tuning_params$t[[1]], tuning_params$t[[2]]
          ))
        })
      })

      observeEvent(input$test, {
        vec <- v()
        vec[[3]][[2]] <- np$array(as.numeric(vec[[3]][[2]]))
        vec[[4]][[2]] <- np$array(as.numeric(vec[[4]][[2]]))
        og_y <- np$array(vec[[2]][, 19])
        score_metr <- ifelse(score_dem() == "r2", 1, 2)

        results_best <- regression$testing(
          vec[[3]], vec[[4]],
          mean(og_y), tuning_params$t[[1]], tuning_params$t[[2]]
        )
        results_custom <- regression$testing(
          vec[[3]], vec[[4]],
          mean(og_y), as.numeric(input$l_slider), input$a_slider_t
        )

        output$lr_test_score <- renderText({
          paste0("Test score:  ", round(ifelse(input$best,
            results_best[[1]][1, score_metr], results_custom[[1]][1, score_metr]
          ), 3))
        })

        output$en_test_score <- renderText({
          paste0("Test score:  ", round(ifelse(input$best,
            results_best[[1]][2, score_metr], results_custom[[1]][2, score_metr]
          ), 3))
        })

        lr_pred <- as.data.frame(results_best[[2]][1, , ])

        if (input$best) {
          en_pred <- as.data.frame(results_best[[2]][2, , ])
        } else {
          en_pred <- as.data.frame(results_custom[[2]][2, , ])
        }

        colnames(lr_pred) <- c("Character", "Attack", "Predicted", "Actual")
        colnames(en_pred) <- c("Character", "Attack", "Predicted", "Actual")

        output$lr_pred_tb <- renderTable(lr_pred)
        output$en_pred_tb <- renderTable(en_pred)
      })

      output$scattrp <- renderPlot({
        scatterplot(df, input$response_var, "#000fff")
      })
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
