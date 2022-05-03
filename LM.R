library(readr)
library(tidyverse)
library(ggplot2)
library(boot)
library(scales)
library(magrittr)
library(shiny)
library(DT)
library(shinyjs)
library(V8)
library(corrplot)
library(PerformanceAnalytics)
library(GGally)
library(modelr)
library(broom)
library(bslib)


dm <<- list()

jscode <- "shinyjs.refresh = function() { history.go(0); }"
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Define UI for dataset viewer app -------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("pageCol")), #

  tags$head(
    tags$style(HTML("
      .option-group {
        border: 1px solid #ccc;
        border-radius: 6px;
        padding: 0px 5px;
        margin: 5px -10px;
        background-color: #f5f5f5;
      }

      .option-header {
        color: #79d;
        text-transform: uppercase;
        margin-bottom: 5px;
      }
      
      h2 {
        font-size: 24px !important;
        font-family: 'Tableau Bold', Tableau, Arial, sans-serif;
        line-height: 36px; 
        color: rgb(102, 102, 102);
        font-weight: bold; 
        font-style: normal;
        text-decoration: none;
      }
      
      hr {
        border-top: 2px solid black;
      }

      #TableView{
        margin-top: 15px;
      }
    "))
  ),
  
  hr(),
  
  titlePanel("Correlation & Regression"),
  
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      div(
      id = "form",
         
      # Input: Selector for choosing Data Source ----
      selectInput(inputId = "datasource",
                  label = "Choose a Data Source",
                  choices = c("Upload CSV"="uploadcsv", "Cut and Paste"="cutandpaste"),
                  selectize = TRUE),

      # File Control Input
      fileInput("inputfile", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Text Area Input
      textAreaInput(inputId = "clipboard",
                label = "Cut and Paste from Clipboard",
                value = ""),

      actionButton("pasteButton", "Paste"),      

      checkboxInput("check_correlation", "Show Correlation Plot Options", FALSE),

      div(id="opt_correlation",
      splitLayout(cellArgs = list(style = "overflow: visible"),
      # Input: Selector for choosing Layout ----
      selectInput(inputId = "corrlayout",
                  label = "Layout",
                  choices = c("full", "upper", "lower", "mixed"),
                  selectize = TRUE),

      # Input: Selector for choosing Text Color ----
      selectInput(inputId = "textcolor",
                  label = "Text Color",
                  choices = c("black", "red", "blue"),
                  selectize = TRUE)
      ),

      # Input: Selector for choosing Method ----
      selectInput(inputId = "corrmethod", width="50%",
                  label = "Method",
                  choices = c("circle", "square", "ellipse", "number", "shade", "color", "pie"),
                  selectize = TRUE),
      
      splitLayout(id="upperlower", cellArgs = list(style = "overflow: visible"),
      # Input: Selector for choosing lower Method in mixed mode ----
      selectInput(inputId = "corrmethod_lower",
                  label = "Lower",
                  choices = c("circle", "square", "ellipse", "number", "shade", "color", "pie"),
                  selectize = TRUE),

      # Input: Selector for choosing upper Method in mixed mode ----
      selectInput(inputId = "corrmethod_upper",
                  label = "Upper",
                  choices = c("circle", "square", "ellipse", "number", "shade", "color", "pie"),
                  selectize = TRUE)
      ),

      splitLayout(cellArgs = list(style = "overflow: visible"),
      # Input: Selector for entering the significance level ----
      numericInput("siglevel", "p Level", 0, min = 0.0, max = 1.0, step = 0.01),

      # Input: Selector for choosing sort order ----
      selectInput(inputId = "corrsort",
                  label = "Sort",
                  choices = c("none", "AOE", "FPC", "hclust", "alphabet"),
                  selectize = TRUE),

      # Input: Selector for adding rectangles ----
      numericInput("addrects", "Rectangles", 1, min = 1, max = 8, step = 1)

      ),

      ),

      checkboxInput("check_ggpairs", "Show GG Pairs Options", FALSE),

      div(id="opt_ggpairs",
      div(HTML("Lower Plot Options")),
      splitLayout(cellArgs = list(style = "overflow: visible"),
      # Input: Selector for choosing Continuous for GGPairs ----
      selectInput(inputId = "lower_ggcontinuous",
                  label = "Continuous",
                  choices = c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'),
                  selectize = TRUE),

      # Input: Selector for choosing Continuous for GGPairs ----
      selectInput(inputId = "lower_ggcombo",
                  label = "Combo",
                  choices = c('box', 'box_no_facet', 'dot', 'dot_no_facet', 'facethist', 'facetdensity', 'denstrip', 'blank'),
                  selectize = TRUE),

      # Input: Selector for choosing Continuous for GGPairs ----
      selectInput(inputId = "lower_ggdiscrete",
                  label = "Discrete",
                  choices = c('facetbar', 'ratio', 'blank'),
                  selectize = TRUE)
      ),

      div(HTML("Upper Plot Options")),
      splitLayout(cellArgs = list(style = "overflow: visible"),
      # Input: Selector for choosing Continuous for GGPairs ----
      selectInput(inputId = "upper_ggcontinuous",
                  label = "Continuous",
                  choices = c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'),
                  selectize = TRUE),

      # Input: Selector for choosing Continuous for GGPairs ----
      selectInput(inputId = "upper_ggcombo",
                  label = "Combo",
                  choices = c('box', 'box_no_facet', 'dot', 'dot_no_facet', 'facethist', 'facetdensity', 'denstrip', 'blank'),
                  selectize = TRUE),

      # Input: Selector for choosing Continuous for GGPairs ----
      selectInput(inputId = "upper_ggdiscrete",
                  label = "Discrete",
                  choices = c('facetbar', 'ratio', 'blank'),
                  selectize = TRUE)
      ),
      ),

      checkboxInput("check_reg", "Show Regression Options", FALSE),

      div(id="opt_reg",
      # Input: Selector for choosing Input Columns ----
      selectInput(inputId = "input_colnames",
                  label = "Input Variables",
                  choices = colnames(dm),
                  multiple=TRUE),      
      
      # Input: Selector for choosing Output Column ----
      selectInput(inputId = "output_colname",
                  label = "Output Variable",
                  choices = colnames(dm)
                  ),

      sliderInput(inputId = "slider_training", label = "Training Sample Size", min=50, max=90, value=70, step=5),

      selectInput(inputId = "use_interactions",
                  label = "Use Interactions",
                  choices = c('No', 'Yes')
                  )

      ),

      # Button to reset the input form
      actionButton("resetButton", "Reset Form")
      
      ),      

     ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      width = 10,
      tabsetPanel(type = "tabs", selected = "Table",
        tabPanel("Data Table", tableOutput(outputId = "TableView")),
        tabPanel("Correlation Plot", plotOutput(outputId = "CorrPlot", width="700px", height="700px")),
        tabPanel("Performance Plot", plotOutput(outputId = "PerfPlot", width="700px", height="700px")),
        tabPanel("GG Pairs", plotOutput(outputId = "GGPairs", width="700px", height="700px")),
        tabPanel("Regression",
		 fluidRow(
                   column(5, verbatimTextOutput(outputId = "RegSummaryView")),
                   column(7, plotOutput(outputId = "RegressionView", height="700px"))
                 )
                )
        )
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {

  observeEvent(input$datasource, {
    if (input$datasource == "uploadcsv") {
    shinyjs::hide("clipboard")
    shinyjs::hide("pasteButton")
    shinyjs::show("inputfile")
    } else {
    shinyjs::show("clipboard")
    shinyjs::show("pasteButton")
    shinyjs::hide("inputfile")
    }
  })

  get_file <- eventReactive(input$inputfile, {
    req(input$inputfile, file.exists(input$inputfile$datapath))
    dm <<- as.data.frame(read.csv(input$inputfile$datapath, sep=","))
  })

  get_clipboard <- eventReactive(input$pasteButton, {
    dm <<- as.data.frame(read.table(
      "clipboard", header = TRUE, sep = "\t",
      comment.char = "", fill = TRUE, as.is = TRUE,
      check.names = FALSE
    ))
  })

  observeEvent(input$check_correlation, {
    shinyjs::toggle("opt_correlation")
  })

  observeEvent(input$check_ggpairs, {
    shinyjs::toggle("opt_ggpairs")
  })

  observeEvent(input$check_reg, {
    shinyjs::toggle("opt_reg")
  })

  observeEvent(input$pasteButton, {
    reset("clipboard")
  })
  
  observeEvent(input$corrlayout, {
    if (input$corrlayout == "mixed") {
    shinyjs::hide("corrmethod")
    shinyjs::show("upperlower")
    } else {
    shinyjs::hide("upperlower")
    shinyjs::show("corrmethod")
    }
  })

  observeEvent(input$corrsort, {
    if ((input$corrsort == "hclust") & (input$corrlayout != "mixed")) {
    shinyjs::show("addrects")
    } else {
    shinyjs::hide("addrects")
    }
  })

  observeEvent(input$resetButton, {
    remove(dm)
    remove(df)
    reset("form");
  })

  observe ({
    ifelse (input$datasource == "uploadcsv", get_file(), get_clipboard())
    updateSelectInput(session, "input_colnames", choices = colnames(dm))
    updateSelectInput(session, "output_colname", choices = colnames(dm))
  })

  output$RegSummaryView <- renderPrint({

    ifelse (input$datasource == "uploadcsv", get_file(), get_clipboard())

    df1 <- dm %>% select(input$input_colnames)
    df2 <- dm %>% select(input$output_colname)

    df_plot <<- as.data.frame(cbind(df1, df2))

    sample <- sample(c(TRUE, FALSE), nrow(df_plot), replace = T, prob = c(input$slider_training/100,(100-input$slider_training)/100))
    train <<- df_plot[sample, ]
    test <<- df_plot[!sample, ]
    
    if (input$use_interactions == "Yes") {
    eval(parse(text=toString(paste("fit <<- lm(", input$output_colname, " ~ (. - ", input$output_colname, ")^2, train)", sep=""))))
    } else {
    eval(parse(text=toString(paste("fit <<- lm(", input$output_colname, " ~ . - ", input$output_colname, ", train)", sep=""))))
    }

    test <- test %>% 
    add_predictions(fit)
    eval(parse(text=toString(paste("test$diff <- test$", input$output_colname, "- test$pred", sep=""))))
    test <- test %>%
    summarise(MSE = mean(diff^2))

    train <- train %>% 
    add_predictions(fit)
    eval(parse(text=toString(paste("train$diff <- train$", input$output_colname, "- train$pred", sep=""))))
    train <- train %>%
    summarise(MSE = mean(diff^2))

    multi_return <- function() {
    my_list <- list(summary(fit), confint(fit), test, train, model_eqn)
    return(my_list) 
    }

    multi_return()

  })

  output$RegressionView <- renderPlot({

    #Reactive expression to ensure the entire renderPlot is reactive too
    df1 <- dm %>% select(input$input_colnames)
    df2 <- dm %>% select(input$output_colname)
    temp <- input$slider_training

    model1_results <- augment(fit, train)

    p1 <- ggplot(model1_results, aes(.fitted, .std.resid)) +
      geom_ref_line(h = 0) +
      geom_point(shape=1) +
      geom_smooth(se = FALSE, color='red') +
      theme_bw() +
      ggtitle("Standardized Residuals vs Fitted")

    p2 <- ggplot(model1_results, aes(.fitted, sqrt(.std.resid))) +
      geom_ref_line(h = 0) +
      geom_point(shape=1) +
      geom_smooth(se = FALSE, color='red') +
      theme_bw() +
      ggtitle("Scale-Location")

    p3 <- ggplot(model1_results, aes(sample = model1_results$.resid)) +
      geom_qq(shape=1) +
      geom_qq_line() +
      theme_bw() +
      ggtitle("Normal Q-Q Plot")

    p4 <- ggplot(model1_results, aes(seq_along(.cooksd), .cooksd)) +
      geom_col() +
      theme_bw() +
      ggtitle("Cook's Distance")

    p5 <- ggplot(model1_results, aes(.hat, .std.resid)) +
      geom_point(aes(size = .cooksd), shape=1) +
      geom_smooth(se = FALSE, color='red') +
      theme_bw() +
      ggtitle("Residuals vs Leverage")

    format_args <- list()
    model_coeff <- fit$coefficients
    format_args$x <- abs(fit$coefficients)
    model_coeff_sign <- sign(model_coeff)
    model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                  model_coeff_sign == 1 ~ " + ",
                                  model_coeff_sign == 0 ~ " + ")
    model_eqn <<- paste(strsplit(as.character(fit$call$formula), "~")[[2]], # 'y'
                     "=",
                     paste(if_else(model_coeff[1]<0, "- ", ""),
                           do.call(format, format_args)[1],
                           paste(model_coeff_prefix[-1],
                                 do.call(format, format_args)[-1],
                                 " * ",
                                 names(model_coeff[-1]),
                                 sep = "", collapse = ""),
                           sep = ""))

    #Delete output and = in model_eqn
    #Replace column headings with df_plot$
    fit_formula <<- paste("df_plot$newoutput <- ", model_eqn, sep="")
    fit_formula <- str_replace_all(fit_formula, colnames(df_plot)[ncol(df_plot)], "")
    fit_formula <- str_replace_all(fit_formula, " = ", "")
    for (r in 1:(ncol(df_plot)-1)) {
      fit_formula <- str_replace_all(fit_formula, colnames(df_plot)[r], paste("df_plot$", colnames(df_plot)[r], sep=""))
    }

    fit_formula <- str_replace_all(fit_formula, ":", "*")

    eval(parse(text=toString(fit_formula)))
    p6 <<- ggplot(df_plot, aes(x=as.numeric(row.names(df_plot)))) +
      geom_line(aes_string(y=colnames(df_plot)[ncol(df_plot)-1]), color='red') +
      geom_line(aes_string(y=df_plot$newoutput)) +
      geom_point(aes_string(y=colnames(df_plot)[ncol(df_plot)-1]), shape=15, size=2, color='red') +
      geom_point(aes_string(y=df_plot$newoutput), shape=16, size=2) +
      theme_bw() +
      ggtitle("Actual vs Model")

    multiplot(p1, p2, p3, p4, p5, p6, cols=2)

  })

  output$TableView <- renderTable({

    ifelse (input$datasource == "uploadcsv", get_file(), get_clipboard())
    return(dm)

  })

  output$CorrPlot <- renderPlot({

    ifelse (input$datasource == "uploadcsv", get_file(), get_clipboard())

    #Remove non-numeric columns from the dataframe
    rmcols <- rev(seq(1,ncol(dm))[!as.logical(sapply(dm, is.numeric))])
    for (i in rmcols) dm[[i]] <- NULL

    #Create correlation plot

    res <- cor.mtest(dm, conf.level = input$siglevel)
    M <- cor(dm)
 
    if (input$corrlayout == "mixed") {
      if (input$corrsort == "none") {
      corrplot.mixed(M, p.mat=res$p, tl.col=input$textcolor, lower=input$corrmethod_lower, upper=input$corrmethod_upper, sig.level=(1-input$siglevel))
      } else {
      corrplot.mixed(M, p.mat=res$p, order=input$corrsort, addrect=input$addrects, tl.col=input$textcolor, lower=input$corrmethod_lower, upper=input$corrmethod_upper, sig.level=(1-input$siglevel))
      }
    } else {
      if (input$corrsort == "none") {
      corrplot(M, p.mat=res$p, method=input$corrmethod, tl.col=input$textcolor, type=input$corrlayout, sig.level=(1-input$siglevel))
      } else {
      corrplot(M, p.mat=res$p, order=input$corrsort, addrect=input$addrects, method=input$corrmethod, tl.col=input$textcolor, type=input$corrlayout, sig.level=(1-input$siglevel))
      }
    }

  })

  output$PerfPlot <- renderPlot({

    ifelse (input$datasource == "uploadcsv", get_file(), get_clipboard())

    #Remove non-numeric columns from the dataframe
    rmcols <- rev(seq(1,ncol(dm))[!as.logical(sapply(dm, is.numeric))])
    for (i in rmcols) dm[[i]] <- NULL

    print(chart.Correlation(dm, histogram=TRUE))

  })

  output$GGPairs <- renderPlot({

    ifelse (input$datasource == "uploadcsv", get_file(), get_clipboard())

    ggpairs(dm, upper = list(continuous = input$upper_ggcontinuous, combo = input$upper_ggcombo, discrete = input$upper_ggdiscrete), lower = list(continuous = input$lower_ggcontinuous, combo = input$lower_ggcombo, discrete = input$lower_ggdiscrete))

  })
    
}

# Create Shiny app ----
shinyApp(ui, server)
runApp(app,host="0.0.0.0",port=5058)
