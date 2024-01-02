#!/usr/bin/env Rscript
# Shiny app to create histogram of the number of genes of selected cells in the Tabular Sapiens dataset
# nikola

library(shiny)
library(shinythemes)

options(stringsAsFactors = FALSE)

wd <- "/home/nikola/Project_Data/R_data/cell_meta_TS_shiny"
setwd(wd)

dir_i <- "/home/nikola/Project_Data/Python_data/Spyder/Tabular_Sapiens/output_figshare/meta"

## import data
cell_mt <- read.csv(paste(dir_i, "obs.csv", sep = "/"), header = TRUE)
# head(cell_mt)
# str(cell_mt)

## build shiny app
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Numbers of genes of selected cells in the Tabular Sapiens dataset"),
  sidebarLayout(
    sidebarPanel(
      HTML("<h3>Input parameters</h3>"),
      selectInput(inputId = "compartment", label = "Compartment", choices = unique(cell_mt$compartment)), selected = "immune",
      selectInput(inputId = "organ", label = "Organ", choices = unique(cell_mt$organ_tissue), selected = "Blood"),
      selectInput(inputId = "cell_label", label = "Cell label", choices = unique(cell_mt$cell_ontology_class), selected = "classical monocyte"),
      selectInput(inputId = "method", label = "Method", choices = unique(cell_mt$method), selected = "10X"),
      sliderInput(
        inputId = "bins",
        label = "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      ),
    ),
    mainPanel(
      tags$label(h3("Output")),
      plotOutput(outputId = "hist")
    )
  )
)

server <- function(input, output) {
  select <- reactive({
    cell_mt$compartment == input$compartment &
      cell_mt$organ_tissue == input$organ &
      cell_mt$free_annotation == input$cell_label &
      cell_mt$method == input$method
  })

  df <- reactive({
    cell_mt[select(), ]
  })

  output$hist <- renderPlot({
    x <- na.omit(df()$n_genes)

    if (length(x) == 0) {
      plot(0, type = "n", xlab = "Number of genes", ylab = "", main = "No cells found")
    } else {
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x,
        breaks = bins, col = "#75AADB", border = "black",
        xlab = "Number of genes",
        main = "Histogram of the number of genes"
      )
    }
  })
}

shinyApp(ui = ui, server = server)

# run this command in R console to launch the Shiny app
# runApp("cell_meta_Tabular_Sapiens_shiny.r")
