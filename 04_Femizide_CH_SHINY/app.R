
# load packages and data --------------------------------------------------
# Pakete laden
library(tidyverse)
library(shiny)
library(htmlwidgets)
library(ggiraph)

### 1. Data
year_files <- c(
  "2025" = "01_Data/Femizide_Schweiz_2025.rds",
  "2024" = "01_Data/Femizide_Schweiz_2024.rds",
  "2023" = "01_Data/Femizide_Schweiz_2023.rds",
  "2022" = "01_Data/Femizide_Schweiz_2022.rds",
  "2020" = "01_Data/Femizide_Schweiz_2020.rds"
)
plots_by_year <- map(year_files, ~ read_rds(.x))


### 2. Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("(Versuchte) Femizide in der Schweiz"),
  
  do.call(
    tabsetPanel,
    c(
      list(id = "year_tab"),                     
      lapply(names(plots_by_year), function(y) { 
        tabPanel(
          title = y,
          girafeOutput(
            outputId = paste0("plot_", y),
            width  = "80%",
            height = "calc(100vw * 0.6)"
          )
        )
      })
    )
  )
)


### 3. Define server logic
server <- function(input, output, session) {
  
  walk2(
    .x = names(plots_by_year),
    .y = plots_by_year,
    .f = function(y, plot_obj) {
      output[[paste0("plot_", y)]] <- renderGirafe({
        plot_obj
      })
    }
  )
}

### 4. Run the application 
shinyApp(ui = ui, server = server)
