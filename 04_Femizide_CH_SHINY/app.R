
# load packages and data --------------------------------------------------
# Pakete laden
library(tidyverse)
library(shiny)
library(htmlwidgets)
library(ggiraph)

### 1. Data
year_files <- c(
  "Overall"  = "01_Data/Femizide_Schweiz_Overall.rds",
  "2025" = "01_Data/Femizide_Schweiz_2025.rds",
  "2024" = "01_Data/Femizide_Schweiz_2024.rds",
  "2023" = "01_Data/Femizide_Schweiz_2023.rds",
  "2022" = "01_Data/Femizide_Schweiz_2022.rds",
  "2020" = "01_Data/Femizide_Schweiz_2020.rds"
)
plots_by_year <- map(year_files, ~ read_rds(.x))


### 2. Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$style(HTML("
      .nav-tabs > li > a {
        background-color: #fbeaea;
        color: #000000;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: #ffffff;
        color: #000000;
      }
      .nav-tabs > li > a:hover {
        background-color: #ffffff;
        color: #000000;
      }
      .impressum-text {
      font-size: 3rem;  
      }
      .impressum-text a {
      color: #eba4a4; 
      text-decoration: underline;
      }
    
    ")),
  
  
  titlePanel("(Versuchte) Femizide in der Schweiz"), br(), br(),
  
  # Graphs
  do.call(
    tabsetPanel,
    c(list(id = "year_tab"),
      lapply(names(plots_by_year), function(y) {
        tabPanel(
          title = y,
          girafeOutput(
            outputId = paste0("plot_", y),
            width    = "75%",
            height   = "calc(100vw * 0.5)"
          )
        )
      }),
      
      # Impressum
      list(
        tabPanel(
          title = "Impressum",
          fluidRow(
            column(
              width = 12,
              tags$br(),
              tags$br(),
              tags$br(),
              tags$hr(),
              tags$p(
                class = "impressum-text",
                "Recherche und Dokumentation der Daten:",
                tags$a(href = "https://www.stopfemizid.ch/",
                       "stopfemizid")
              ),
              tags$hr(),
              tags$p(
                class = "impressum-text",
                "Shiny-App (Visualisierung):",
                tags$a(href = "https://www.linkedin.com/in/luca-keiser-806329285",
                       "Luca Keiser"),
                " | ",
                tags$a(href = "https://github.com/LucaKeiser/Femizide_Schweiz",
                       "Code"),
                tags$hr()
              )
            )
          )
        )
      )
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
