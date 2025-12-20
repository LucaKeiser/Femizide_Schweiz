
# load packages and data --------------------------------------------------
library(tidyverse)
library(shiny)
library(htmlwidgets)
library(ggiraph)

# Data
year_files <- c(
  "Overall"  = "01_Data/Femizide_Schweiz_Overall.rds",
  "2025" = "01_Data/Femizide_Schweiz_2025.rds",
  "2024" = "01_Data/Femizide_Schweiz_2024.rds",
  "2023" = "01_Data/Femizide_Schweiz_2023.rds",
  "2022" = "01_Data/Femizide_Schweiz_2022.rds",
  "2020" = "01_Data/Femizide_Schweiz_2020.rds"
)
plots_by_year <- map(year_files, ~ read_rds(.x))


# Define UI ---------------------------------------------------------------
ui <- fluidPage(
  
  ### 1. Styling
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
  
  # Info box
  tags$div(
    class = "alert alert-info alert-dismissible", 
    style = "margin-top: 20px;",
    tags$button(
      type = "button",
      class = "close",
      `data-dismiss` = "alert",
      `aria-label` = "Close",
      tags$span(`aria-hidden` = "true", HTML("&times;"))
    ),
    strong("Hinweis zum Gebrauch mit dem Smartphone:"),   
    HTML("
        <li>Bitte Querformat verwenden.</li>
        <li>Damit die zusätzlichen Informationen auf Kantonsebene sichtbar werden, etwas länger auf den entsprechenden Kanton drücken.</li>
    ")
  ),
  
  titlePanel("(Versuchte) Femizide in der Schweiz"), br(), br(),
  
  ### 2. Tabs
  # Maps
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
              tags$br(), tags$br(), tags$br(), 
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


# Server logic ------------------------------------------------------------
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


# Run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
