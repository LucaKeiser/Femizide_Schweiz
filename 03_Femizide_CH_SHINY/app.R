
# load packages and data --------------------------------------------------
library(tidyverse)
library(shiny)
library(htmlwidgets)
library(ggiraph)
library(shinyWidgets)

# Data
year_files <- c(
  "Overall"  = "01_Data/Femizide_Schweiz_Overall.rds",
  "2025" = "01_Data/Femizide_Schweiz_2025.rds",
  "2024" = "01_Data/Femizide_Schweiz_2024.rds",
  "2023" = "01_Data/Femizide_Schweiz_2023.rds",
  "2022" = "01_Data/Femizide_Schweiz_2022.rds",
  "2021"    = "01_Data/Femizide_Schweiz_DUMMY.rds", 
  "2020" = "01_Data/Femizide_Schweiz_2020.rds"
)
plots_by_year <- map(year_files, ~ read_rds(.x))


# Define UI ---------------------------------------------------------------
ui <- fluidPage(
  
  ### 1. Styling
  tags$head(
    tags$title("Femizide Schweiz")
  ),
  
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
    strong("Hinweis zum Gebrauch mit Smartphones/Tablets:"),   
    HTML("<br>
        - Bitte Querformat verwenden.<br>
        - <i>Hover-Effekt</i> ist nur <i>eingeschränkt</i> verfügbar. Anzeigedauer pro Tap einstellen (Workaround).")
  ),
  
  hr(),
  
  prettyRadioButtons(
    inputId = "mouseout_delay_s",
    label   = strong("Anzeigedauer pro Tap:"),
    choices = c("0s" = 0.25, 
                "5s" = 5, 
                "10s" = 10, 
                "30s" = 30),
    selected = 0.25,
    inline = TRUE,
    status = "danger",
    shape = "curve",
    animation = "pulse"
  ),
  hr(),
  
  titlePanel(
    strong(
      "(Versuchte) Femizide in der Schweiz")
  ), 
  br(),
  
  ### 2. Tabs
  # Maps
  do.call(
    tabsetPanel,
    c(list(id = "year_tab"),
      
      lapply(names(plots_by_year), function(y) {
        tabPanel(
          title = y,
          
          # No data for 2021
          if (y == "2021") {
            tags$div(
              class = "alert alert-warning",
              style = "margin-top: 20px;",
              tags$strong("Hinweis: "),
              "Für das Jahr 2021 liegen keine Daten vor."
            )
            # Ohter years
          } else {
            girafeOutput(
              outputId = paste0("plot_", y),
              width    = "70%",
              height   = "calc(100vw * 0.5)"
            )
          }
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
              tags$i("\"Femizide sind keine Einzelfälle, sondern Resultat von struktureller Gewalt,
                     deren Ausgangspunkt in den patriarchalen Machtverhältnissen unserer Gesellschaft 
                     liegt. Gewalt gegen Frauen wird noch oft als Privatsache behandelt, was sich am 
                     gesellschaftlichen Umgang damit ablesen lässt: Der Begriff Femizid ist in der 
                     Schweiz noch immer kein etablierter politischer Begriff. Um Gewalt gegen Frauen 
                     möglichst umfassend zu dokumentieren, zählen wir nicht nur Femizide in Folge 
                     häuslicher Gewalt, sondern auch die Femizide, in denen die Täter keine Beziehung 
                     zu den Opfern hatten, Fälle von rassistischen, homo-, transphoben und 
                     behindertenfeindlichen Motiven, und solche an Sexarbeiterinnen. Wir versuchen, 
                     jede Tat zu dokumentieren. Wir wissen: Die Liste ist unvollständig.\" - stopfemizid.ch"),
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
        
        # set delay_mouseout based on user input
        delay_ms <- as.numeric(input$mouseout_delay_s) * 1000
        girafe_options(
          plot_obj,
          opts_tooltip(delay_mouseover = 0,
                       delay_mouseout  = delay_ms,
                       opacity = 0.75,
                       use_cursor_pos  = TRUE,
                       placement = "container"),
          opts_selection(type = "single",
                         only_shiny = TRUE,
                         css = "")
        )
      })
      
    }
  )
}


# Run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
