# Pakete laden ------------------------------------------------------------
library(tidyverse)
library(ggiraph)
library(sf)
library(maps)
library(htmlwidgets)

`%ni%` <- negate(`%in%`)



# Daten laden und aufbereiten ---------------------------------------------
df_2025 <- readxl::read_xlsx("01_Data/Femizide/femizide_Schweiz_raw.xlsx",
                             sheet = "2025") %>% 
  mutate(date = as.Date(date))
df_2024<- readxl::read_xlsx("01_Data/Femizide/femizide_Schweiz_raw.xlsx",
                            sheet = "2024") %>% 
  mutate(date = as.Date(date))
df_2023 <- readxl::read_xlsx("01_Data/Femizide/femizide_Schweiz_raw.xlsx",
                             sheet = "2023") %>% 
  mutate(date = as.Date(date))
df_2022 <- readxl::read_xlsx("01_Data/Femizide/femizide_Schweiz_raw.xlsx",
                             sheet = "2022") %>% 
  mutate(date = as.Date(date))
df_2020 <- readxl::read_xlsx("01_Data/Femizide/femizide_Schweiz_raw.xlsx",
                             sheet = "2020") %>% 
  mutate(date = as.Date(date))

df <- df_2025 %>% 
  bind_rows(df_2024,
            df_2023,
            df_2022,
            df_2020)

# Shape-File Schweiz
shape_file <- read_sf("01_Data/Shapefile_Switzerland/swissBOUNDARIES3D_1_4_TLM_KANTONSGEBIET.shp")
shape_agg <- shape_file %>%
  group_by(NAME) %>%      
  summarise(geometry = st_union(geometry), 
            .groups = "drop")

### Daten aufbereiten
df <-  df %>% 
  mutate(year = year(date),
         year = as.numeric(year)) %>% 
  mutate(canton_shapefile = case_when(canton == "Wallis" ~ "Valais",
                                      canton == "Tessin" ~ "Ticino",
                                      canton == "Biel" ~ "Bern",
                                      canton == "Freiburg" ~ "Fribourg",
                                      canton == "Neuenburg" ~ "Neuchâtel",
                                      canton == "Sankt Gallen" ~ "St. Gallen",
                                      canton == "Waadt" ~ "Vaud",
                                      canton == "Genf" ~ "Genève",
                                      TRUE ~ canton)) %>% 
  add_count(year, canton_shapefile,
            name = "n_femizid") %>% 
  select(rowid, 
         year,
         date,
         category, 
         canton, 
         canton_shapefile,
         community,
         info, 
         n_femizid)

# Info für Tooltipp erstellen
df <- df %>% 
  arrange(date) %>% 
  mutate(date = format(date, "%d.%m.%Y")) %>% 
  mutate(info_tooltipp = paste0(date, ": ", community, "; ", category, ": ", info)) %>% 
  group_by(year, canton_shapefile) %>%
  mutate(info_tooltipp = str_c(info_tooltipp, 
                               collapse = "<br>")) %>% 
  ungroup() %>% 
  mutate(info_tooltipp = paste0("<b>", canton_shapefile, "</b>", "<br>",
                                "Anzahl (versuchte) Feminzide: ", n_femizid, "<br><br>",
                                info_tooltipp)) %>% 
  mutate(info_tooltipp = str_remove_all(info_tooltipp,
                                        "NA"))



# Outputs erstellen -------------------------------------------------------
for(year_search in unique(df$year)) {
  
  message(year_search)
  
  ### 1. Daten aufbereiten
  df_shiny <- df %>% 
    filter(year == year_search)
  
  missing_cantons <- shape_agg %>% 
    anti_join(df_shiny,
              by = c("NAME" = "canton_shapefile")) %>% 
    pull(NAME)
  
  for(canton in missing_cantons) {
    df_shiny <- df_shiny %>% 
      add_row(canton_shapefile = canton,
              info_tooltipp = paste0("<b>", canton, "</b>", "<br>Keine Informationen verfügbar"))
  }
  
  n_total <- df_shiny %>% 
    count(n_femizid, category) %>% 
    filter(!is.na(n_femizid)) %>% 
    group_by(category) %>% 
    summarise(total = sum(n))
  
  p1 <- shape_agg %>% 
    left_join(df_shiny,
              by = c("NAME" = "canton_shapefile")) %>% 
    distinct() %>% 
    ggplot(aes(tooltip = info_tooltipp, 
               data_id = rowid)) +
    geom_sf_interactive(aes(fill = n_femizid),
                        color = "grey",
                        hover_nearest = TRUE,
                        show.legend = FALSE) + 
    scale_fill_gradient2(low = "#eba4a4",
                         mid = "#ec5353",
                         high =  "#d12e2e",
                         midpoint = 3,
                         na.value = "#ffe4e0") + 
    labs(title = paste0(year_search, " - Anzahl Femizide: ", n_total[[1, 2]],  " | Anzahl Versuchte Femizide: ", n_total[[2, 2]]),
         caption = "stopfemizid versucht, jede Tat zu dokumentieren. Dennoch ist diese Liste als unvollständig zu betrachten.\nDaten: https://www.stopfemizid.ch/ | Visualisierung: Luca Keiser") +
    theme_void()
  
  
  ### 2. Karte erstellen
  p1_interactive <- girafe(ggobj = p1,
                           options = list(
                             opts_hover(css = "fill: #990000"), 
                             # opts_hover_inv(css = "fill: gray"),
                             opts_toolbar(saveaspng = FALSE),
                             opts_sizing(rescale = TRUE, width = 0.75),
                             opts_selection(type = "none")
                           ))
  
  p1_interactive_shiny <- girafe(ggobj = p1,
                                 options = list(
                                   opts_hover(css = "fill: #990000"), 
                                   # opts_hover_inv(css = "fill: gray"),
                                   opts_toolbar(saveaspng = FALSE),
                                   opts_sizing(rescale = TRUE, width = 1),
                                   opts_selection(type = "none")
                                 ))
  
  
  ### 3. Speichern
  htmlwidgets::saveWidget(p1_interactive, 
                          paste0("03_Output/Femizide_Schweiz_", year_search, ".html"),
                          selfcontained = TRUE)
  write_rds(p1_interactive_shiny,
            paste0("04_Femizide_CH_SHINY/01_Data/Femizide_Schweiz_", year_search, ".rds"))
  
}
