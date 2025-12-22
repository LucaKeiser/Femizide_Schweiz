# Pakete laden ------------------------------------------------------------
library(tidyverse)
library(ggiraph)
library(sf)
library(maps)
library(htmlwidgets)
library(shinyWidgets)

`%ni%` <- negate(`%in%`)


# Daten laden und aufbereiten ---------------------------------------------
### Daten stopfemizid.ch
sheets <- c(2025:2022, 2020)
for(sheet in seq_along(sheets)) { 
  
  df_temp <- readxl::read_xlsx("01_Data/Femizide/femizide_Schweiz_raw.xlsx",
                               sheet = sheet) %>% 
    mutate(date = as.Date(date))
  assign(paste0("df_", sheets[sheet]), df_temp)
  
}

df <- df_2025 %>% 
  bind_rows(df_2024,
            df_2023,
            df_2022,
            df_2020)

### Shape-Files Schweiz
shape_file <- read_sf("01_Data/Shapefile_Switzerland/swissBOUNDARIES3D_1_5_TLM_KANTONSGEBIET.shp")
shape_agg <- shape_file %>%
  group_by(NAME) %>%      
  summarise(geometry = st_union(geometry), 
            .groups = "drop") %>% 
  group_by(NAME) %>% 
  rowid_to_column(var = "id_interactive_plot")

# shape-files angleichen
swiss_lakes <- ggswissmaps::shp_sf$g1s15
sf::st_crs(swiss_lakes) <- sf::st_crs(swiss_lakes, 2056)

### Daten aufbereiten
df <- df %>% 
  mutate(year = year(date),
         year = as.numeric(year)) %>% 
  mutate(canton_shapefile = case_when(canton == "Basel" ~ "Basel-Stadt",
                                      canton == "Biel" ~ "Bern",
                                      canton == "Freiburg" ~ "Fribourg",
                                      canton == "Genf" ~ "Genève",
                                      canton == "Neuenburg" ~ "Neuchâtel",
                                      canton == "Sankt Gallen" ~ "St. Gallen",
                                      canton == "Tessin" ~ "Ticino",
                                      canton == "Waadt" ~ "Vaud",
                                      canton == "Wallis" ~ "Valais",
                                      TRUE ~ canton)) %>% 
  group_by(year, canton_shapefile, category) %>% 
  mutate(n_femizid = if_else(category == "<u>Femizid</u>", sum(factor), 0),
         n_versuchter_femizid = if_else(category == "Versuchter Femizid", sum(factor), 0)) %>%
  select(rowid, 
         year,
         date,
         category, 
         factor,
         canton, 
         canton_shapefile,
         community,
         info,
         n_femizid,
         n_versuchter_femizid)

### Info für Tooltipp erstellen
df <- df %>% 
  arrange(date) %>% 
  mutate(date = format(date, "%d.%m.%Y")) %>% 
  mutate(info_tooltipp = paste0(date, ": ", community, "; ", category, ": ", info)) %>% 
  group_by(year, canton_shapefile) %>%
  mutate(info_tooltipp = str_c(info_tooltipp, 
                               collapse = "<br>")) %>% 
  group_by(year, canton_shapefile) %>% 
  mutate(info_tooltipp = paste0("<b>", canton_shapefile, "</b>", "<br>",
                                "<u>Femizide</u>: ", max(n_femizid), " | Versuchte Femizide: ", max(n_versuchter_femizid), "<br><br>",
                                info_tooltipp)) %>% 
  ungroup() %>% 
  mutate(info_tooltipp = str_remove_all(info_tooltipp,
                                        "NA"))


# Outputs erstellen -------------------------------------------------------
## Overall ----------------------------------------------------------------
### 1. Daten aufbereiten
df_shiny <- df %>% 
  select(year, canton_shapefile, n_femizid, n_versuchter_femizid) %>% 
  distinct() %>% 
  group_by(canton_shapefile) %>% 
  mutate(n_femizid = sum(n_femizid),
         n_versuchter_femizid = sum(n_versuchter_femizid)) %>% 
  select(-year) %>% 
  distinct() %>%
  mutate(info_tooltipp = paste0("<b>", canton_shapefile, "</b>", "<br>",
                                "Anzahl Feminzide: ", n_femizid, "<br>",
                                "Anzahl versuchte Femizide: ", n_versuchter_femizid)) %>% 
  mutate(info_tooltipp = str_remove_all(info_tooltipp,
                                        "NA")) %>%
  ungroup() %>% 
  select(canton_shapefile, info_tooltipp, n_femizid, n_versuchter_femizid) %>% 
  distinct()

# Fehlende Kantone hinzufügen
missing_cantons <- shape_agg %>% 
  anti_join(df_shiny,
            by = c("NAME" = "canton_shapefile")) %>% 
  pull(NAME)

for(canton in missing_cantons) {
  df_shiny <- df_shiny %>% 
    add_row(canton_shapefile = canton,
            info_tooltipp = paste0("<b>", canton, "</b>", "<br>Keine Informationen verfügbar"))
}

# Total pro Kategorie berechnen
n_total <- df_shiny %>% 
  summarise(total_fem = sum(n_femizid , na.rm = TRUE),
            totl_v_fem = sum(n_versuchter_femizid, na.rm = TRUE))

# Roh-Grafik erstellen
p1 <- shape_agg %>% 
  left_join(df_shiny,
            by = c("NAME" = "canton_shapefile")) %>% 
  distinct() %>% 
  ggplot() +
  geom_sf_interactive(aes(tooltip = info_tooltipp, 
                          data_id = id_interactive_plot,
                          fill = n_femizid),
                      colour = "#ffffff",
                      show.legend = FALSE,
                      size = 0.05) + 
  scale_fill_gradient2(low = "#eba4a4",
                       mid = "#ec5353",
                       high =  "#d12e2e",
                       midpoint = 12,
                       na.value = "#eedada") + 
  geom_sf(data = swiss_lakes,
          fill = "#add8e6", 
          colour = NA) + 
  labs(title = paste0("Overall - Anzahl Femizide: ", n_total$total_fem,  " | Anzahl Versuchte Femizide: ", n_total$totl_v_fem),
       caption = "stopfemizid versucht jede Tat zu dokumentieren. Dennoch sind die dargestellten Informationen als unvollständig zu betrachten.") +
  theme_void() +
  theme(
    plot.caption = element_text(
      size = 6,
      face = "italic",
      hjust = 1)
  )

### 2. interaktive Karte erstellen
p1_interactive_shiny <- girafe(ggobj = p1,
                               options = list(
                                 opts_hover(css = "fill:#D8BFD8"),
                                 opts_hover_inv(css = ""),
                                 opts_toolbar(saveaspng = FALSE),
                                 opts_sizing(rescale = TRUE, width = 1)
                               ))

### 3. Speichern
write_rds(p1_interactive_shiny,
          paste0("03_Femizide_CH_SHINY/01_Data/Femizide_Schweiz_Overall.rds"))




## Auf Jahresebene --------------------------------------------------------
for(year_search in unique(df$year)) {
  
  message(year_search)
  
  ### 1. Daten aufbereiten
  df_shiny <- df %>% 
    filter(year == year_search)
  
  # Fehlende Kantone hinzufügen
  missing_cantons <- shape_agg %>% 
    anti_join(df_shiny,
              by = c("NAME" = "canton_shapefile")) %>% 
    pull(NAME)
  
  for(canton in missing_cantons) {
    df_shiny <- df_shiny %>% 
      add_row(canton_shapefile = canton,
              info_tooltipp = paste0("<b>", canton, "</b>", "<br>Keine Informationen verfügbar"))
  }
  
  # Total pro Kategorie berechnen
  n_total <- df_shiny %>% 
    select(canton_shapefile, n_femizid, n_versuchter_femizid) %>% 
    distinct() %>% 
    summarise(n_femizid = sum(n_femizid, na.rm = TRUE),
              n_versuchter_femizid = sum(n_versuchter_femizid, na.rm = TRUE))
  
  # Roh-Grafik erstellen
  p1 <- shape_agg %>% 
    left_join(df_shiny,
              by = c("NAME" = "canton_shapefile")) %>% 
    distinct() %>% 
    ggplot() +
    geom_sf_interactive(aes(fill = n_femizid,
                            tooltip = info_tooltipp, 
                            data_id = id_interactive_plot),
                        colour = "#ffffff",
                        show.legend = FALSE,
                        size = 0.05) + 
    scale_fill_gradient2(low = "#eba4a4",
                         mid = "#ec5353",
                         high =  "#d12e2e",
                         midpoint = 3,
                         na.value = "#eedada") + 
    geom_sf(data = swiss_lakes,
            fill = "#add8e6", 
            colour = NA) + 
    labs(title = paste0(year_search, " - Anzahl Femizide: ", n_total[[1, 1]],  " | Anzahl Versuchte Femizide: ", n_total[[1, 2]]),
         caption = "stopfemizid versucht jede Tat zu dokumentieren. Dennoch sind die dargestellten Informationen als unvollständig zu betrachten.") +
    theme_void() +
    theme(
      plot.caption = element_text(
        size = 6,
        face = "italic",
        hjust = 1)
    )
  
  ### 2. interaktive Karte erstellen
  p1_interactive_shiny <- girafe(ggobj = p1,
                                 options = list(
                                   opts_hover(css = "fill:#D8BFD8"),
                                   opts_hover_inv(css = ""),
                                   opts_toolbar(saveaspng = FALSE),
                                   opts_sizing(rescale = TRUE, width = 1)
                                 ))
  
  ### 3. Speichern
  write_rds(p1_interactive_shiny,
            paste0(
              "03_Femizide_CH_SHINY/01_Data/Femizide_Schweiz_", 
              year_search, 
              ".rds")
  )
  
}


# Dummy-Datei für 2021 ----------------------------------------------------
# Für das Jahr 2021 sind keine Daten verfügbar.
write_rds(tibble(),
          "03_Femizide_CH_SHINY/01_Data/Femizide_Schweiz_DUMMY.rds")
