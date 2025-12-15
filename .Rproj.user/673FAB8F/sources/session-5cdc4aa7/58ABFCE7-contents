# Pakete laden ------------------------------------------------------------
library(tidyverse)
library(ggiraph)
library(sf)
library(maps)
library(htmlwidgets)

`%ni%` <- negate(`%in%`)



# Daten laden und aufbereiten ---------------------------------------------
df <- readxl::read_xlsx("01_Data/Femizide/femizide_Schweiz_raw.xlsx") %>% 
  mutate(date = as.Date(date))

shape_file <- read_sf("01_Data/Shapefile_Switzerland/swissBOUNDARIES3D_1_4_TLM_KANTONSGEBIET.shp")
shape_agg <- shape_file %>%
  group_by(NAME) %>%      
  summarise(geometry = st_union(geometry), 
            .groups = "drop")

# Daten aufbereiten
df <-  df %>% 
  mutate(canton_shapefile = case_when(canton == "Wallis" ~ "Valais",
                                      canton == "Tessin" ~ "Ticino",
                                      canton == "Biel" ~ "Bern",
                                      canton == "Freiburg" ~ "Fribourg",
                                      canton == "Neuenburg" ~ "Neuchâtel",
                                      canton == "Sankt Gallen" ~ "St. Gallen",
                                      canton == "Waadt" ~ "Vaud",
                                      TRUE ~ canton)) %>% 
  add_count(canton_shapefile,
            name = "n_femizid") %>% 
  select(rowid, 
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
  group_by(canton_shapefile) %>%
  mutate(info_tooltipp = str_c(info_tooltipp, 
                               collapse = "<br>")) %>% 
  ungroup() %>% 
  mutate(info_tooltipp = paste0("<b>", canton_shapefile, "</b>", "<br>",
                                "Anzahl (versuchte) Feminzide: ", n_femizid, "<br><br>",
                                info_tooltipp)) %>% 
  mutate(info_tooltipp = str_remove_all(info_tooltipp,
                                        "NA"))

# Fehlende Kantone hinzufügen
shape_agg %>% 
  anti_join(df,
            by = c("NAME" = "canton_shapefile"))
df <- df %>% 
  add_row(canton_shapefile = "Appenzell Ausserrhoden",
          info_tooltipp = "<b>Appenzell Ausserrhoden</b><br>Keine Informationen verfügbar") %>%
  add_row(canton_shapefile = "Appenzell Innerrhoden",
          info_tooltipp = "<b>Appenzell Innerrohden</b><br>Keine Informationen verfügbar") %>% 
  add_row(canton_shapefile = "Basel-Landschaft",
          info_tooltipp = "<b>Basel-Landschaft</b><br>Keine Informationen verfügbar") %>% 
  add_row(canton_shapefile = "Basel-Stadt",
          info_tooltipp = "<b>Basel-Stadt</b><br>Keine Informationen verfügbar") %>% 
  add_row(canton_shapefile = "Genève",
          info_tooltipp = "<b>Genève</b><br>Keine Informationen verfügbar") %>% 
  add_row(canton_shapefile = "Glarus",
          info_tooltipp = "<b>Glarus</b><br>Keine Informationen verfügbar") %>% 
  add_row(canton_shapefile = "Graubünden",
          info_tooltipp = "<b>Graubünden</b><br>Keine Informationen verfügbar") %>% 
  add_row(canton_shapefile = "Jura",
          info_tooltipp = "<b>Jura</b><br>Keine Informationen verfügbar") %>% 
  add_row(canton_shapefile = "Nidwalden",
          info_tooltipp = "<b>Nidwalden</b><br>Keine Informationen verfügbar") %>% 
  add_row(canton_shapefile = "Obwalden",
          info_tooltipp = "<b>Obwalden</b><br>Keine Informationen verfügbar") %>% 
  add_row(canton_shapefile = "Schwyz",
          info_tooltipp = "<b>Schwyz</b><br>Keine Informationen verfügbar") %>% 
  add_row(canton_shapefile = "Uri",
          info_tooltipp = "<b>Uri</b><br>Keine Informationen verfügbar")



# Karte -------------------------------------------------------------------
# fehlende Kantone hinzufügen
p1 <- shape_agg %>% 
  left_join(df,
            by = c("NAME" = "canton_shapefile")) %>% 
  distinct() %>% 
  ggplot(aes(tooltip = info_tooltipp, 
             data_id = rowid)) +
  geom_sf_interactive(aes(fill = n_femizid),
                      color = "grey80",
                      hover_nearest = TRUE,
                      show.legend = FALSE) + 
  scale_fill_gradient2(low = "#B995FF",
                       mid = "#6A22B8",
                       high =  "#2C0D4E",
                       midpoint = 3,
                       na.value = "#DBC4FF") + 
  labs(title = "(Versuchte) Femizide in der Schweiz - 2025",
       subtitle = "Femizide: 22 | versuchte Femizide: 8",
       caption = "Quelle: https://www.stopfemizid.ch/",
       fill = "Anzahl") +
  theme_void()


p1_interactive <- girafe(ggobj = p1,
                         options = list(
                           opts_hover(css = ""), 
                           # opts_hover_inv(css = "fill: gray"),
                           opts_toolbar(saveaspng = FALSE),
                           opts_sizing(rescale = TRUE, width = 0.75)
                         ))
htmlwidgets::saveWidget(p1_interactive, 
                        "Femizide_Schweiz_2025.html",
                        selfcontained = TRUE)
