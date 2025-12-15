# Pakete laden ------------------------------------------------------------
library(tidyverse)
library(ggiraph)
library(sf)

`%ni%` <- negate(`%in%`)

# Target-Webseite definieren
target_webpage <- read_html("https://www.stopfemizid.ch/deutsch")

### Daten laden
# 2025
df_2025 <- target_webpage %>% 
  html_elements(css = "#block-yui_3_17_2_1_1738144082356_131227 > div:nth-child(1) > div:nth-child(1)") %>% 
  html_text()
df_2025 <- df_2025 %>% 
  str_replace_all("St. Gallen", "Sankt Gallen")

# Rest
df_rest <- target_webpage %>% 
  html_elements(css = "#block-6799fc1dcc7d181b7fa3859c > div:nth-child(1) > div:nth-child(1)") %>% 
  html_text()



# Daten aufbereiten -------------------------------------------------------
### Generell
df_2025 <- df_2025 %>% 
  str_split("Jahre alt\\.|nicht bekannt\\.|3 und 10\\.") %>% 
  unlist() %>% 
  as_tibble() %>% 
  mutate(value = str_squish(value),
         value = str_remove_all(value, "\n")) %>% 
  rowid_to_column()

### Femizide vs. versuchte Feminizide
cut_off <- df_2025 %>% 
  filter(str_detect(value, "Versuchte Femizide")) %>% 
  pull(rowid)
df_2025 <- df_2025 %>% 
  mutate(category = if_else(rowid >= cut_off, 
                            "Versuchter Femizid",
                            "Femizid")) %>% 
  # leere Zeile entfernen
  filter(rowid != max(rowid)) %>% 
  # unnötige Infos entfernen %>% 
  mutate(value = str_remove_all(value, "Femizide im Jahr 2025"),
         value = str_remove_all(value, "Versuchte Femizide"))


### Infos extrahieren
df_2025 <- df_2025 %>% 
  separate(value, 
           into = c("date", "community", "other", "other2"),
           sep = ",") %>% 
  mutate(other = paste0(other, other2),
         other = str_remove_all(other, "NA")) %>%
  select(-other2) %>% 
  separate(other, 
           into = c("canton", "info", "info2"),
           sep = "\\.") %>% 
  mutate(info = paste0(info, info2),
         info = str_remove_all(info, "NA")) %>% 
  select(-info2)


### Spezialfall
df_2025 <- df_2025 %>% 
  # 25: Nahe Solothurn. Die Frau überlebt. Sie ist 40
  mutate(community = if_else(rowid == 25, "Solothurn", community),
         canton = if_else(rowid == 25, "Solothurn", canton),
         info = if_else(rowid == 25, "Nähe Solothurn. Die Frau überlebt. Sie ist 40 Jahre alt.", info)) %>% 
  mutate(info = if_else(rowid == 26, paste0(info, " 3 und 10."), info)) %>% 
  mutate(info = if_else(rowid %ni% c(25, 26), paste0(info, " Jahre alt."), info)) %>% 
  mutate(across(where(is.character), ~str_squish(.))) %>% 
  mutate(date = dmy(date)) %>% 
  mutate(canton_shapefile = case_when(canton == "Wallis" ~ "Valais",
                                      canton == "Tessin" ~ "Ticino",
                                      canton == "Biel" ~ "Bern",
                                      canton == "Freiburg" ~ "Fribourg",
                                      canton == "Neuenburg" ~ "Neuchâtel",
                                      canton == "Sankt Gallen" ~ "St. Gallen",
                                      canton == "Waadt" ~ "Vaud",
                                      TRUE ~ canton)) %>% 
  add_count(canton_shapefile,
            name = "n_femizid")




# Karte -------------------------------------------------------------------
setwd("C:/Users/LucaK/OneDrive/Desktop/")
shape_file <- sf::read_sf(here::here("Shapefile_Switzerland/swissBOUNDARIES3D_1_4_TLM_KANTONSGEBIET.shp"))
mapping_data <- shape_file %>% 
  left_join(df_2025,
            by = c("NAME" = "canton_shapefile"),
            relationship = "many-to-many") %>% 
  distinct()

myplot <- mapping_data %>% 
  ggplot(aes(tooltip = info, data_id = info)) +
  geom_sf_interactive(aes(fill = n_femizid),
                      color = "grey50",
                      hover_nearest = TRUE) + 
  scale_fill_gradient2(low = "darkblue",
                       mid = "orange",
                       high =  "red",
                       na.value = "darkblue")

interactive_plot <- girafe(ggobj = myplot)
htmltools::save_html(interactive_plot, "ggiraph-2.html")
