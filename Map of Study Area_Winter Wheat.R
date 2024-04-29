library(sf)
theme_set(theme_bw())
library(tidyverse)
library(giscoR)
library(ggthemes)
library(readxl)
library(ggplot2)
library(ggspatial)


# Loading the dataset containing the location details of the study area

data_area <-read_excel("Analysis_21-22.xlsx", sheet = "Sheet2", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric"))
View(data_area)

data_state <- read_excel("Analysis_21-22.xlsx", sheet = "Sheet12", col_types = c("text", "numeric", "numeric"))
View(data_state)


er_fedtstates <- gisco_get_nuts(
  nuts_level = 1,
  resolution = 10,
  country = "Germany",
  year = 2021
)

ger_fedstates_end <- ger_fedtstates %>%
  left_join(data_state, by = c("NUTS_NAME" = "Federal State"))

ggplot(ger_fedstates_end) +
  geom_sf(aes(fill = `Data (%)`)) +
  scale_fill_viridis_c(option = "D") +
  geom_sf_text(aes(label = NUTS_NAME), colour = "black") +
  coord_sf()+
  scale_x_continuous(labels = function(x) paste0(x, '\u00B0', "E")) +
  scale_y_continuous(labels = function(x) paste0(x, '\u00B0', "N")) +
  labs(title = "Winter Wheat") +
  geom_point(
    data = data_area,
    aes(x = Longitude, y = Latitude), color = "red", size = 2) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.15, "in"),
                         style = north_arrow_fancy_orienteering)
  
