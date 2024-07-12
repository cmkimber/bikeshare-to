library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)

rides_2022_sf <- readRDS("./Data/rides_2022_sf.rds")

# Some sanity-checking maps
map <- leaflet() %>%
  addTiles() %>%
  addHeatmap(data = rides_2022_sf$start_geometry,
             radius = 8)
map

map2 <- leaflet() %>%
  addTiles() %>%
  addHeatmap(data = rides_2022_sf$end_geometry,
             radius = 8)
map2

# Testing adding a linestring for each ride to plot the 'as-the-crow-flies" path of each ride
st_line_test <- st_union(rides_2022_sf$start_geometry[1],
                         rides_2022_sf$end_geometry[1]) %>%
  st_cast("LINESTRING")

st_arrow_test <- as_Spatial(st_line_test)

leaflet() %>% addTiles() %>% addArrowhead(data = st_arrow_test, color = "blue", options = arrowheadOptions(frequency = "endonly"))

leaflet() %>% addTiles() %>% addPolylines(data = st_line_test, color = "blue") %>% addArrowhead(data = st_line_test, color = "blue")

st_arrow_df <- rides_2022_sf %>% filter(date(Start.Time) == "2022-01-01") %>% mutate(route = st_union(start_geometry, end_geometry) %>% st_cast("LINESTRING"))

leaflet() %>% addTiles() %>% addArrowhead(data = st_arrow_df$route[hour(st_arrow_df$Start.Time) == "11"],
                                          options = arrowheadOptions(size = "200m"))
