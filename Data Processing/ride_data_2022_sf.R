library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)

stations_sf <- readRDS("./Data/stations_update_2022_sf.rds")
data_all_years <- read_csv("./Data/data_all_years.csv")

rides_2022_cleaned <- data_all_years %>%
  filter(Start.Station.Id %in% stations_sf$station_id & End.Station.Id %in% stations_sf$station_id & year(Start.Time) == 2022) 

rides_2022_sf <- left_join(rides_2022_cleaned,
                           select(stations_sf, c(station_id, geometry)),
                           by = join_by(Start.Station.Id == station_id),
                           relationship = "many-to-one")

rides_2022_sf <- rides_2022_sf %>% rename(start_geometry = geometry)

rides_2022_sf <- left_join(rides_2022_sf,
                           select(stations_sf, c(station_id, geometry)),
                           by = join_by(End.Station.Id == station_id),
                           relationship = "many-to-one")

rides_2022_sf <- rides_2022_sf %>% rename(end_geometry = geometry)

saveRDS(rides_2022_sf, "./Data/rides_2022_sf.rds")

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