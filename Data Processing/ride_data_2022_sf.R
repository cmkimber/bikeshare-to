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

