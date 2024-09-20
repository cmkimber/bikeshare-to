library(tidyverse)
library(sf)
library(arrow)

stations_sf <- readRDS("./Data/stations_update_2022_sf.rds")
data_all_years <- read_rds("./Data/data_all_years.rds")

rides_2022_cleaned <- data_all_years %>%
  filter(Start.Station.Id %in% stations_sf$station_id & End.Station.Id %in% stations_sf$station_id & year(Start.Time) == 2022) %>%
  mutate(across(c(Start.Station.Id, End.Station.Id), as.numeric))

saveRDS(rides_2022_cleaned, "./Data/rides_2022_cleaned.rds")
write_dataset(rides_2022_cleaned, path = "./Data/rides_2022_cleaned")


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

