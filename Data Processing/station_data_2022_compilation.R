library(tidyverse)
library(sf)
library(leaflet)

# load in the known station information and station locations inferred through various methods in the "Station_ID_Identification" Jupyter notebook
stations_update_2022 <- readRDS("./Data/stations_update_2022.rds")
stations_fuzzjoin_found <- readRDS("./Data/stations_fuzzjoin_found.rds")
stations_levitate_correct <- readRDS("./Data/stations_levitate_correct.rds")
stations_levitate_incorrect <- readRDS("./Data/stations_levitate_incorrect.rds")

# convert the known station locations to a spatial features object
stations_sf <- st_as_sf(x = stations_update_2022,
                        coords = c("lon", "lat"),
                        crs = st_crs(4326))

# patterns for parsing the sf geometries that had been converted to chr back to coordinates
coordinates <- "(?<lon>[-][0-9]+[\\.][0-9]+),\\s(?<lat>[0-9]+[\\.][0-9]+)"


fuzzjoin_lookup <- c(station_id = "Start.Station.Id", name = "Start.Station.Name")

# reformat the station locations identified using the fuzzyjoin package to appropriately formatted sf object and add to station information table
stations_fuzzjoin_found <- stations_fuzzjoin_found %>%
  select(c(Start.Station.Id, Start.Station.Name, geometry)) %>%
  rename(all_of(fuzzjoin_lookup)) %>%
  rowwise() %>%
  mutate(lon = as.numeric(str_match_all(geometry, coordinates)[[1]][1,2]),
         lat = as.numeric(str_match_all(geometry, coordinates)[[1]][1,3])) %>%
  select(-geometry)

stations_fuzzjoin_found_sf <- st_as_sf(stations_fuzzjoin_found, coords = c("lon", "lat"), crs = st_crs(4326))
stations_sf <- bind_rows(stations_sf, stations_fuzzjoin_found_sf)

# reformat the station locations identified using the levitate package to appropriately formatted sf object and add to the station information table
stations_levitate_correct <- stations_levitate_correct %>%
  select(c(Start.Station.Id, Start.Station.Name, geometry)) %>%
  rename(all_of(fuzzjoin_lookup)) %>%
  rowwise() %>%
  mutate(lon = as.numeric(str_match_all(geometry, coordinates)[[1]][1,2]),
         lat = as.numeric(str_match_all(geometry, coordinates)[[1]][1,3])) %>%
  select(-geometry)

stations_levitate_correct_sf <- st_as_sf(stations_levitate_correct, coords = c("lon", "lat"), crs = st_crs(4326))
stations_sf <- bind_rows(stations_sf, stations_levitate_correct_sf)

# a basic map of the station locations for sanity checking
map <- leaflet() %>%
  addTiles() %>%
  addMarkers(data = stations_sf,
             popup = ~name)
map
