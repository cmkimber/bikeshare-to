library(tidyverse)

rides_2022_sf <- readRDS("./Data/rides_2022_sf.rds")
stations_update_2022_sf <- readRDS("./Data/stations_update_2022_sf.rds")

top_start_stations_annual <- rides_2022_sf %>%
  count(Start.Station.Id) %>%
  arrange(desc(n)) %>%
  left_join(stations_update_2022_sf,
            by = join_by(Start.Station.Id == station_id)) %>%
  select(Start.Station.Id, n, name, geometry) %>%
  rename(station_id = Start.Station.Id) %>%
  mutate(start.rank = row_number())

top_end_stations_annual <- rides_2022_sf %>%
  count(End.Station.Id) %>%
  arrange(desc(n)) %>%
  left_join(stations_update_2022_sf,
            by = join_by(End.Station.Id == station_id)) %>%
  select(End.Station.Id, n, name, geometry) %>%
  rename(station_id = End.Station.Id) %>%
  mutate(end.rank = row_number())

top_startvend_annual <- top_start_stations_annual %>%
  select(-n) %>%
  left_join(top_end_stations_annual %>% select(-n))

top_start_stations_month <- rides_2022_sf %>%
  count(Start.Month = month(Start.Time), Start.Station.Id) %>%
  arrange(Start.Month, desc(n)) %>%
  left_join(stations_update_2022_sf,
            by = join_by(Start.Station.Id == station_id)) %>%
  select(Start.Station.Id, Start.Month, n, name, geometry) %>%
  rename(station_id = Start.Station.Id) %>%
  filter(Start.Month == 11) %>%
  mutate(start.rank = row_number()) 

top_end_stations_month <- rides_2022_sf %>%
  count(End.Month = month(End.Time), End.Station.Id) %>%
  arrange(End.Month, desc(n)) %>%
  left_join(stations_update_2022_sf,
            by = join_by(End.Station.Id == station_id)) %>%
  select(End.Station.Id, End.Month, n, name, geometry) %>%
  rename(station_id = End.Station.Id) %>%
  filter(End.Month == 11) %>%
  mutate(end.rank = row_number()) 
  
top_startvend_month <- top_start_stations_month %>%
  select(-c(n, Start.Month)) %>%
  left_join(top_end_stations_month %>% select(-c(n, End.Month)))