library(tidyverse)
library(sf)

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

top_startvend_long <- top_startvend_annual %>%
  filter(start.rank <= 15 | end.rank <= 15) %>%
  pivot_longer(cols = !(station_id:geometry),
               names_to = "terminus",
               values_to = "rank") %>%
  mutate(terminus = as.factor(terminus)) %>%
  mutate(terminus = fct_relevel(terminus, "start.rank", "end.rank"))

library(ggiraph)
library(glue)
p <- ggplot(top_startvend_long, aes(x = terminus, y = rank, group = station_id)) +
  geom_line_interactive(aes(tooltip = glue("Station ID#: {station_id}<br/>",
                                           "Station Name: {name}<br/>"),
                            data_id = station_id), size = 1.2,
                        alpha = 0.6) +
  geom_point_interactive(aes(tooltip = glue("Station ID#: {station_id}<br/>",
                                            "Station Name: {name}<br/>",
                                            "Rank: {rank}"),
                             data_id = station_id)) + 
  labs(x = "",
       y = "Rank (# of Trips)") +
  scale_x_discrete(expand = c(0.01,0.01),
                   labels = c("Start Station", "End Station")) +
  scale_y_reverse(limits = c(max(top_startvend_long$rank), 1),
                  expand = c(0,1),
                  breaks = c(1,seq(5,40, 5)),
                  sec.axis = sec_axis(trans = ~.,
                                      breaks = c(1,seq(5,40, 5)))) +
  theme_minimal() +
  theme(plot.margin = margin(5,15,5,5))

girafe(ggobj = p,
       options = list(
         opts_hover_inv(css = "stroke-width:1;opacity:0.4"),
         opts_hover(css = "stroke-width:4;opacity:1")
       ))

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