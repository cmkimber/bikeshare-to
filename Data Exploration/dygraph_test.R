library(tidyverse)
library(sf)
#library(shiny)
#library(leaflet)
#library(leaflet.extras)
#library(xts)
#library(dygraphs)
library(ggiraph)

rides_2022_sf <- readRDS("./Data/rides_2022_sf.rds")

rides_2022_dailysum <- rides_2022_sf %>%
  group_by(User.Type) %>%
  count(date = date(Start.Time)) %>%
  pivot_wider(names_from = User.Type,
              values_from = n,
              names_repair = "universal") %>%
  rowwise() %>%
  mutate(Total = sum(Annual.Member, Casual.Member))

test <- xts(x = rides_2022_dailysum[,-1], order.by = rides_2022_dailysum$date)

p <- dygraph(test)
p

p <- ggplot(rides_2022_dailysum, aes(date)) +
  geom_line(aes(y = Total, colour = "yellow")) +
  geom_line(aes(y = Casual.Member)) +
  geom_line(aes(y = Annual.Member))
p

rides_2022_daily_sum <- rides_2022_sf %>% group_by(User.Type) %>% count(date = date(Start.Time)) %>% bind_rows(rides_2022_sf %>% group_by(date = date(Start.Time)) %>% count() %>% mutate(User.Type = "Total"))

p <- ggplot(rides_2022_daily_sum, aes(x = date, y = n, colour = User.Type, group = User.Type)) +
  geom_line_interactive() +
  geom_point_interactive() + 
  scale_colour_manual(values = c("blue", "red", "yellow"))
#p
girafe(ggobj = p)

