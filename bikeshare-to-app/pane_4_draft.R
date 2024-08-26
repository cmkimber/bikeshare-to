library(tidyverse)
library(arrow)
library(sf)
library(shiny)
library(bslib)
library(leaflet)
library(wesanderson)
library(ggiraph)
library(RColorBrewer)
library(glue)
library(plotly)

rides_path <- file.path("./Data/rides_2022_cleaned")
rides_2022_dset <- open_dataset(rides_path)
stations_update_2022_sf <- readRDS("./Data/stations_update_2022_sf.rds")

# define month abbreviations for monthly data select input
month_values <- seq(1,12,1)
names(month_values) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


# Custom theme used for plotting in this project
theme_bikeshare <- function(){
  font <- "Helvetica"
  
  theme_linedraw() %+replace%
    
    theme(
      
      # grid elements
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.ticks.x = element_blank(),
      
      # legend elements
      legend.position = "bottom",
      
      # text elements
      plot.title = element_text(
        family = font,
        size = 20,
        face = "bold",
        hjust = 0,
        vjust = 2),
      
      plot.subtitle = element_text(
        family = font,
        size = 14),
      
      axis.title = element_text(
        family = font,
        size = 14),
      
      axis.title.x = element_text(
        margin = margin(t = 5)),
      
      axis.title.y = element_text(
        margin = margin(r = 10),
        angle = 90),
      
      axis.text = element_text(
        family = font,
        size = 12),
      
      axis.text.x = element_text(
        margin = margin(t = 5))
    )
}

zis_colours <- wes_palette("Zissou1", type = "discrete")

# YEARLY RIDES ----
yearly_rides_station <- rides_2022_dset %>%
  filter(Start.Station.Id == 7746) %>%
  mutate(trip_month = as.Date(floor_date(Start.Time, unit = "month"))) %>%
  group_by(trip_month, User.Type) %>%
  count() %>%
  collect() %>%
  group_by(User.Type) %>%
  complete(trip_month = seq.Date(from = as.Date("2022-01-01"),
                                 to = as.Date("2022-12-31"),
                                 by = "month")) %>%
  union_all(rides_2022_dset %>%
              filter(Start.Station.Id == 7746) %>%
              mutate(trip_month = as.Date(floor_date(Start.Time, unit = "month"))) %>%
              count(trip_month) %>%
              collect() %>%
              complete(trip_month = seq.Date(from = as.Date("2022-01-01"),
                                             to = as.Date("2022-12-31"),
                                             by = "month")) %>%
              mutate(User.Type = "Total", .before = 1)
            ) %>%
  replace_na(list(n = 0))

p_y <- ggplot(yearly_rides_station, aes(x = trip_month,
                                         y = n,
                                         group = User.Type,
                                         fill = User.Type)) +
  geom_bar_interactive(aes(data_id = trip_month,
                           tooltip = glue("{month(trip_month, label = TRUE)}<br/>",
                                          "{User.Type}<br/>",
                                          "{n} Trips<br/>",
                           )),
                       position = "dodge",
                       stat = "identity") +
  labs(x = "Month",
       y = "Number of Trips",
       fill = "User Type") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  scale_fill_manual(values = c(zis_colours[1], zis_colours[5], zis_colours[3])) +
  theme_bikeshare() +
  theme(axis.ticks.x = element_line())

girafe(ggobj = p_y,
       options = list(opts_tooltip(use_fill = TRUE,
                                   use_stroke = TRUE),
                      opts_hover(css = "stroke:black;stroke-width:2px")),)

# MONTHLY RIDES ----
monthly_rides_station <- rides_2022_dset %>%
  filter(month(Start.Time) == 1 & Start.Station.Id == 7260) %>%
  mutate(trip_date = date(Start.Time)) %>%
  group_by(trip_date, User.Type) %>%
  count() %>%
  collect() %>%
  group_by(User.Type) %>%
  complete(trip_date = seq.Date(from = as.Date(paste(2022, 1, 01, sep = "-")),
                                to = as.Date(paste(2022, 1, 01, sep = "-")) + months(1) - days(1),
                                by = "day")) %>%
  union_all(rides_2022_dset %>%
              filter(month(Start.Time) == 1 & Start.Station.Id == 7260) %>%
              mutate(trip_date = date(Start.Time)) %>%
              count(trip_date) %>%
              collect() %>%
              complete(trip_date = seq.Date(from = as.Date(paste(2022, 1, 01, sep = "-")),
                                            to = as.Date(paste(2022, 1, 01, sep = "-")) + months(1) - days(1),
                                            by = "day")) %>%
              mutate(User.Type = "Total", .before = 1)
              ) %>%
  replace_na(list(n = 0))

p_m <- ggplot(monthly_rides_station, aes(x = trip_date,
                                       y = n,
                                       group = User.Type,
                                       colour = User.Type
                                       )) +
  geom_line_interactive(aes(data_id = User.Type)) +
  geom_point_interactive(aes(tooltip = glue("{strftime(trip_date, format = '%a, %b %d')}<br/>",
                                            "{User.Type}<br/>",
                                            "{n} Trips<br/>"
                                            ),
                             data_id = trip_date)) +
  labs(x = "Date",
       y = "Number of Trips",
       colour = "User Type") + 
  scale_x_date(date_breaks = "7 days",
               date_minor_breaks = "1 day",
               date_labels = "%b %d"
               # NOTE minor ticks only work from ggplot2 3.5.0
               # guide = guide_axis(minor.ticks = TRUE)
                     ) +
  scale_colour_manual(values = c(zis_colours[1], zis_colours[5], zis_colours[3])) +
  theme_bikeshare() + 
  theme(axis.ticks.x = element_line())

# ggplotly(p) 
girafe(ggobj = p_m,
      options = list(
        opts_tooltip(use_fill = TRUE,
                     use_stroke = TRUE),
        opts_hover(css = "stroke-width:2;"),
        opts_hover_inv(css = "opacity:0.3;")
      ))

# DAILY RIDES ----

# Note that it is important to expand the table using complete() to obtain all possible combinations of hour and User Type to properly render hours with 0 trips
daily_rides_station <- rides_2022_dset %>%
  filter(date(Start.Time) == "2022-07-01" & Start.Station.Id == 7260) %>%
  mutate(trip_hour = floor_date(Start.Time, unit = "hour")) %>%
  group_by(User.Type) %>%
  count(trip_hour) %>%
  collect() %>%
  group_by(User.Type) %>%
  complete(trip_hour = seq(from = as.POSIXct("2022-07-01"),
                           to = as.POSIXct("2022-07-01") + hours(24) - seconds(1),
                           by = "hour"),
  ) %>%
  union_all(rides_2022_dset %>%
              filter(date(Start.Time) == "2022-07-01" & Start.Station.Id == 7260) %>%
              mutate(trip_hour = floor_date(Start.Time, unit = "hour")) %>%
              count(trip_hour) %>%
              collect() %>%
              complete(trip_hour = seq(from = as.POSIXct("2022-07-01"),
                                       to = as.POSIXct("2022-07-01") + hours(24) - seconds(1),
                                       by = "hour")) %>%
              mutate(User.Type = "Total", .before = 1)
            ) %>%
  replace_na(list(n = 0, Start.Station.Id = 7260))

p_d <- ggplot(daily_rides_station, aes(x = trip_hour,
                                       y = n,
                                       group = User.Type,
                                       colour = User.Type)) +
  geom_line_interactive(aes(data_id = User.Type)) +
  geom_point_interactive(aes(tooltip = glue("{strftime(trip_hour, format = '%H:%M')}<br/>",
                                            "{User.Type}<br/>",
                                            "{n} Trips"),
                             data_id = trip_hour)) +
  labs(x = "Time",
       y = "Number of Trips",
       colour = "User Type") + 
  scale_x_datetime(date_breaks = "4 hours",
                   date_labels = "%H:%M") +
  scale_colour_manual(values = c(zis_colours[1], zis_colours[5], zis_colours[3])) +
  theme_bikeshare() +
  theme(axis.ticks.x = element_line())

girafe(ggobj = p_d,
       options = list(
         opts_tooltip(use_fill = TRUE,
                      use_stroke = TRUE),
         opts_hover(css = "stroke-width:2;"),
         opts_hover_inv(css = "opacity:0.3;")
       ))
