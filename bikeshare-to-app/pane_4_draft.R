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

monthly_rides_station <- rides_2022_dset %>%
  filter(month(Start.Time) == 1) %>%
  group_by(User.Type) %>%
  count(trip_date = date(Start.Time), Start.Station.Id) %>%
  filter(Start.Station.Id == 7260) %>%
  collect() %>%
  union_all(rides_2022_dset %>%
              filter(month(Start.Time) == 1) %>%
              count(trip_date = date(Start.Time), Start.Station.Id) %>%
              collect() %>%
              mutate(User.Type = "Total", .before = 1) %>%
              filter(Start.Station.Id == 7260)
              )

p <- ggplot(monthly_rides_station, aes(x = trip_date,
                                       y = n,
                                       group = User.Type,
                                       colour = User.Type
                                       )) +
  geom_line_interactive(aes(data_id = User.Type)) +
  geom_point_interactive(aes(tooltip = glue("{as.character(wday(trip_date, label = TRUE))}, {as.character(month(trip_date, label = TRUE))}, {day(trip_date)}<br/>",
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
girafe(ggobj = p,
      options = list(
        opts_tooltip(use_fill = TRUE,
                     use_stroke = TRUE),
        opts_hover(css = "stroke-width:2;"),
        opts_hover_inv(css = "opacity:0.3;")
      ))
