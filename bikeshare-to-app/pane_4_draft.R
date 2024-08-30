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
# library(reactlog)

# reactlog_enable()

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
pal_pane_4 <- c("Annual Member" = zis_colours[1],
                "Casual Member" = zis_colours[5],
                "Total" = zis_colours[3])

station_choices <- stations_update_2022_sf$station_id
for (i in (1:length(station_choices)))
  names(station_choices)[i] <- paste0(stations_update_2022_sf$name[i],
                                      " (#",
                                      stations_update_2022_sf$station_id[i],
                                      ")")

ui <- page_fluid(
  selectInput("select_station_4",
              "Select Station:",
              choices = station_choices,
              selected = 7000),
  girafeOutput("plot_yearly_stationwise"),
  girafeOutput("plot_monthly_stationwise"),
  girafeOutput("plot_daily_stationwise")
)

server <- function(input, output, session){
  
  session$onFlushed(function(){
    session$sendCustomMessage(type = 'plot_yearly_stationwise_set', message = "2022-01-01")
  })
  
  yearly_rides_stationwise <- reactive({
    rides_2022_dset %>%
      filter(Start.Station.Id == as.numeric(!!input$select_station_4)) %>%
      mutate(trip_month = as.Date(floor_date(Start.Time, unit = "month"))) %>%
      group_by(trip_month, User.Type) %>%
      count() %>%
      collect() %>%
      group_by(User.Type) %>%
      complete(trip_month = seq.Date(from = as.Date("2022-01-01"),
                                     to = as.Date("2022-12-31"),
                                     by = "month")) %>%
      union_all(rides_2022_dset %>%
                  filter(Start.Station.Id == as.numeric(!!input$select_station_4)) %>%
                  mutate(trip_month = as.Date(floor_date(Start.Time, unit = "month"))) %>%
                  count(trip_month) %>%
                  collect() %>%
                  complete(trip_month = seq.Date(from = as.Date("2022-01-01"),
                                                 to = as.Date("2022-12-31"),
                                                 by = "month")) %>%
                  mutate(User.Type = "Total", .before = 1)) %>%
      replace_na(list(n = 0))
  })
  
  output$plot_yearly_stationwise <- renderGirafe({
    p_y <- ggplot(yearly_rides_stationwise(), aes(x = trip_month,
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
      scale_fill_manual(values = pal_pane_4,
                        limits = names(pal_pane_4)) +
      theme_bikeshare() +
      theme(axis.ticks.x = element_line())
    
    girafe(ggobj = p_y,
           options = list(opts_tooltip(use_fill = TRUE,
                                       use_stroke = TRUE),
                          opts_hover(css = "stroke:white;stroke-width:2px"),
                          opts_hover_inv(css = "opacity:0.3;"),
                          opts_selection(selected = input$plot_yearly_stationwise_selected,
                                         type = "single",
                                         css = "stroke:black;stroke-width:2px")
           ))
  })

  monthly_rides_stationwise <- reactive({
    req(!(is.null(input$plot_yearly_stationwise_selected)))
    rides_2022_dset %>%
      filter(month(Start.Time) == month(as.Date(!!input$plot_yearly_stationwise_selected)) & Start.Station.Id == as.numeric(!!input$select_station_4)) %>%
      mutate(trip_date = date(Start.Time)) %>%
      group_by(trip_date, User.Type) %>%
      count() %>%
      collect() %>%
      group_by(User.Type) %>%
      complete(trip_date = seq.Date(from = as.Date(!!input$plot_yearly_stationwise_selected),
                                    to = as.Date(!!input$plot_yearly_stationwise_selected) + months(1) - days(1),
                                    by = "day")) %>%
      union_all(rides_2022_dset %>%
                  filter(month(Start.Time) == month(as.Date(!!input$plot_yearly_stationwise_selected)) & Start.Station.Id == as.numeric(!!input$select_station_4)) %>%
                  mutate(trip_date = date(Start.Time)) %>%
                  count(trip_date) %>%
                  collect() %>%
                  complete(trip_date = seq.Date(from = as.Date(!!input$plot_yearly_stationwise_selected),
                                                to = as.Date(!!input$plot_yearly_stationwise_selected) + months(1) - days(1),
                                                by = "day")) %>%
                  mutate(User.Type = "Total", .before = 1)
      ) %>%
      replace_na(list(n = 0))
  })
  
  output$plot_monthly_stationwise <- renderGirafe({
    req(monthly_rides_stationwise())
    p_m <- ggplot(monthly_rides_stationwise(), aes(x = trip_date,
                                                   y = n,
                                                   group = User.Type,
                                                   colour = User.Type
    )) +
      geom_line_interactive() +
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
      scale_y_continuous(limits = c(0, NA)) +
      scale_colour_manual(values = pal_pane_4,
                          limits = names(pal_pane_4)) +
      theme_bikeshare() + 
      theme(axis.ticks.x = element_line())
    
    # ggplotly(p) 
    girafe(ggobj = p_m,
           options = list(
             opts_tooltip(use_fill = TRUE,
                          use_stroke = TRUE),
             opts_hover(css = "stroke-width:2;"),
             opts_hover_inv(css = "opacity:0.3;"),
             opts_selection(selected = as.Date(input$plot_yearly_stationwise_selected),
                            type = "single",
                            css = "stroke:black;stroke-width:2px")
           ))
  })
  
  daily_rides_stationwise <- reactive({
    req(!(is.null(input$plot_monthly_stationwise_selected)))
    rides_2022_dset %>%
      filter(date(Start.Time) == as.Date(!!input$plot_monthly_stationwise_selected) & Start.Station.Id == as.numeric(!!input$select_station_4)) %>%
      mutate(trip_hour = floor_date(Start.Time, unit = "hour")) %>%
      group_by(User.Type) %>%
      count(trip_hour) %>%
      collect() %>%
      group_by(User.Type) %>%
      complete(trip_hour = seq(from = as.POSIXct(!!input$plot_monthly_stationwise_selected, tz = "GMT"),
                               to = as.POSIXct(!!input$plot_monthly_stationwise_selected, tz = "GMT") + hours(24) - seconds(1),
                               by = "hour"),
      ) %>%
      union_all(rides_2022_dset %>%
                  filter(date(Start.Time) == as.Date(!!input$plot_monthly_stationwise_selected) & Start.Station.Id == as.numeric(!!input$select_station_4)) %>%
                  mutate(trip_hour = floor_date(Start.Time, unit = "hour")) %>%
                  count(trip_hour) %>%
                  collect() %>%
                  complete(trip_hour = seq(from = as.POSIXct(!!input$plot_monthly_stationwise_selected, tz = "GMT"),
                                           to = as.POSIXct(!!input$plot_monthly_stationwise_selected, tz = "GMT") + hours(24) - seconds(1),
                                           by = "hour")) %>%
                  mutate(User.Type = "Total", .before = 1)
      ) %>%
      replace_na(list(n = 0))
  })
  
  output$plot_daily_stationwise <- renderGirafe({
    req(daily_rides_stationwise())
    p_d <- ggplot(daily_rides_stationwise(), aes(x = trip_hour,
                                           y = n,
                                           group = User.Type,
                                           colour = User.Type)) +
      geom_line_interactive() +
      geom_point_interactive(aes(tooltip = glue("{strftime(trip_hour, format = '%H:%M', tz = 'GMT')}<br/>",
                                                "{User.Type}<br/>",
                                                "{n} Trips"),
                                 data_id = trip_hour)) +
      labs(x = "Time",
           y = "Number of Trips",
           colour = "User Type") + 
      scale_x_datetime(date_breaks = "4 hours",
                       date_labels = "%H:%M") +
      scale_y_continuous(limits = c(0, NA)) +
      scale_colour_manual(values = pal_pane_4,
                          limits = names(pal_pane_4)) +
      theme_bikeshare() +
      theme(axis.ticks.x = element_line())
    
    girafe(ggobj = p_d,
           options = list(
             opts_tooltip(use_fill = TRUE,
                          use_stroke = TRUE),
             opts_hover(css = "stroke-width:2;"),
             opts_hover_inv(css = "opacity:0.3;")
           ))
  })
  
}

shinyApp(ui = ui, server = server)
