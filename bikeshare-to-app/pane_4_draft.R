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
  layout_columns(
    card(titlePanel("Trips by Station")),
    card(actionButton("help_4",
                      "Help")),
    col_widths = c(6,-3,3)
  ),
  card(tags$head(
    tags$style(HTML("#select_station_4+ div>.selectize-dropdown {position: unset;}"))
  ),
    selectInput("select_station_4",
                   "Select Station:",
                   choices = station_choices,
                   selected = 7000)),
  layout_column_wrap(
    width = 1/2,
    card(card_header("Trips Started by Month"),
         girafeOutput("plot_yearly_swise_start")),
    card(card_header("Trips Ended by Month"), 
         girafeOutput("plot_yearly_swise_end")),
    card(card_header("Trips Started by Day"),
         girafeOutput("plot_monthly_swise_start")),
    card(card_header("Trips Ended by Day"),
         girafeOutput("plot_monthly_swise_end")),
    card(card_header("Trips Started by Hour"),
         girafeOutput("plot_daily_swise_start")),
    card(card_header("Trips Ended by Hour"),
         girafeOutput("plot_daily_swise_end"))
  )
)

server <- function(input, output, session){
  
  observeEvent(input$help_4, {
    showModal(modalDialog(
      title = "How this pane works",
      HTML("Placeholder")
    ))
  })
  
  # YEARLY DATA----
  
  selected_month <- reactiveVal(value = NULL)
  
  observeEvent(selected_month(), {
    session$sendCustomMessage(type = 'plot_yearly_swise_start_set',
                              message = selected_month())
    session$sendCustomMessage(type = 'plot_yearly_swise_end_set',
                              message = selected_month())
  })
  
  observeEvent(input$plot_yearly_swise_start_selected, {
    selected_month(input$plot_yearly_swise_start_selected)
  })
  observeEvent(input$plot_yearly_swise_end_selected, {
    selected_month(input$plot_yearly_swise_end_selected)
  })
  
  yearly_rides_swise_start <- reactive({
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
  
  output$plot_yearly_swise_start <- renderGirafe({
    p_y <- ggplot(yearly_rides_swise_start(), aes(x = trip_month,
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
                          opts_selection(selected = "2022-01-01",
                                         type = "single",
                                         css = "stroke:black;stroke-width:2px")
           ))
  })
  
  yearly_rides_swise_end<- reactive({
    rides_2022_dset %>%
      filter(End.Station.Id == as.numeric(!!input$select_station_4)) %>%
      mutate(trip_month = as.Date(floor_date(End.Time, unit = "month"))) %>%
      group_by(trip_month, User.Type) %>%
      count() %>%
      collect() %>%
      group_by(User.Type) %>%
      complete(trip_month = seq.Date(from = as.Date("2022-01-01"),
                                     to = as.Date("2022-12-31"),
                                     by = "month")) %>%
      union_all(rides_2022_dset %>%
                  filter(End.Station.Id == as.numeric(!!input$select_station_4)) %>%
                  mutate(trip_month = as.Date(floor_date(End.Time, unit = "month"))) %>%
                  count(trip_month) %>%
                  collect() %>%
                  complete(trip_month = seq.Date(from = as.Date("2022-01-01"),
                                                 to = as.Date("2022-12-31"),
                                                 by = "month")) %>%
                  mutate(User.Type = "Total", .before = 1)) %>%
      replace_na(list(n = 0))
  })
  
  output$plot_yearly_swise_end <- renderGirafe({
    p_y <- ggplot(yearly_rides_swise_end(), aes(x = trip_month,
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
                          opts_selection(selected = "2022-01-01",
                                         type = "single",
                                         css = "stroke:black;stroke-width:2px")
           ))
  })
  
  
  # MONTHLY DATA----

  selected_day <- reactiveVal(value = NULL)
  
  observeEvent(selected_month(),{
    temp_day <- selected_month()
    selected_day(temp_day)
  })
  
  observeEvent(selected_day(), {
    session$sendCustomMessage(type = "plot_monthly_swise_start_set", message = selected_day())
    session$sendCustomMessage(type = "plot_monthly_swise_end_set", message = selected_day())
  })
  
  observeEvent(input$plot_monthly_swise_start_selected, {
    selected_day(input$plot_monthly_swise_start_selected)
  })
  observeEvent(input$plot_monthly_swise_end_selected, {
    selected_day(input$plot_monthly_swise_end_selected)
  })
  
  
  monthly_rides_swise_start <- reactive({
    req(!(is.null(selected_month())))
    rides_2022_dset %>%
      filter(month(Start.Time) == month(as.Date(selected_month())) & Start.Station.Id == as.numeric(!!input$select_station_4)) %>%
      mutate(trip_date = date(Start.Time)) %>%
      group_by(trip_date, User.Type) %>%
      count() %>%
      collect() %>%
      group_by(User.Type) %>%
      complete(trip_date = seq.Date(from = as.Date(selected_month()),
                                    to = as.Date(selected_month()) + months(1) - days(1),
                                    by = "day")) %>%
      union_all(rides_2022_dset %>%
                  filter(month(Start.Time) == month(as.Date(selected_month())) & Start.Station.Id == as.numeric(!!input$select_station_4)) %>%
                  mutate(trip_date = date(Start.Time)) %>%
                  count(trip_date) %>%
                  collect() %>%
                  complete(trip_date = seq.Date(from = as.Date(selected_month()),
                                                to = as.Date(selected_month()) + months(1) - days(1),
                                                by = "day")) %>%
                  mutate(User.Type = "Total", .before = 1)
      ) %>%
      replace_na(list(n = 0))
  })
  
  output$plot_monthly_swise_start <- renderGirafe({
    req(monthly_rides_swise_start())
    jitterer <- position_jitter(width = 0.2, height = 0, seed = 123)
    p_m <- ggplot(monthly_rides_swise_start(), aes(x = trip_date,
                                                   y = n,
                                                   group = User.Type,
                                                   colour = User.Type
    )) +
      geom_line(position = jitterer) +
      geom_point_interactive(aes(tooltip = glue("{strftime(trip_date, format = '%a, %b %d')}<br/>",
                                                "{User.Type}<br/>",
                                                "{n} Trips<br/>"
      ),
      data_id = trip_date),
      position = jitterer) +
      labs(title = glue("{month(as.Date(selected_month()), label = TRUE)} {year(as.Date(selected_month()))}"),
           x = "Date",
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
             opts_selection(selected = as.Date(selected_month()),
                            type = "single",
                            css = "stroke:black;stroke-width:2px")
           ))
  })
  
  monthly_rides_swise_end <- reactive({
    req(!(is.null(selected_month())))
    rides_2022_dset %>%
      filter(month(End.Time) == month(as.Date(selected_month())) & End.Station.Id == as.numeric(!!input$select_station_4)) %>%
      mutate(trip_date = date(End.Time)) %>%
      group_by(trip_date, User.Type) %>%
      count() %>%
      collect() %>%
      group_by(User.Type) %>%
      complete(trip_date = seq.Date(from = as.Date(selected_month()),
                                    to = as.Date(selected_month()) + months(1) - days(1),
                                    by = "day")) %>%
      union_all(rides_2022_dset %>%
                  filter(month(End.Time) == month(as.Date(selected_month())) & End.Station.Id == as.numeric(!!input$select_station_4)) %>%
                  mutate(trip_date = date(End.Time)) %>%
                  count(trip_date) %>%
                  collect() %>%
                  complete(trip_date = seq.Date(from = as.Date(selected_month()),
                                                to = as.Date(selected_month()) + months(1) - days(1),
                                                by = "day")) %>%
                  mutate(User.Type = "Total", .before = 1)
      ) %>%
      replace_na(list(n = 0))
  })
  
  output$plot_monthly_swise_end <- renderGirafe({
    req(monthly_rides_swise_end())
    jitterer <- position_jitter(width = 0.2, height = 0, seed = 123)
    p_m <- ggplot(monthly_rides_swise_end(), aes(x = trip_date,
                                                 y = n,
                                                 group = User.Type,
                                                 colour = User.Type
    )) +
      geom_line(position = jitterer) +
      geom_point_interactive(aes(tooltip = glue("{strftime(trip_date, format = '%a, %b %d')}<br/>",
                                                "{User.Type}<br/>",
                                                "{n} Trips<br/>"
      ),
      data_id = trip_date),
      position = jitterer) +
      labs(title = glue("{month(as.Date(selected_month()), label = TRUE)} {year(as.Date(selected_month()))}"),
           x = "Date",
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
             opts_selection(selected = as.Date(selected_month()),
                            type = "single",
                            css = "stroke:black;stroke-width:2px")
           ))
  })
  
  # DAILY DATA----
  
  daily_rides_swise_start <- reactive({
    req(!(is.null(selected_day())))
    rides_2022_dset %>%
      filter(date(Start.Time) == as.Date(selected_day()) & Start.Station.Id == as.numeric(!!input$select_station_4)) %>%
      mutate(trip_hour = floor_date(Start.Time, unit = "hour")) %>%
      group_by(User.Type) %>%
      count(trip_hour) %>%
      collect() %>%
      group_by(User.Type) %>%
      complete(trip_hour = seq(from = as.POSIXct(selected_day(), tz = "GMT"),
                               to = as.POSIXct(selected_day(), tz = "GMT") + hours(24) - seconds(1),
                               by = "hour"),
      ) %>%
      union_all(rides_2022_dset %>%
                  filter(date(Start.Time) == as.Date(selected_day()) & Start.Station.Id == as.numeric(!!input$select_station_4)) %>%
                  mutate(trip_hour = floor_date(Start.Time, unit = "hour")) %>%
                  count(trip_hour) %>%
                  collect() %>%
                  complete(trip_hour = seq(from = as.POSIXct(selected_day(), tz = "GMT"),
                                           to = as.POSIXct(selected_day(), tz = "GMT") + hours(24) - seconds(1),
                                           by = "hour")) %>%
                  mutate(User.Type = "Total", .before = 1)
      ) %>%
      replace_na(list(n = 0))
  })
  
  output$plot_daily_swise_start <- renderGirafe({
    req(daily_rides_swise_start())
    p_d <- ggplot(daily_rides_swise_start(), aes(x = trip_hour,
                                           y = n,
                                           group = User.Type,
                                           colour = User.Type)) +
      geom_line_interactive() +
      geom_point_interactive(aes(tooltip = glue("{strftime(trip_hour, format = '%H:%M', tz = 'GMT')}<br/>",
                                                "{User.Type}<br/>",
                                                "{n} Trips"),
                                 data_id = trip_hour)) +
      labs(title = glue("{wday(as.Date(selected_day()), label = TRUE)}, {month(as.Date(selected_day()), label = TRUE)} {day(as.Date(selected_day()))}"),
           x = "Time",
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
  
  daily_rides_swise_end <- reactive({
  req(!(is.null(selected_day())))
  rides_2022_dset %>%
    filter(date(End.Time) == as.Date(selected_day()) & End.Station.Id == as.numeric(!!input$select_station_4)) %>%
    mutate(trip_hour = floor_date(End.Time, unit = "hour")) %>%
    group_by(User.Type) %>%
    count(trip_hour) %>%
    collect() %>%
    group_by(User.Type) %>%
    complete(trip_hour = seq(from = as.POSIXct(selected_day(), tz = "GMT"),
                             to = as.POSIXct(selected_day(), tz = "GMT") + hours(24) - seconds(1),
                             by = "hour"),
    ) %>%
    union_all(rides_2022_dset %>%
                filter(date(End.Time) == as.Date(selected_day()) & End.Station.Id == as.numeric(!!input$select_station_4)) %>%
                mutate(trip_hour = floor_date(End.Time, unit = "hour")) %>%
                count(trip_hour) %>%
                collect() %>%
                complete(trip_hour = seq(from = as.POSIXct(selected_day(), tz = "GMT"),
                                         to = as.POSIXct(selected_day(), tz = "GMT") + hours(24) - seconds(1),
                                         by = "hour")) %>%
                mutate(User.Type = "Total", .before = 1)
    ) %>%
    replace_na(list(n = 0))
})

output$plot_daily_swise_end <- renderGirafe({
  req(daily_rides_swise_end())
  p_d <- ggplot(daily_rides_swise_end(), aes(x = trip_hour,
                                               y = n,
                                               group = User.Type,
                                               colour = User.Type)) +
    geom_line_interactive() +
    geom_point_interactive(aes(tooltip = glue("{strftime(trip_hour, format = '%H:%M', tz = 'GMT')}<br/>",
                                              "{User.Type}<br/>",
                                              "{n} Trips"),
                               data_id = trip_hour)) +
    labs(title = glue("{wday(as.Date(selected_day()), label = TRUE)}, {month(as.Date(selected_day()), label = TRUE)} {day(as.Date(selected_day()))}"),
         x = "Time",
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
