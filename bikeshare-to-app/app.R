library(tidyverse)
library(arrow)
library(sf)
library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(wesanderson)
library(ggiraph)
library(DT)
library(RColorBrewer)
library(glue)

### LOAD APP-WIDE DATA AND FUNCTIONS ----

# rides_2022_sf <- readRDS("./Data/rides_2022_sf.rds")
# rides_2022_cleaned <- readRDS("./Data/rides_2022_cleaned.rds")
rides_path <- file.path("./Data/rides_2022_cleaned")
rides_2022_dset <- open_dataset(rides_path)
stations_update_2022_sf <- readRDS("./Data/stations_update_2022_sf.rds")

# define month abbreviations for monthly data select input
month_values <- seq(1,12,1)
names(month_values) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# define choices for station select input
station_choices <- stations_update_2022_sf$station_id
for (i in (1:length(station_choices)))
  names(station_choices)[i] <- paste0(stations_update_2022_sf$name[i],
                                      " (#",
                                      stations_update_2022_sf$station_id[i],
                                      ")")

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

### LOAD PANE 1 DATA ----

rides_2022_daily_sum <- rides_2022_dset %>%
  group_by(User.Type) %>%
  count(date = date(Start.Time)) %>%
  collect() %>%
  bind_rows(rides_2022_dset %>%
              group_by(date = date(Start.Time)) %>%
              count() %>%
              mutate(User.Type = "Total") %>%
              collect())

### LOAD PANE 2 DATA AND FUNCTIONS ----

pal_ride_num <- colorNumeric(palette = "Reds", domain = NULL)

get_station_rank_popup <- function(station_row){
  popup_text <- glue(
    "<b>Station ID#:</b> {station_row$station_id}<br/>",
    "<b>Station Name:</b> {station_row$name}<br/>",
    "<b>Number of Rides:</b> {station_row$n}<br/>",
    "<b>Rank:</b> {station_row$rank}"
  )
  return(popup_text)
}

### LOAD PANE 3 FUNCTIONS ----

# set scale range for the IO ratio to be symmetrical about 1 to make a diverging palette that is white at 1
io_scale_range <- c(0,2)

# function to saturate the IO ratio value colour at 2
pal_io <- function(x){
  return(if_else(x > 2, # if IO values are greater than the symmetrical scale
                 "#053061", #return blue value for 2, else use colorNumeric
                 suppressWarnings(colorNumeric(palette = "RdBu",
                                               domain = io_scale_range, 
                                               na.color="black")(x)),
                 missing = "#FF000000"
  )
  )
}

# manually set colours for the legend because the plotted palette saturates at 2 but automatic legend will not
io_legend_colors <- c(pal_io(0), pal_io(0.25), pal_io(0.5), pal_io(0.75), pal_io(1), pal_io(1.25), pal_io(1.5), pal_io(1.75), pal_io(2))
io_legend_labels <- c("0", "0.25", "0.5", "0.75", "1", "1.25", "1.5", "1.75", "2")

popup_folder <- tempdir()
io_plot_colors <- c(pal_io(0), pal_io(2))

get_io_popup <- function(station_row){
  io_plot <- station_row %>%
    pivot_longer(cols = c(Start.Count, End.Count), names_to = "Terminus", values_to = "n") %>%
    ggplot(aes(x = n, y = Terminus, fill = Terminus)) +
    geom_col() +
    labs(title = glue("Station ID: {station_row$station_id}"),
         subtitle = str_wrap(station_row$name, width = 40),
         x = "# of Trips",
         y = "Terminus") + 
    scale_y_discrete(labels = c("End", "Start")) +
    scale_fill_manual(values = alpha(io_plot_colors, 0.8)) + 
    theme_bikeshare() + 
    theme(legend.position = "none",
          plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 8))
  
  svg(filename = paste(popup_folder, "io_plot.svg", sep = "/"),
      width = 500*0.005, height = 300*0.005)
  print(io_plot)
  dev.off()
  
  popup_content <- paste(readLines(paste(popup_folder, "io_plot.svg", sep = "/")), collapse = "")
  
  return(popup_content)
}

#### LOAD PANE 4 FUNCTIONS ----

pal_pane_4 <- c("Annual Member" = zis_colours[1],
                "Casual Member" = zis_colours[5],
                "Total" = zis_colours[3])

#### BUILD UI ----

ui <- page_fluid(
  card(titlePanel("Bike Share Toronto 2022 Interactive Explorer")),
  navset_tab(
    nav_panel(title = "Daily Trips",
              layout_columns(
                card(titlePanel("Trip Numbers by Date")),
                card(actionButton("help_1",
                                  "Help")),
                col_widths = c(6,-3,3)
              ),
              card(card_header("Select Date:"),
                   card_body(class = "align-items-center",
                             sliderInput("date_slider",
                                         NULL,
                                         min = as_date("2022-01-01"),
                                         max = as_date("2022-12-31"),
                                         value = as_date("2022-01-01"),
                                         step = 1,
                                         animate = animationOptions(interval = 3000),
                                         width = "90%"))
              ),
              layout_column_wrap(
                width = "500px",
                card(card_header("Trip Start Density"),
                     leafletOutput("time_series_heatmap")),
                card(card_header("Daily Trip Count"),
                     card_body(plotOutput("daily_rides",
                                          width = "600px"),
                               class = "align-items-center")
                )
              )
              ),
    nav_panel(title = "Top Stations",
              layout_columns(
                card(titlePanel("Starting vs. Ending Station Use by Month")),
                card(actionButton("help_2",
                                  "Help")),
                col_widths = c(9,3)
              ),
              # accordion(
              #   open = FALSE,
              #   accordion_panel(
              #     "Click for Help",
              #     layout_columns(
              #       card(helpText("Explore the most popular stations to start and end Bike Share Toronto trips at during 2022 using this pane. The controls below can be used to select full year or monthly data, choose the month of interest, or search for a particular station. The tick box to display the top 15 stations will restrict the maps to showing the most popular stations; these 15 stations match those shown in the tables below, which can be expanded or hidden.")),
              #       card(helpText("A station can also be selected by clicking a point in the map or row in a table. The slopegraph at the bottom of the page compares the relative popularity of stations for starting or ending trips in the selected time period. The slopegraph shows all stations which are in the top 15 in popularity for either starting or ending trips during the time period selected. A station can also be selected by clicking on the slopegraph itself.")),
              #       col_widths = c(6, 6)
              #     )
              #   )
              # ),
              layout_columns(
                card(radioButtons("year_vs_month",
                                  "Time Period to Display:",
                                  choices = c("Yearly", "Monthly"),
                                  selected = "Yearly"),
                     selectInput("select_month",
                                 "Month to Display:",
                                 choices = NULL,
                                 selected = NULL,
                                 selectize = FALSE)
                ),
                card( 
                  # note selectize is used here with multiple selections enabled and a max number of selections of 1 to facilitate having the app initialize with no station selected
                  selectizeInput("select_station",
                                 "Choose Station:",
                                 choices = station_choices,
                                 multiple = TRUE,
                                 selected = NULL,
                                 options = list(maxItems = 1)),
                  checkboxInput("filter_top_stations",
                                "Display Top 15 Stations Only",
                                value = FALSE)
                ),
                col_widths = c(6,6)
              ),
              layout_columns(
                card(card_header("Top Starting Stations"),
                     leafletOutput("top_start_stations")),
                card(card_header("Top Ending Stations"),
                     leafletOutput("top_end_stations"))
              ),
              accordion(
                accordion_panel(
                  title = "Top Station Tables",
                  layout_columns(
                    card(card_header("Top 15 Starting Stations"),
                         card_body(DTOutput("top_15_start_stations"),
                                   min_height = "600px"
                                   # max_height = "1100px"
                         )
                    ),
                    card(card_header("Top 15 Ending Stations"),
                         card_body(DTOutput("top_15_end_stations"),
                                   min_height = "600px"
                                   # max_height = "1100px"
                         )
                    ),
                    col_widths = c(6,6)
                  )
                )
              ),
              card(
                card_header("Compare Starting vs. Ending Station Rank"),
                girafeOutput("slopegraph")
              )
      ),
    nav_panel(title = "Start vs. End Ratio",
              layout_columns(
                card(titlePanel("Ratio of Trips Started to Ended by Station")),
                card(actionButton("help_3",
                                  "Help")),
                col_widths = c(9,3),
              ),
              card(radioButtons("year_vs_month_io",
                                "Time Period to Display",
                                choices = c("Yearly", "Monthly"),
                                selected = "Yearly"),
                   selectInput("select_month_io",
                               "Month to Display",
                               choices = NULL,
                               selected = NULL,
                               selectize = FALSE)
              ),
              card(leafletOutput("io_map"))
              ),
    nav_panel(title = "Trips by Station",
              layout_columns(
                card(titlePanel("Trips by Station")),
                card(actionButton("help_4",
                                  "Help")),
                col_widths = c(6,-3,3)
              ),
              # Custom tags allow the dropdown box to expand the card boundary rather than truncating to 1 option
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
              ))
    )
)

#### BUILD SERVER ----

server <- function(input, output, session){
  ### PANE 1 SERVER CONTENTS ----
  
  observeEvent(input$help_1, {
    showModal(modalDialog(
      title = "How this pane works",
      HTML("This pane shows data on the number of trips made per day throughout 2022. The date can be chosen on the slider at the top; by clicking the play/pause button on the slider the visualization can be animated.<br><br>
           The heatmap shows the spatial distribution of trips (based on starting station location) while the bar plot below provides the total number of trips associated with the heatmap, segmented by User Type.")
    ))
  })
  
  heatmap_data <- reactive({
    req(input$date_slider)
    rides_2022_dset %>% filter(date(Start.Time) == input$date_slider) %>%
      collect() %>%
      left_join(select(stations_update_2022_sf, c(station_id, geometry)),
                by = join_by(Start.Station.Id == station_id),
                relationship = "many-to-one")
  })
  
  output$time_series_heatmap <- renderLeaflet({
    leaflet(rides_2022_dset %>% collect()) %>%
      addTiles() %>%
      setView(lng = -79.38, lat = 43.65, zoom = 11)
  })
  
  observe({
    leafletProxy("time_series_heatmap", data = heatmap_data()) %>%
      clearGroup("heatmap") %>%
      addHeatmap(
        data = heatmap_data()$geometry,
        radius = 8,
        group = "heatmap"
      )
  })
  
  output$daily_rides <- renderPlot({
    rides_2022_daily_sum %>%
      filter(date == input$date_slider) %>%
      ggplot(aes(x = User.Type, y = n)) +
      geom_bar(aes(fill = User.Type), stat = "identity") +
      geom_label(aes(label = n,
                     y = 31000,
                     fill = User.Type,
                     fontface = "bold",
                     size = 14)) +
      labs(title = paste0(as.character(wday(input$date_slider, label = TRUE)),
                          ", ",
                          as.character(month(input$date_slider, label = TRUE)),
                          " ",
                          day(input$date_slider),
                          " ",
                          year(input$date_slider)),
           x = "User Type",
           y = "Number of Trips") +
      scale_y_continuous(limits = c(0, max(rides_2022_daily_sum$n)+5000),
                         expand = expansion(add = c(0,0)),
                         breaks = seq(0, max(rides_2022_daily_sum$n), by = 5000)) +
      scale_fill_manual(values = c(zis_colours[1], zis_colours[5], zis_colours[3])) +
      theme_bikeshare() +
      theme(legend.position = "none",
            plot.margin = margin(1, 0, 1, 0, "cm"))
  })
  
  ### PANE 2 SERVER CONTENTS ----
  
  observeEvent(input$help_2, {
    showModal(modalDialog(
      title = "How this pane works",
      HTML("Explore the most popular stations to start and end Bike Share Toronto trips at during 2022 using this pane. The controls below can be used to select full year or monthly data, choose the month of interest, or search for a particular station. The tick box to display the top 15 stations will restrict the maps to showing the most popular stations; these 15 stations match those shown in the tables below, which can be expanded or hidden.<br><br>
           A station can also be selected by clicking a point in the map or row in a table. The slopegraph at the bottom of the page compares the relative popularity of stations for starting or ending trips in the selected time period. The slopegraph shows all stations which are in the top 15 in popularity for either starting or ending trips during the time period selected. A station can also be selected by clicking on the slopegraph itself.")
    ))
  })
  
  # an empty reactive value to hold the data on  stations filtered by time period
  top_start_stations_df <- reactiveVal(value = NULL)
  top_end_stations_df <- reactiveVal(value = NULL)
  
  observeEvent(input$year_vs_month, {
    if (input$year_vs_month == "Yearly"){
      # generate dataset containing all 2022 rides
      temp_df <- rides_2022_dset %>%
        count(Start.Station.Id) %>%
        arrange(desc(n)) %>%
        collect() %>%
        left_join(select(stations_update_2022_sf,
                         c(station_id, name, geometry)),
                  by = join_by(Start.Station.Id == station_id)) %>%
        rename(station_id = Start.Station.Id) %>%
        mutate(rank = row_number())
      top_start_stations_df(temp_df)
      
      temp_df2 <- rides_2022_dset %>%
        count(End.Station.Id) %>%
        arrange(desc(n)) %>%
        collect() %>%
        left_join(select(stations_update_2022_sf,
                         c(station_id, name, geometry)),
                  by = join_by(End.Station.Id == station_id)) %>%
        rename(station_id = End.Station.Id) %>%
        mutate(rank = row_number())
      top_end_stations_df(temp_df2)
      
      # make month select input NULL to prevent a month being chosen when yearly data displayed
      updateSelectInput(session, "select_month",
                        choices = "",
                        selected = "")
    }
    
    else if (input$year_vs_month == "Monthly"){
      # generate dataset containing all 2022 rides grouped by month
      # temp_df <- rides_2022_sf %>%
      #   count(Start.Month = month(Start.Time), Start.Station.Id) %>%
      #   arrange(Start.Month, desc(n)) %>%
      #   left_join(stations_update_2022_sf,
      #             by = join_by(Start.Station.Id == station_id)) %>%
      #   select(Start.Month, Start.Station.Id, n, name, geometry) %>%
      #   rename(station_id = Start.Station.Id) %>%
      #   mutate(rank = row_number()) %>%
      #   filter(Start.Month == input$select_month)
      # top_start_stations_df(temp_df)
      # 
      # populate month select input with choices when monthly data displayed
      updateSelectInput(session, "select_month",
                        choices = month_values,
                        selected = 1)
    }
  })
  
  # when monthly data is to be shown, group dataset by month and then filter for month selected
  observeEvent(input$select_month, {
    req(input$year_vs_month == "Monthly")
    temp_df <- rides_2022_dset %>%
      # NOTE to get Arrow to filter by a numeric value for month from the selectInput, the value must first be escaped with "!!" and then recast as numeric again. Why? I don't know.
      filter(month(Start.Time) == as.numeric(!!input$select_month)) %>%
      count(Start.Month = month(Start.Time), Start.Station.Id) %>%
      arrange(desc(n)) %>%
      collect() %>%
      left_join(select(stations_update_2022_sf,
                       c(station_id, name, geometry)),
                by = join_by(Start.Station.Id == station_id)) %>%
      rename(station_id = Start.Station.Id) %>%
      mutate(rank = row_number())
    top_start_stations_df(temp_df)
    
    temp_df2 <- rides_2022_dset %>%
      filter(month(End.Time) == as.numeric(!!input$select_month)) %>%
      count(End.Month = month(End.Time), End.Station.Id) %>%
      arrange(desc(n)) %>%
      collect() %>%
      left_join(select(stations_update_2022_sf,
                       c(station_id, name, geometry)),
                by = join_by(End.Station.Id == station_id)) %>%
      rename(station_id = End.Station.Id) %>%
      mutate(rank = row_number())
    top_end_stations_df(temp_df2)  
  })
  
  # when the filtered data is changed, plot the new set of station markers
  observeEvent(c(top_start_stations_df(),
                 top_end_stations_df(),
                 input$filter_top_stations), {
                   
                   if (input$filter_top_stations == FALSE){
                     leafletProxy("top_start_stations") %>%
                       clearGroup("station_popup") %>%
                       clearGroup("highlighted_start_station") %>%
                       clearGroup("start_stations") %>%
                       clearControls() %>%
                       addCircleMarkers(layerId = ~top_start_stations_df()$station_id,
                                        lng = st_coordinates(top_start_stations_df() %>%
                                                               pull(geometry))[,1],
                                        lat = st_coordinates(top_start_stations_df() %>%
                                                               pull(geometry))[,2],
                                        radius = 8,
                                        color = "black",
                                        weight = 2,
                                        opacity = 0.6,
                                        fillColor = ~pal_ride_num(top_start_stations_df()$n),
                                        fillOpacity = 0.8,
                                        data = top_start_stations_df(),
                                        group = "start_stations") %>%
                       addLegend(pal = pal_ride_num,
                                 values = top_start_stations_df()$n,
                                 group = "start_stations",
                                 position = "bottomright")
                     
                     leafletProxy("top_end_stations") %>%
                       clearGroup("station_popup") %>%
                       clearGroup("highlighted_end_station") %>%
                       clearGroup("end_stations") %>%
                       clearControls() %>%
                       addCircleMarkers(layerId = ~top_end_stations_df()$station_id,
                                        lng = st_coordinates(top_end_stations_df() %>%
                                                               pull(geometry))[,1],
                                        lat = st_coordinates(top_end_stations_df() %>%
                                                               pull(geometry))[,2],
                                        radius = 8,
                                        color = "black",
                                        weight = 2,
                                        opacity = 0.6,
                                        fillColor = ~pal_ride_num(top_end_stations_df()$n),
                                        fillOpacity = 0.8,
                                        data = top_end_stations_df(),
                                        group = "end_stations") %>%
                       addLegend(pal = pal_ride_num,
                                 values = top_end_stations_df()$n,
                                 group = "end_stations",
                                 position = "bottomright")
                   }
                   else if (input$filter_top_stations == TRUE) {
                     filtered_top_start_stations <- top_start_stations_df() %>%
                       slice_head(n = 15)
                     
                     filtered_palette <- colorNumeric("Reds",
                                                      domain = top_start_stations_df()$n)
                     
                     leafletProxy("top_start_stations") %>%
                       clearGroup("station_popup") %>%
                       clearGroup("highlighted_start_station") %>%
                       clearGroup("start_stations") %>%
                       clearControls() %>%
                       addCircleMarkers(layerId = ~filtered_top_start_stations$station_id,
                                        lng = st_coordinates(filtered_top_start_stations %>%
                                                               pull(geometry))[,1],
                                        lat = st_coordinates(filtered_top_start_stations %>%
                                                               pull(geometry))[,2],
                                        radius = 8,
                                        color = "black",
                                        weight = 2,
                                        opacity = 0.6,
                                        fillColor = ~filtered_palette(filtered_top_start_stations$n),
                                        fillOpacity = 0.8,
                                        data = top_start_stations_df(),
                                        group = "start_stations") %>%
                       addLegend(pal = filtered_palette,
                                 values = top_start_stations_df()$n,
                                 group = "start_stations",
                                 position = "bottomright")
                     
                     filtered_top_end_stations <- top_end_stations_df() %>%
                       slice_head(n = 15)
                     
                     filtered_palette_2 <- colorNumeric("Reds",
                                                        domain = top_end_stations_df()$n)
                     
                     leafletProxy("top_end_stations") %>%
                       clearGroup("station_popup") %>%
                       clearGroup("highlighted_end_station") %>%
                       clearGroup("end_stations") %>%
                       clearControls() %>%
                       addCircleMarkers(layerId = ~filtered_top_end_stations$station_id,
                                        lng = st_coordinates(filtered_top_end_stations %>%
                                                               pull(geometry))[,1],
                                        lat = st_coordinates(filtered_top_end_stations %>%
                                                               pull(geometry))[,2],
                                        radius = 8,
                                        color = "black",
                                        weight = 2,
                                        opacity = 0.6,
                                        fillColor = ~filtered_palette_2(filtered_top_end_stations$n),
                                        fillOpacity = 0.8,
                                        data = top_end_stations_df(),
                                        group = "end_stations") %>%
                       addLegend(pal = filtered_palette_2,
                                 values = top_end_stations_df()$n,
                                 group = "end_stations",
                                 position = "bottomright")
                   }      
                 })
  
  # set default selected station value from inputs to be null
  selected_station <- reactiveVal(value = NULL)
  
  # sync selected station from reactive value to inputs
  observeEvent(selected_station(), {
    req(!is.null(selected_station()))
    
    leafletProxy("top_start_stations") %>%
      clearGroup(group = "highlighted_start_station") %>%
      addCircles(stroke = TRUE,
                 weight = 15,
                 color = "black",
                 opacity = 0.8,
                 data = top_start_stations_df() %>%
                   filter(station_id == selected_station()) %>%
                   pull(geometry),
                 group = "highlighted_start_station")
    leafletProxy("top_start_stations") %>%
      clearGroup("station_popup")
    popup_text <- filter(top_start_stations_df(), station_id == selected_station()) %>%
      get_station_rank_popup()
    leafletProxy("top_start_stations") %>%
      addPopups(
        popup = popup_text,
        data = top_start_stations_df() %>%
          filter(station_id == selected_station()) %>%
          pull(geometry),
        group = "station_popup"
      )
    
    leafletProxy("top_end_stations") %>%
      clearGroup(group = "highlighted_end_station") %>%
      addCircles(stroke = TRUE,
                 weight = 15,
                 color = "black",
                 opacity = 0.8,
                 data = top_end_stations_df() %>%
                   filter(station_id == selected_station()) %>%
                   pull(geometry),
                 group = "highlighted_end_station")
    leafletProxy("top_end_stations") %>%
      clearGroup("station_popup")
    popup_text <- filter(top_end_stations_df(), station_id == selected_station()) %>%
      get_station_rank_popup()
    leafletProxy("top_end_stations") %>%
      addPopups(
        popup = popup_text,
        data = top_end_stations_df() %>%
          filter(station_id == selected_station()) %>%
          pull(geometry),
        group = "station_popup"
      )
    
    updateSelectizeInput(session,
                         inputId = "select_station",
                         selected = selected_station())
    
    session$sendCustomMessage(type = "slopegraph_set", message = selected_station())
  })
  
  # sync selected station from inputs to the reactive value
  observeEvent(input$top_start_stations_marker_click$id, {
    selected_station(input$top_start_stations_marker_click$id)
  })
  observeEvent(input$top_15_start_stations_rows_selected, {
    selected_row_start <- top_start_stations_df() %>%
      slice(input$top_15_start_stations_rows_selected) %>%
      pull(station_id)
    selected_station(selected_row_start)
  })
  
  observeEvent(input$top_end_stations_marker_click$id, {
    selected_station(input$top_end_stations_marker_click$id)
  })
  observeEvent(input$top_15_end_stations_rows_selected, {
    selected_row_end <- top_end_stations_df() %>%
      slice(input$top_15_end_stations_rows_selected) %>%
      pull(station_id)
    selected_station(selected_row_end)
  })
  
  observeEvent(input$select_station, {
    selected_station(input$select_station)
  })
  
  observeEvent(input$slopegraph_selected, {
    selected_station(input$slopegraph_selected)
  })
  
  # wipe the selected station reactive when the input data (and thus the map) changes, otherwise the currently selected station cannot be selected again on the map until the selected station value is changed
  observeEvent(top_start_stations_df(), {
    selected_station(NULL)
    updateSelectizeInput(session,
                         inputId = "select_station",
                         selected = "")
  })
  
  observeEvent(top_end_stations_df(), {
    selected_station(NULL)
    updateSelectizeInput(session,
                         inputId = "select_station",
                         selected = "")
  })
  
  output$top_start_stations <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -79.38, lat = 43.65, zoom = 11)
  })
  outputOptions(output, "top_start_stations", suspendWhenHidden = FALSE)
  
  output$top_end_stations <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -79.38, lat = 43.65, zoom = 11)
  })
  outputOptions(output, "top_end_stations", suspendWhenHidden = FALSE)
  
  output$top_15_start_stations <- renderDT({
    # NOTE the df must already be arranged, not arranged in the slice_head call, otherwise the original df cannot be filtered by row index to set the reactive element
    top_15_start_stations_df <- top_start_stations_df() %>%
      slice_head(n = 15) %>%
      select(station_id, name, n)
    datatable(top_15_start_stations_df,
              selection = "single",
              rownames = FALSE,
              fillContainer = TRUE,
              # height = "100%",
              # style = "bootstrap4",
              options = list(dom = "t",
                             searching = FALSE,
                             pageLength = 15),
              colnames = c("Station ID", "Location", "# of Rides Started"))
  })
  
  output$top_15_end_stations <- renderDT({
    # NOTE the df must already be arranged, not arranged in the slice_head call, otherwise the original df cannot be filtered by row index to set the reactive element
    top_15_end_stations_df <- top_end_stations_df() %>%
      slice_head(n = 15) %>%
      select(station_id, name, n)
    datatable(top_15_end_stations_df,
              selection = "single",
              rownames = FALSE,
              fillContainer = TRUE,
              # height = "100%",
              # style = "bootstrap4",
              options = list(dom = "t",
                             searching = FALSE,
                             pageLength = 15),
              colnames = c("Station ID", "Location", "# of Rides Ended"))
  })
  
  output$slopegraph <- renderGirafe({
    top_startvend_df <- top_start_stations_df() %>%
      select(c(station_id, name, rank)) %>%
      rename(start.rank = rank) %>%
      left_join(top_end_stations_df() %>%
                  select(c(station_id, name, rank)) %>%
                  rename(end.rank = rank),
                by = join_by(station_id, name))
    
    top_startvend_df <- top_startvend_df %>%
      # only stations which are in the top 15 start OR end will appear
      filter(start.rank <= 15 | end.rank <= 15) %>%
      pivot_longer(cols = !(station_id:name),
                   names_to = "terminus",
                   values_to = "rank") %>%
      mutate(terminus = as.factor(terminus)) %>%
      mutate(terminus = fct_relevel(terminus, "start.rank", "end.rank"))
    
    p <- ggplot(top_startvend_df, aes(x = terminus,
                                      y = rank,
                                      group = station_id)) +
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
      scale_y_reverse(limits = c(max(top_startvend_df$rank), 1),
                      expand = c(0,1),
                      breaks = c(1,seq(5,40, 5)),
                      sec.axis = sec_axis(trans = ~.,
                                          breaks = c(1,seq(5,40, 5)))) +
      theme_bikeshare() +
      theme(plot.margin = margin(5,15,5,5))
    
    girafe(ggobj = p,
           options = list(
             opts_hover_inv(css = "stroke-width:1;opacity:0.4"),
             opts_hover(css = "stroke-width:4;opacity:1"),
             opts_selection(type = "single",
                            css = "stroke-width:4;opacity:1;stroke:red")
           ))
  })
  ### PANE 3 SERVER CONTENTS ----
  
  observeEvent(input$help_3, {
    showModal(modalDialog(
      title = "How this pane works",
      HTML("See whether Bike Share Toronto stations were used as the start or end point of rides using this panel. The controls below can be used to select full year or monthly data, and to choose the month of interest in the latter case.<br><br>
The map uses colour-coding to display the ratio of rides started to rides ended, calculated as (# trips started)/(# trips ended); a ratio greater than 1 (blue shades) means more trips were started than ended, while a ratio less than 1 (red shades) means more trips ended than started.<br><br>
To see the data for a given station, click on the station marker on the map and view the actual number of rides started and ended in the resulting pop-up bar plot")
    ))
  })
  
  # an empty reactive value to hold the data on IO filtered by time period
  io_input <- reactiveVal(value = NULL)
  
  observeEvent(input$year_vs_month_io, {
    
    if (input$year_vs_month_io == "Yearly"){
      # generate dataset containing all 2022 rides
      # NOTE that in addition to the quirky handling of the selectInput value for month (see notes in pane 1), there is an issue with full_join of arrow tables where it will retain the left table key as NA if the value is only present in the right table (at least as of arrow 12), so tables are collected into R before joining
      temp_df_io <- rides_2022_dset %>%
        count(Start.Station.Id) %>%
        rename(Start.Count = n) %>%
        collect() %>%
        full_join(rides_2022_dset %>%
                    count(End.Station.Id) %>%
                    rename(End.Count = n) %>%
                    collect(),
                  by = join_by(Start.Station.Id == End.Station.Id)) %>%
        mutate(IO.Ratio = Start.Count/End.Count) %>%
        left_join(select(stations_update_2022_sf,
                         c(station_id, name, geometry)),
                  by = join_by(Start.Station.Id == station_id)) %>%
        rename(station_id = Start.Station.Id)
      io_input(temp_df_io)
      
      # Make month select input NULL to prevent a month being chosen when yearly data displayed
      updateSelectInput(session,
                        "select_month_io",
                        choices = "",
                        selected = "")
    }
    
    else if (input$year_vs_month_io == "Monthly"){
      updateSelectInput(session,
                        "select_month_io",
                        choices = month_values,
                        selected = 1)
    }
  })
  
  observeEvent(input$select_month_io, {
    req(input$year_vs_month_io == "Monthly")
    temp_df_io <- rides_2022_dset %>%
      filter(month(Start.Time) == as.numeric(!!input$select_month_io)) %>%
      count(Start.Station.Id, Trip.Month = month(Start.Time)) %>%
      rename(Start.Count = n) %>%
      collect() %>%
      full_join(rides_2022_dset %>%
                  filter(month(End.Time) == as.numeric(!!input$select_month_io)) %>%
                  count(End.Station.Id, Trip.Month = month(End.Time)) %>%
                  rename(End.Count = n) %>%
                  collect(),
                by = join_by(Start.Station.Id == End.Station.Id,
                             Trip.Month)) %>%
      mutate(IO.Ratio = Start.Count/End.Count) %>%
      left_join(select(stations_update_2022_sf,
                       c(station_id, name, geometry)),
                by = join_by(Start.Station.Id == station_id)) %>%
      rename(station_id = Start.Station.Id)
    
    io_input(temp_df_io)
  })
  
  observeEvent(io_input(), {
    leafletProxy("io_map") %>%
      clearGroup("io_stations") %>%
      clearPopups() %>%
      clearControls() %>%
      addCircleMarkers(layerId = ~io_input()$station_id,
                       lng = st_coordinates(io_input()$geometry)[,1],
                       lat = st_coordinates(io_input()$geometry)[,2],
                       radius = 8, 
                       color = "black",
                       weight = 2,
                       opacity = 0.6,
                       fillColor = ~pal_io(io_input()$IO.Ratio),
                       fillOpacity = 0.8,
                       data = io_input(),
                       group = "io_stations"
      ) %>%
      addLegend(colors = io_legend_colors,
                labels = io_legend_labels,
                opacity = 0.8,
                title = HTML("Start to<br>End Ratio"),
                position = "bottomright")
    
  })
  
  observeEvent(input$io_map_marker_click, {
    popup_content <- io_input() %>%
      filter(station_id == input$io_map_marker_click$id) %>%
      get_io_popup()
    leafletProxy("io_map") %>%
      clearPopups() %>%
      addPopups(data = io_input() %>%
                  filter(station_id == input$io_map_marker_click$id) %>%
                  pull(geometry),
                # group = "io_popups",
                popup = popup_content) 
  })
  
  output$io_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -79.38, lat = 43.65, zoom = 11)
  })
  outputOptions(output, "io_map", suspendWhenHidden = FALSE)
  
  ### PANE 4 SERVER CONTENTS ----
  
  observeEvent(input$help_4, {
    showModal(modalDialog(
      title = "How this pane works",
      HTML("This pane shows the number of rides that start and end at a particular station over a selected time period. The station of interest can be chosen using the dropdown menu at the top.<br/><br/>

The top bar plots show data by month across 2022. To drill down into the monthly data, a month of interest is selected by clicking on the bars in one of the Trips by Month plots. This will display the rides by day for that month in the line plots immediately below. To drill down further, a day of interest is selected by clicking on any of the data points for that day in one of the Trips by Day plots. The hourly data for that day is then displayed in the line plots immediately below.<br/><br/>

The time period chosen is synchronized between the plots for Trips Started and Trips Ended, so choosing a time period for one type of data will change the period for the other as well. To see details of rides by user type in any given plot, hover over the data points of interest to display a popup.")
    ))
  })
  
  # YEARLY DATA
  
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
  
  
  # MONTHLY DATA
  
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
  
  # DAILY DATA
  
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
      geom_line() +
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
      geom_line() +
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

#### RUN APP ----

shinyApp(ui = ui, server = server)