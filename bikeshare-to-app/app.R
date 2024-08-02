library(tidyverse)
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

rides_2022_sf <- readRDS("./Data/rides_2022_sf.rds")

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

rides_2022_daily_sum <- rides_2022_sf %>% group_by(User.Type) %>% count(date = date(Start.Time)) %>% bind_rows(rides_2022_sf %>% group_by(date = date(Start.Time)) %>% count() %>% mutate(User.Type = "Total"))

### LOAD PANE 2 DATA AND FUNCTIONS ----

stations_update_2022_sf <- readRDS("./Data/stations_update_2022_sf.rds")

# define month abbreviations for monthly data select input
month_values <- seq(1,12,1)
names(month_values) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

pal_ride_num <- colorNumeric(palette = "Reds", domain = NULL)

# define choices for station select input
station_choices <- stations_update_2022_sf$station_id
for (i in (1:length(station_choices)))
  names(station_choices)[i] <- paste0(stations_update_2022_sf$name[i],
                                      " (#",
                                      stations_update_2022_sf$station_id[i],
                                      ")")

get_station_rank_popup <- function(station_row){
  popup_text <- glue(
    "<b>Station ID#:</b> {station_row$station_id}<br/>",
    "<b>Station Name:</b> {station_row$name}<br/>",
    "<b>Number of Rides:</b> {station_row$n}<br/>",
    "<b>Rank:</b> {station_row$rank}"
  )
  return(popup_text)
}

#### BUILD UI ----

ui <- page_fluid(
  navset_tab(
    nav_panel(title = "Pane 1",
              layout_columns(
                card(titlePanel("Trip Numbers By Date")),
                card(actionButton("help_1",
                                  "Help")),
                col_widths = c(6,-3,3)
              ),
              card(card_header("Select Date:"),
                   card_body(class = "align-items-center",
                             sliderInput("date_slider",
                                         NULL,
                                         min = min(date(rides_2022_sf$Start.Time)),
                                         max = max(date(rides_2022_sf$Start.Time)),
                                         value = min(date(rides_2022_sf$Start.Time)),
                                         step = 1,
                                         animate = animationOptions(interval = 3000),
                                         width = "90%"))
              ),
              card(card_header("Trip Start Density"),
                   leafletOutput("time_series_heatmap")),
              card(plotOutput("daily_rides"))
              ),
    nav_panel(title = "Pane 2",
              titlePanel(
                "Starting vs. Ending Station Use by Month"
              ),
              accordion(
                open = FALSE,
                accordion_panel(
                  "Click for Help",
                  layout_columns(
                    card(helpText("Explore the most popular stations to start and end Bike Share Toronto trips at during 2022 using this pane. The controls below can be used to select full year or monthly data, choose the month of interest, or search for a particular station. The tick box to display the top 15 stations will restrict the maps to showing the most popular stations; these 15 stations match those shown in the tables below, which can be expanded or hidden.")),
                    card(helpText("A station can also be selected by clicking a point in the map or row in a table. The slopegraph at the bottom of the page compares the relative popularity of stations for starting or ending trips in the selected time period. The slopegraph shows all stations which are in the top 15 in popularity for either starting or ending trips during the time period selected. A station can also be selected by clicking on the slopegraph itself.")),
                    col_widths = c(6, 6)
                  )
                )
              ),
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
      )
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
  
  filtered_data <- reactive({
    req(input$date_slider)
    filter(rides_2022_sf, date(Start.Time) == input$date_slider)
  })
  
  output$time_series_heatmap <- renderLeaflet({
    leaflet(rides_2022_sf) %>%
      addTiles() %>%
      setView(lng = -79.38, lat = 43.65, zoom = 11)
  })
  
  observe({
    leafletProxy("time_series_heatmap", data = filtered_data()) %>%
      clearGroup("heatmap") %>%
      addHeatmap(
        data = filtered_data()$start_geometry,
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
  # an empty reactive value to hold the data on  stations filtered by time period
  top_start_stations_df <- reactiveVal(value = NULL)
  top_end_stations_df <- reactiveVal(value = NULL)
  
  observeEvent(input$year_vs_month, {
    if (input$year_vs_month == "Yearly"){
      # generate dataset containing all 2022 rides
      temp_df <- rides_2022_sf %>%
        count(Start.Station.Id) %>%
        arrange(desc(n)) %>%
        left_join(stations_update_2022_sf,
                  by = join_by(Start.Station.Id == station_id)) %>%
        select(Start.Station.Id, n, name, geometry) %>%
        rename(station_id = Start.Station.Id) %>%
        mutate(rank = row_number())
      top_start_stations_df(temp_df)
      
      temp_df2 <- rides_2022_sf %>%
        count(End.Station.Id) %>%
        arrange(desc(n)) %>%
        left_join(stations_update_2022_sf,
                  by = join_by(End.Station.Id == station_id)) %>%
        select(End.Station.Id, n, name, geometry) %>%
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
    temp_df <- rides_2022_sf %>%
      count(Start.Month = month(Start.Time), Start.Station.Id) %>%
      arrange(Start.Month, desc(n)) %>%
      left_join(stations_update_2022_sf,
                by = join_by(Start.Station.Id == station_id)) %>%
      select(Start.Month, Start.Station.Id, n, name, geometry) %>%
      rename(station_id = Start.Station.Id) %>%
      filter(Start.Month == input$select_month) %>%
      mutate(rank = row_number())
    top_start_stations_df(temp_df)
    
    temp_df2 <- rides_2022_sf %>%
      count(End.Month = month(End.Time), End.Station.Id) %>%
      arrange(End.Month, desc(n)) %>%
      left_join(stations_update_2022_sf,
                by = join_by(End.Station.Id == station_id)) %>%
      select(End.Month, End.Station.Id, n, name, geometry) %>%
      rename(station_id = End.Station.Id) %>%
      filter(End.Month == input$select_month) %>%
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
                                        group = "start_stations")
                     
                     leafletProxy("top_end_stations") %>%
                       clearGroup("station_popup") %>%
                       clearGroup("highlighted_end_station") %>%
                       clearGroup("end_stations") %>%
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
                                        group = "end_stations")
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
                                        group = "start_stations")
                     
                     filtered_top_end_stations <- top_end_stations_df() %>%
                       slice_head(n = 15)
                     
                     filtered_palette_2 <- colorNumeric("Reds",
                                                        domain = top_end_stations_df()$n)
                     
                     leafletProxy("top_end_stations") %>%
                       clearGroup("station_popup") %>%
                       clearGroup("highlighted_end_station") %>%
                       clearGroup("end_stations") %>%
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
                                        group = "end_stations")
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
  
  output$top_end_stations <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -79.38, lat = 43.65, zoom = 11)
  })
  
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
      theme_minimal() +
      theme(plot.margin = margin(5,15,5,5))
    
    girafe(ggobj = p,
           options = list(
             opts_hover_inv(css = "stroke-width:1;opacity:0.4"),
             opts_hover(css = "stroke-width:4;opacity:1"),
             opts_selection(type = "single",
                            css = "stroke-width:4;opacity:1;stroke:red")
           ))
  })
}

#### RUN APP ---

shinyApp(ui = ui, server = server)