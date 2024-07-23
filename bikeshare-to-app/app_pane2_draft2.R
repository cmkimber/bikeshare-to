library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(DT)
library(RColorBrewer)
library(glue)

### Load data and functions
rides_2022_sf <- readRDS("./Data/rides_2022_sf.rds")
stations_update_2022_sf <- readRDS("./Data/stations_update_2022_sf.rds")

month_values <- seq(1,12,1)
names(month_values) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

pal_ride_num <- colorNumeric(palette = "Reds", domain = NULL)

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

ui <- fluidPage(
  titlePanel(
    "Title Here"
  ),
  sidebarLayout(
    sidebarPanel(
      radioButtons("year_vs_month",
                   "Time Period to Display:",
                   choices = c("Yearly", "Monthly"),
                   selected = "Yearly"),
      selectInput("select_month",
                  "Month to Display:",
                  choices = NULL,
                  selected = NULL,
                  selectize = FALSE),
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
    mainPanel(
      fluidRow(
        column(6,
               leafletOutput("top_start_stations"),
               DTOutput("top_15_start_stations")
               ),
        column(6,
               leafletOutput("top_end_stations"),
               DTOutput("top_15_end_stations")
               )
      )
    )
  )
)

server <- function(input, output, session){
  
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
              options = list(dom = "t",
                             searching = FALSE,
                             pageLength = 15),
              colnames = c("Station ID", "Location", "# of Rides Started"))
  })
  
}

shinyApp(ui = ui, server = server)