library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(DT)
library(RColorBrewer)
library(glue)

rides_2022_sf <- readRDS("./Data/rides_2022_sf.rds")
stations_update_2022_sf <- readRDS("./Data/stations_update_2022_sf.rds")

top_start_stations_2022 <- rides_2022_sf %>%
  count(Start.Station.Id) %>%
  arrange(desc(n)) %>%
  left_join(stations_update_2022_sf,
            by = join_by(Start.Station.Id == station_id)) %>%
  select(Start.Station.Id, n, name, geometry) %>%
  rename(station_id = Start.Station.Id) %>%
  mutate(rank = row_number())

pal_ride_num <- colorNumeric(palette = "Reds",
                             domain = top_start_stations_2022$n)

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
  leafletOutput("top_start_stations"),
  DTOutput("top_15_start_stations")
)

server <- function(input, output, session){
  
  # set default reactive value from inputs to be null
  selected_start_station <- reactiveVal(value = NULL)
  
  # sync from reactive value to inputs
  observeEvent(selected_start_station(), {
    req(!is.null(selected_start_station()))
    leafletProxy("top_start_stations") %>%
      clearGroup(group = "highlighted_start_station")
    leafletProxy("top_start_stations") %>%
      addCircles(stroke = TRUE,
                 weight = 15,
                 color = "black",
                 opacity = 0.8,
                 data = top_start_stations_2022 %>%
                   filter(station_id == selected_start_station()) %>%
                   pull(geometry),
                 group = "highlighted_start_station")
    leafletProxy("top_start_stations") %>%
      clearGroup("station_popup")
    popup_text <- filter(top_start_stations_2022, station_id == selected_start_station()) %>%
      get_station_rank_popup()
    leafletProxy("top_start_stations") %>%
      addPopups(
                popup = popup_text,
                data = top_start_stations_2022 %>%
                  filter(station_id == selected_start_station()) %>%
                           pull(geometry),
                group = "station_popup"
                         )
  })
  
  # sync from inputs to the reactive value
  observeEvent(input$top_start_stations_marker_click$id, {
    selected_start_station(input$top_start_stations_marker_click$id)
  })
  observeEvent(input$top_15_start_stations_rows_selected, {
    selected_row_start <- top_start_stations_2022 %>%
      slice(input$top_15_start_stations_rows_selected) %>%
      pull(station_id)
    selected_start_station(selected_row_start)
  })
  
  output$top_start_stations <- renderLeaflet({
    leaflet(data = top_start_stations_2022) %>%
      addTiles() %>%
      setView(lng = -79.38, lat = 43.65, zoom = 11) %>%
      addCircleMarkers(layerId = ~top_start_stations_2022$station_id,
                 lng = st_coordinates(top_start_stations_2022$geometry)[,1],
                 lat = st_coordinates(top_start_stations_2022$geometry)[,2],
                 radius = 8,
                 color = "black",
                 weight = 2,
                 opacity = 0.6,
                 fillColor = ~pal_ride_num(top_start_stations_2022$n),
                 fillOpacity = 0.8)
  })
  
  output$top_15_start_stations <- renderDT({
    # NOTE the df must already be arranged, not arranged in the slice_head call, otherwise the original df cannot be filtered by row index to set the reactive element
    top_15_start_stations_df <- top_start_stations_2022 %>%
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
  
}

shinyApp(ui = ui, server = server)