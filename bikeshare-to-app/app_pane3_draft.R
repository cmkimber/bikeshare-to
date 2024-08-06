library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(glue)

rides_2022_sf <- readRDS("./Data/rides_2022_sf.rds")
stations_update_2022_sf <- readRDS("./Data/stations_update_2022_sf.rds")

io_yearly <- rides_2022_sf %>%
  count(Start.Station.Id) %>%
  rename(Start.Count = n) %>%
  full_join(rides_2022_sf %>%
              count(End.Station.Id) %>%
              rename(End.Count = n),
            by = join_by(Start.Station.Id == End.Station.Id)) %>%
  mutate(IO.Ratio = Start.Count/End.Count) %>%
  left_join(stations_update_2022_sf,
            by = join_by(Start.Station.Id == station_id)) %>%
  select(Start.Station.Id:name, geometry) %>%
  rename(station_id = Start.Station.Id)

io_scale_range <- c(0,2)
pal_io_legend <- colorNumeric(palette = "RdBu",
                        domain = io_scale_range)
io_legend_colors <- c(pal_io(0), pal_io(0.25), pal_io(0.5), pal_io(0.75), pal_io(1), pal_io(1.25), pal_io(1.5), pal_io(1.75), pal_io(2))
io_legend_labels <- c("0", "0.25", "0.5", "0.75", "1", "1.25", "1.5", "1.75", "2")

pal_io <- function(x){
  return(if_else(x > 2, #if it was founded in 1422
                 "#053061", #return this orange, else use colorNumeric
                 suppressWarnings(colorNumeric(palette = "RdBu",
                                               domain = io_scale_range, 
                                               na.color="black")(x)),
                 missing = "#FF000000"
                 )
         )
}

ui <- page_fluid(
  leafletOutput("io_map")
)

server <- function(input, output, session){
  
  output$io_map <- renderLeaflet({
    leaflet(data = io_yearly) %>%
      addTiles() %>%
      setView(lng = -79.38, lat = 43.65, zoom = 11) %>%
      addCircleMarkers(layerId = ~io_yearly$station_id,
                       lng = st_coordinates(io_yearly$geometry)[,1],
                       lat = st_coordinates(io_yearly$geometry)[,2],
                       radius = 8, 
                       color = "black",
                       weight = 2,
                       opacity = 0.6,
                       fillColor = ~pal_io(io_yearly$IO.Ratio),
                       fillOpacity = 0.8) %>%
      addLegend(colors = io_legend_colors,
                labels = io_legend_labels,
                opacity = 0.8,
                title = "Placeholder",
                position = "bottomright")
  })
}

shinyApp(ui = ui, server = server)