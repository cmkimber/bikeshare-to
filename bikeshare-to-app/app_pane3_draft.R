library(tidyverse)
library(sf)
library(shiny)
library(bslib)
library(leaflet)
library(RColorBrewer)
library(glue)
library(leafpop)

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

# set scale range for the IO ratio to be symmetrical about 1 to make a diverging palette that is white at 1
io_scale_range <- c(0,2)
# pal_io_legend <- colorNumeric(palette = "RdBu",
#                         domain = io_scale_range)
# manually set colours for the legend because the plotted palette saturates at 2 but automatic legend will not
io_legend_colors <- c(pal_io(0), pal_io(0.25), pal_io(0.5), pal_io(0.75), pal_io(1), pal_io(1.25), pal_io(1.5), pal_io(1.75), pal_io(2))
io_legend_labels <- c("0", "0.25", "0.5", "0.75", "1", "1.25", "1.5", "1.75", "2")

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

# get_io_popup_text <- function(station_row){
#   popup_text <- glue(
#     "Station ID: {station_row$station_id}"
#   )
#   return(popup_text)
# }

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
    theme_minimal() + 
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

ui <- page_fluid(
  layout_columns(
    card(titlePanel("Ratio of Trips Started to Ended by Station")),
    card(actionButton("help_3",
                      "Help")),
    col_widths = c(9,3)
  ),
  leafletOutput("io_map")
)

server <- function(input, output, session){
  
  observeEvent(input$help_3, {
    showModal(modalDialog(
      title = "How this pane works",
      HTML("Lorem ipsum etc.")
    ))
  })
  
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
  
  observeEvent(input$io_map_marker_click, {
    # io_popup_text <- io_yearly %>%
    #   filter(station_id == input$io_map_marker_click$id) %>%
    #   get_io_popup_text()
    popup_content <- io_yearly %>%
      filter(station_id == input$io_map_marker_click$id) %>%
      get_io_popup()
    leafletProxy("io_map") %>%
      clearPopups() %>%
      addPopups(data = io_yearly %>%
                  filter(station_id == input$io_map_marker_click$id) %>%
                  pull(geometry),
                group = "io_popups",
                popup = popup_content) 
  })
}

shinyApp(ui = ui, server = server)