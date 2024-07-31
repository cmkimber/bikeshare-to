library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(wesanderson)

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

rides_2022_daily_sum <- rides_2022_sf %>% group_by(User.Type) %>% count(date = date(Start.Time)) %>% bind_rows(rides_2022_sf %>% group_by(date = date(Start.Time)) %>% count() %>% mutate(User.Type = "Total"))

ui <- page_fluid(
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
)

server <- function(input, output, session){
  
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
    
}

shinyApp(ui = ui, server = server)