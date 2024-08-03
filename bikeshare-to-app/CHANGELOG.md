# Changelog

## v1.0.2

### Added or Changed

* Added legends to the maps on pane 2
  - Legends are dynamically updated when the time period displayed on the map changes
* Moved the help text on pane 2 from an accordion element at the top of the pane to a modal help button
* Updated pane 2 slopegraph theme to match ggplot2 theme developed for the rest of the project

## v1.0.1

### Added or Changed

* Reformatted pane 1 (trip numbers by day) to have width-responsive column number
  - On narrow screens the heatmap and barplot of trip number per day display in a single column, while on wide screens they display adjacent to one another in two columns

## v1.0.0

### Added or Changed

* Initialized Shiny app with 2 panes of content
* Pane 1 is an animated display of trip numbers by date linking two visualizations to a synchronized timeline
  - Visualization 1 is a heatmap showing trip starting stations for a given day, built using Leaflet
  - Visualization 2 is a barplot showing the number of trips for a given day by user type, built using ggplot2
* Pane 2 displays the most popular starting and ending stations for trips in a given time period (yearly or monthly)
  - Visualization 1 is a pair of Leaflet maps with colour-gradient markers showing each station's relative use as a starting or ending point for trips
  - Visualization 2 is a pair of Data Table tables that show the top 15 starting and ending stations
  - Visualization 3 is a slopegraph showing the relationship in popularity for starting and ending trips for the top 15 stations, built with ggiraph
  - Information on a given station can be obtained by interacting with any visualization or with a dropdown input
