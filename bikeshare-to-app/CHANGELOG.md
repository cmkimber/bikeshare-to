# Changelog

## v.2.1

### Added or Changed

* Added pane 4 to the app
* Pane 4 displays the number of trips started and ended at a given station across 3 different time periods (monthly, daily, hourly) for the two different user types
  - This is visualized using ggiraph, with a bar plot showing monthly data and line plots showing daily and hourly data for a selected month/day
  - Drilling down the time period is accomplished using the interactive selection features of ggiraph, where clicking on a time period in a higher order graph selects the focal period for a lower order graph

## v.2.0

### Added or Changed

* Converted the application's handling of trip datafrom RDS binaries to Parquet files and Apache Arrow
  - This change was intended to reduce the app's in-memory footprint and to make the app more responsive
  - The in-memory footprint reduced somewhat (but not enough to run on shinyapps.io's free tier), but the application is now much more responsive with major improvements in Leaflet performance
  - There are a few remaining quirks of the Arrow implementation ex. it does not correctly handle full joins and cannot join to a table containing an sf geometry

## v.1.1

### Added or Changed

* Added pane 3 to the app
* Pane 3 displays the ratio of trips started to trips ended by station for a given time period (yearly or monthly)
  - This is visualized using a Leaflet map with colour-coded markers to indicate the start to end ratio (using a custom diverging palette to maintain symmetry at 1)
  - Trips started and ended by station for the time period are displayed using bar plots in ggplot2 that are generated when a station is selected in Leaflet, converted to SVG images and rendered as custom Leaflet popups

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
