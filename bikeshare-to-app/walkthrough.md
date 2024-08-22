# App Walkthrough

## Raison d'etre

At present, the Shiny app is too memory-intensive to host on shinyapps.io, and serverless hosting using `shinylive` is not possible (because `curl` is not available in webR). While exploring other avenues for free/nearly free hosting, I have made this GIF walkthrough to show the app's functionality.

## Pane 1: Trip Numbers by Date

This pane shows daily spatial density of Bike Share Toronto system use across 2022, visualized using a heatmap. The heatmap is generated using the start points of rides, as ride routes are not available. Using the date slider, a given date can be chosen or the visualization can be animated using the play/pause buttons. To contextualize the spatial density of system use, the accompanying bar plot shows the number of trips made on the selected day, segmented by user type.

![](https://github.com/cmkimber/bikeshare-to/blob/42e9f3ad054294d757f8842be386ce29417759c7/Assets/Screen_Recording_Pane_1.gif)

## Pane 2: Starting vs. Ending Station Use by Month

This pane shows how popular Bike Share Toronto stations were as starting or ending points for rides across 2022. The user can toggle between yearly and monthly data at the top of the pane.

The number of trips started and ended at all stations during the selected time period is shown on a pair of colour-coded maps, while the number of rides at the top 15 starting and ending stations is shown in expandable tables beneath. The maps can be filtered to display only the top 15 stations shown in the tables using a control at the top of the pane.

The relationship between the number of rides started and ended for a station is visualized in the slopegraph at the bottom of the pane; this also shows data for the top 15 stations.

A given station can be highlighted by selecting it at the top of the pane, or clicking on it in any of the visualizations.

![](https://github.com/cmkimber/bikeshare-to/blob/42e9f3ad054294d757f8842be386ce29417759c7/Assets/Screen_Recording_Pane_2.gif)

## Pane 3: Ratio of Trips Started to Ended by Station

This pane shows how traffic flows into and out of a given Bike Share Toronto station in 2022, visualized using a ratio of rides started to rides ended. The user can toggle between yearly and monthly data at the top of the pane.

The ratio of trips started to ended is shown using colour-coded markers on the map; blue markers have more flow out than in and vice versa for red markers. A popup with data on a specific station, including a bar plot showing the number of trips started and ended, can be displayed by clicking on the station marker on the map.

![](https://github.com/cmkimber/bikeshare-to/blob/42e9f3ad054294d757f8842be386ce29417759c7/Assets/Screen_Recording_Pane_3.gif)

## Acknowledgements

* [convert-video-to-gif](https://blog.interaction-dynamics.io/how-to-convert-your-screen-recordings-to-gif-on-macos-productivity-1-781dbe56fe5c)
