# Bike Share Toronto Web App

## About This Application

This web application is part of an analysis of publicly available usage data from Bike Share Toronto. More information about the analysis in a broader context, including information about the dataset used, can be found in the [main repo readme](/readme.md). The purpose of the application is to allow interactive exploration of how Bike Share Toronto was utilized during the year 2022, illuminating interesting patterns and complementing current and future written reports, with a particular focus on spatial components of the data presented as interactive maps. The application is currently WIP, with new elements roadmapped and being added regularly. [View an animated walkthrough of the app.](/bikeshare-to-app/walkthrough.md)

## A Note on Data

The decision to limit data used in the application to 2022 was made based on the availability of information on the location of stations in the network, which at the outset of the project was available for early 2023. As station ID #s were not linked one-to-one with a given station location throughout the history of the service (see an exploration of this phenomenon [here](/Data%20Exploration/Station_ID_Duplication.ipynb)), identifying locations becomes particularly difficult without contemporary station information. Much more information on the process of identifying station locations from 2022 is given in a Jupyter Notebook [here](/Data%20Processing/Station_ID_Identification.ipynb) along with associated code. The preprocessing pipeline for the usage data can be found a Jupyter Notebook [here](/Data%20Processing/data_loading.ipynb). The scripts to prepare the spatial data on stations and usage for plotting in the app are [here](/Data%20Processing/station_data_2022_compilation.R) and [here](/Data%20Processing/ride_data_2022_sf.R).

Ideally, spatial data on system usage prior to 2022 can be mapped (to some extent) in the future by identifying station locations using similar approaches to those used for a subeset of stations in the scripts above. This would be particuarly interesting for visualizing the effects of the COVID-19 pandemic on system usage spatio-temporally.

The datasets from the City of Toronto's Open Data Portal used in the web app are the [Bike Share Toronto Ridership Data](https://open.toronto.ca/dataset/bike-share-toronto-ridership-data/) and [Intersection File - City of Toronto](https://open.toronto.ca/dataset/intersection-file-city-of-toronto/).

## Built With

* [R 4.1](https://www.r-project.org/)
* [sf](https://r-spatial.github.io/sf/)
* [Jupyter Lab](https://jupyter.org/)
* [Shiny](https://shiny.posit.co/)
* [bslib](https://rstudio.github.io/bslib/index.html)
* [Arrow](https://arrow.apache.org/docs/r/index.html)
* [Leaflet](https://leafletjs.com/)
* [Leaflet Extras](https://trafficonese.github.io/leaflet.extras/)
* [Data Tables](https://datatables.net/)
* [ggplot2](https://ggplot2.tidyverse.org/)
* [ggiraph](http://davidgohel.github.io/ggiraph/)

_Note_: In my experience, the _renderLeaflet()_ function in Shiny did not play nicely with R 4.3 at the outset of this project. The reason appears related to the change in the handling of the _&&_ operator with vectors rather than scalars in R as of 4.3. This change is covered [here](https://www.jumpingrivers.com/blog/whats-new-r43/). It is for this reason that a legacy version of R is used instead.

## Roadmap

- [X] Pane 1: Trip Frequency by Date w/ Heatmap
- [X] Pane 2: Most Popular Starting and Ending Stations
- [X] Pane 3: Station Departure-Arrival Ratio (Flows)
- [ ] Additional Panes (Working Concepts)
  - [ ] Trip Frequency by User Type
  - [ ] Distribution of Trips by Time of Day
  - [ ] Relationship Between Station Use and TTC Station Proximity
- [ ] Add Interpretive Analysis to Panes

See [Open Issues](https://github.com/cmkimber/bikeshare-to/issues) for more detail on proposed features (and known issues).

## Acknowledgements

* [City of Toronto Open Data Portal](https://open.toronto.ca/)
* [City of Toronto Open Data License](https://www.toronto.ca/city-government/data-research-maps/open-data/open-data-licence/)
* [conda-forge](https://www.conda-forge.org)
* [Mastering Shiny](https://mastering-shiny.org/index.html)
* [ggiraph-book](https://www.ardata.fr/ggiraph-book/)
* [Apache Arrow R Cookbook](https://arrow.apache.org/cookbook/r/index.html)

[Amanda Xuereb](https://www.linkedin.com/in/amanda-xuereb-4a75139a/) kindly provided feedback on the layout and content of the application.
