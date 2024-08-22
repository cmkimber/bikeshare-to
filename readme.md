# Bike Share Toronto Data Analysis
## About This Project

This repository is to contain a series of reports and associated code for an analysis of usage data from Bike Share Toronto, a publicly owned bike sharing service in Toronto, ON, Canada. The data is provided freely by the City of Toronto on their [Open Data Portal](https://open.toronto.ca/). This analysis is in no way associated with the City of Toronto or Bike Share Toronto.

The aims of these reports are twofold:
- To investigate what publicly available data can tell the public about how the City of Toronto’s bike sharing service was used before, during and after the COVID-19 pandemic and to consider how the service might best be managed going forward.
- To explore the use of different software tools to analyze and visualize this rich temporal and geospatial dataset and to create readable, interpretable reports.

The intention is for this series of reports to be published as blog posts on LinkedIn, but they are also readable in this repo along with the underlying code.

In addition, there is an interactive web application in development that allows for exploration of a subset of the data used in the reports (from 2022). The application focuses on spatial aspects of the service in particular. At the moment, the web application is unfortunately not hosted online, as it is too memory-intensive for a free tier shinyapps.io instance and requires packages which are not available in shinylive.io. I am working on a solution for hosting currently. [Find the current web app code within this repo here](/bikeshare-to-app) and [view an animated walkthrough of the app here](/bikeshare-to-app/walkthrough.md).

## Contents

- [Data Processing](/Data%20Processing): scripts for ingesting and preprocessing data from the City of Toronto. At this point, all data importing was done in R/tidyverse.
- [Data Exploration](/Data%20Exploration): scripts for identifying features of the data or patterns in the results to guide report and web application development.
- [Report 1](/Report%201/readme.md): Ridership statistics at a yearly/monthly scale from 2017-2022. Generated using R with tidyverse and ggplot2 in Jupyter Lab.
- [Web Application (bikeshare-to-app)](/bikeshare-to-app): Ridership data from 2022 with spatial information on stations used during the rides, presented in a series of interactive graphics. Built using R/tidyverse and Shiny in RStudio, using a series of other tools delineated in the app readme. **Currently WIP**.

## Limitations to Public Data

There are inherent limitations to working with the public data provided by the City of Toronto. First and foremost, there is no data on individual users available, even in anonymized form. Secondly, financial data is not associated with the dataset at all. Thirdly, the City of Toronto only provides data on bike share stations at the present time, rather than across all years of data available. This can limit or complicate the interpretation of historical spatial data. Nevertheless, there is still a lot of interesting potential in this dataset to explore and to use as a vehicle for testing data analysis tools.

## The Authors

At the moment, I (Chris) am responsible for the code and the report writing. [Amanda Xuereb](https://www.linkedin.com/in/amanda-xuereb-4a75139a/) and Andy Wong have been collaborative brainstorming and editing partners and may well contribute coding in future reports.
