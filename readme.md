# Bike Share Toronto Data Analysis
## About This Project

This repository is to contain a series of reports and associated code for an analysis of usage data from Bike Share Toronto, a publically owned bike sharing service in Toronto, ON, Canada. The data is provided freely by the City of Toronto on their [Open Data Portal](https://open.toronto.ca/). This analysis is in no way associated with the City of Toronto or Bike Share Toronto.

The aims of these reports are twofold:
- To investigate what publically available data can tell the public about how the City of Toronto’s bike sharing service was used before, during and after the COVID-19 pandemic and to consider how the service might best be managed going forward.
- To explore the use of different software tools to analyze and visualize this rich temporal and geospatial dataset and to create readable, interpretable reports. 

## Limitations to Public Data

There are inherent limitations to working with the public data provided by the City of Toronto. First and foremost, there is no data on individual users available, even in anonymized form. Secondly, financial data is not associated with the dataset at all. Thirdly, the City of Toronto only provides data on bike share stations at the present time, rather than across all years of data available. This can limit or complicate the interpretation of historical spatial data. Nevertheless, there is still a lot of interesting potential in this dataset to explore and to use as a vehicle for testing data. 

## Contents

- Data Processing: scripts for ingesting and preprocessing data from the City of Toronto. At this point, all data importing was done in R/tidyverse.
- Report 1: Ridership statistics at a yearly/monthly scale from 2017-2022. Generated using R with tidyverse and ggplot2 in Jupyter Lab.

## The Authors

At the moment, I (Chris) am responsible for the code and the report writing. Amanda Xuereb (LINK?) and Andy Wong have been collaborative brainstorming and editing partners and may well contribute coding in future reports.
