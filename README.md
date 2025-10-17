# Code repository for SAE4LMIC Dashboard 

## Repository structure

This code repository assumes the following directory structures

- `Gates-data`
  - `rawDHS` Raw DHS recode data
  - `geoBoundaries` admin shapefiles
- `Gates-results`
  - `results` fitted model results
  - `reports` PDF reports on the website
  - `estimates` CSV of final estimates on the website
  - `ReportPlots` static plots for the report
  - `ShinyPlots` HTML and static plots to be passed to website 
- `SAE4LMIC` **(This repository)**
  - `reports` RMD files for generating reports
  - `models` R scripts to fit the final models on the website
  - `plots` R scripts to generate files in `../Gates-results/ReportPlots
  - `shinyplots` R scripts to generate files in `../Gates-results/ShinyPlots`
  - `misc` additional R scripts to diagnose, evaluate, or compare models


## Steps to carry out analysis
1. Make sure the local folder structure is set up the same as described above. 
2. Run ... to fit model for a country or indicator
3. Run ... to generate figures for report
4. Run ... to generate PDF report
5. Run ... to generate plots for the website



## Detailed folder structures

TODO: explain within the folders how things are organized