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

## Instructions for contributors
1. Clone this repository and organize your local project folder as above.
2. 

## Steps to carry out analysis
1. Make sure the local folder structure is set up the same as described above. 
2. Run ... to fit model for a country or indicator
3. Run ... to generate figures for report
4. Run ... to generate PDF report
5. Run ... to generate plots for the website

## Administrator tasks

### Steps when adding new indicators
1. Update `info/infolist.csv`
2. Run `models/prep_wd.R` to create the directories
3. ...

### Steps when adding new surveys
1. Update `info/surveylist.csv` and `info/shapefilelist.csv`
2. Run `models/prep_wd.R` to create the directories
3. ...


## Detailed folder structures

TODO: explain within the folders how things are organized