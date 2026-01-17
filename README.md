# Code repository for SAE4LMIC Dashboard 

This code repository contains codes needed to generate the input to the website at [https://sae4lmic.stat.uw.edu/](https://sae4lmic.stat.uw.edu/). 

To re-create the website locally, you will need the following
1. The data structure described in the next section. 
2. The codes in this repository to generate static and interactive plots, as well as reports. 
3. The codes in the [website repository](https://github.com/UW-Statistics/gatesweb) to deploy the website (locally or through Github Action)
4. Optionally, for websites with many large HTML files, you may need to store the source files in more than one repository to allow Github Action to properly build the website. See the [webplots folder](webplots/) for details.

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
  - `ShinyPlots1` First batch of HTML plots to be passed to website 
  - `ShinyPlots2` Second batch of HTML plots to be passed to website 
  - `StaticWebPlots` Static plots to be passed to website 
- `SAE4LMIC` **(This repository)**
  - `info` Meta data and list of surveys
  - `prep` R scripts to prepare datasets
  - `report` RMD files for generating reports
  - `models` R scripts to fit the final models on the website
  - `plots` R scripts to generate files in `../Gates-results/ReportPlots
  - `webplots` R scripts to generate files in `../Gates-results/ShinyPlots`
  - `misc` additional R scripts to diagnose, evaluate, or compare models


## Administrator tasks

### Initialization from scratch
1. Follow `prep/README.md` to set things up

### Generating outputs from fitted models

### Adding new indicators
1. Update `info/infolist.csv`
2. Follow `prep/README.md` to update the directory and binary data files
3. Check consistency with API using ...

### Adding new surveys
1. Update `info/surveylist.csv` and `info/shapefilelist.csv`
2. Follow `prep/README.md` to update the directory and binary data files
3. Check consistency in country information  
4. Check consistency with API 

 