# Modeling codes 
This folder contains codes to run all models for each country. 

Step 1: run the script `model-setup.R`. Fix any missing dependence issues.

```
source("model-setup.R")
```

Step 2. Run a country-specific model script, e.g., for Nigeria

```
source("model-Nigeria.R")
```

The script calls `model-core.R` repeatedly for all indicators. If there are errors, please do not edit `model-core.R` directly. Please try to fix the errors with minimal changes to the steps in `model-core.R` and include the changes **in the country-specific** file (e.g., `model-Nigeria.R`). An administrator will merge the country-specific changes to the core model fitting script after examination. 

Please read carefully the printed log and summarize which indicators were not processed.


#### Administrator update
For mass update when all country-specific scripts are taken care of, run 
```
source("model-loop.R")
```
to get a fresh copy of all model results.