# Repository & Data preparation
This folder contains codes for setting up the repository. It only needs to be run by the lead administrator.

To run all prep scripts from scratch:

```
source("prepare_wd.R")
source("prepare_basic.R")
source("prepare_data.R")
```

To update only a subset of countries or indicators, you need to edit the loops inside the last two scripts.