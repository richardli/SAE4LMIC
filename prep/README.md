# Repository & Data preparation (`prep/`)

Code for setting up the repository and building the per-survey input data. Only
the lead administrator needs to run these. Everything writes into
`../Gates-results/` (paths are relative to the `GATES` root, i.e. `source_path`).

> **Before running anything:** make sure the GitHub development versions of
> `surveyPrev` and `SUMMER` are what R loads — see **Requirements** in the
> [top-level README](../README.md). 

## Files in this folder

| File | Run or source | What it does |
|------|---------------|--------------|
| `prepare_functions.R` | **sourced** (not run on its own) | Helper functions used by the other scripts: `detect_geoname()`, `get_dhs_filenames()`, `get_dhs_for_indicator()`, `savedata_one_indicator()`. |
| `prepare_wd.R` | run | Creates the folder structure for each survey: `Results/`, `estimates/`, `ReportPlots/`, `check/`, `ShinyPlots/` under `Gates-results/{country}/{year}`. |
| `prepare_basic.R` | run | **First loop:** builds `basic.Rdata` (`admin.info1`, `admin.info2`, `cluster.info`, `poly.adm1`, `poly.adm2`, `geo`) per survey and a boundary-check map in `Gates-results/check/{country}/{year}`. **End block:** updates `info/shapefileList.csv` (admin/cluster counts + boundary source). |
| `prepare_data.R` | run | Builds one `.qs` per indicator (`surveyPrev::getDHSindicator`), saved as `Gates-results/Results/{country}/{year}/{indicator}.qs`. |

## Order of running

Run **from scratch** (all surveys):

```r
source("prepare_wd.R")     # 1. make folders
source("prepare_basic.R")  # 2. basic.Rdata + shapefileList.csv
source("prepare_data.R")   # 3. per-indicator .qs
```

`prepare_functions.R` is sourced automatically inside 1–3; you never run it
directly. Modeling (`../models/`) comes after these.

To update only a subset of countries/surveys/indicators, edit the loop
selectors inside each script (see below) — do **not** rerun the whole thing.

## Adding a new survey — what to hand-edit

Complete these in order. The first four are one-time metadata/setup edits; the
last three are the scoped runs.

1. **Raw data** — put the DHS recode files and the geo shapefile under
   `Gates-data/rawDHS/{country}/{year}/` *(manual, outside this folder)*.
2. **`info/surveyslist.csv`** — add a row for the new `country, year`
   *(manual; see the top-level README metadata table)*.
3. **`prepare_functions.R` → `get_dhs_filenames()`** — add an
   `else if (country == "..." & year == ....)` branch giving the IR / KR / PR /
   BR recode filenames for the new survey. *(This is the DHS-filename map — note
   it lives here, not in `prepare_data.R`.)*
4. **`prepare_basic.R`, first loop** — point the loop at the new survey
   (e.g. `for (i in which(surveys$country == "Ethiopia" & surveys$year == 2024))`
   instead of a hard-coded index). If the country needs non-default boundaries,
   add a branch in the `if (country %in% ...)` block:
   - default → geoBoundaries (`get_geoBoundaries` + `addUpper`)
   - `Tanzania` → GADM; `Sierra Leone` → local WHO `.rds`
   - survey-specific geo fixes go here too (e.g. the Malawi 2024 "Dowa (Camps)"
     filter). Also confirm whether the survey needs `alt.strata = "v022"` at the
     modeling step (Rwanda / Tanzania-2015 / Mali-2023 / Sierra Leone) — that is
     set later in `../models/model-core.R`, but decide it now from the DHS report.
5. **`prepare_wd.R`** — set the selector to the new survey and run it:
   `countryList <- "Ethiopia"; yearList <- 2024` (Option B), or leave Option A
   (all) — it only creates directories, so it is safe either way.
6. **`prepare_basic.R`** — run the **first loop** (scoped as in step 4) to build
   `basic.Rdata`, then run the **incremental `Step 1.2` block at the end**
   (scoped via its `countryList`/`yearList`) to add the new survey's row to
   `shapefileList.csv`. Use the incremental block — the older full-rebuild block
   overwrites rows for countries whose `basic.Rdata` is not present locally.
7. **`prepare_data.R`** — set its loop to the new survey
   (`for (i in which(...))`) and, if desired, subset `indicatorlist`; run to
   produce the per-indicator `.qs` files.

After this, all DHS survey information and binary indicator files exist under
`Gates-results/Results/{country}/{year}/`, ready for modeling in `../models/`.

## Notes

- **Check the boundary map** saved in `Gates-results/check/{country}/{year}` —
  its title lists `wrong.points` (clusters without usable GPS/admin info).
  Confirm the admin-1 boundaries match the DHS report before modeling.
- **Subsetting:** every loop can be scoped with
  `which(surveys$country %in% countryList & surveys$year %in% yearList)`;
  `prepare_wd.R` and the incremental `prepare_basic.R` block already expose
  `countryList` / `yearList` selectors with an all-vs-one toggle.
- **DHS API data** (national/subnational) for API-consistency checks lives in
  `Gates-data/API/`; for surveys after summer 2025 it is computed with
  `../check/download_api.R`.
