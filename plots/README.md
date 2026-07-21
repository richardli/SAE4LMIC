# Plotting codes 
This folder contains
1. report_plot_country and report_table_country: call helper functions and loop over selected countries and indicators.
2. report_helper: all plotting functions in one
3. report_map.R: savemaps_ridge()
	- MAPs for 1)prevalence, 2)length of credible interval, and 3) exceedance, saved as "ad2_map_", "ad1_map_".R
	- Ridge plots, saved as "ridge_".
4. report_clustermap.R: saveclustermap()
	- admin 1 and admin 2 MAPs number clusters, samples, and events,  saved as"Basic-yr1", "Basic-yr2"
5. report_interval.R: saveinterval_overlay()
	- admin 1 and admin 2 interval plots, saved as "interval-ad2-", "interval-ad1-"
	- overlay plots, saved as "overlay-"
6. report_scatter.R: savescatter()
	- admin 2 scatter plot: "scatter-"
7. report_one.R: single entry point that loads the split saver functions above
   (only their function definitions, skipping the test calls) and wraps them
   behind `report_one_indicator()` / `report_country()`. Holds the prefix presets
   (`prefix_set_core` = plain names; `prefix_set_newvarfix` = `new_` names) and
   `prefix_set_for_country()`. Sources `misc/resolve_qs.R` so the savers resolve
   the plain vs `new_` prefix per survey automatically. `report_one_indicator()`
   (and `save_tab1/3/456`) accept an optional `out_sub` = year-pair subfolder;
   when set, tables are written under `estimates/<country>/<out_sub>/` instead of
   the flat `estimates/<country>/`.

## `report_year_pair()` — plots for a chosen old/new year pair (`report_years.R`)

`report_years.R` adds one driver on top of `report_one.R` that renders **all**
plots (and, with `with_tables = TRUE`, the `National*` tables) for a selected
(old, new) survey-year pair into **year-named subfolders**, so multiple
comparisons coexist and previous outputs are never deleted:

    plots  ->  Gates-results/ReportPlots/<country>/<yr1>-<yr2>/
    tables ->  Gates-results/estimates/<country>/<yr1>-<yr2>/

### Usage

```r
source("report_years.R")

report_year_pair("Malawi", 2015, 2024)                         # all indicators
report_year_pair("Malawi", 2015, 2024, indicators = "CH_VACC_C_MSL")  # one indicator
report_year_pair("Ethiopia", 2019, 2024)                       # mixed new_/plain, auto
report_year_pair("Malawi")                                     # default = min/max years
```

Plots go to `Gates-results/ReportPlots/<country>/<yr1>-<yr2>/`; tables (when
`with_tables = TRUE`) go to `Gates-results/estimates/<country>/<yr1>-<yr2>/`.

### Arguments

- `country` — must be in `info/surveyslist.csv`.
- `yr1`, `yr2` — old / new survey years; default = min / max for the country.
  **Any pair works, not just min/max**: the driver temporarily restricts the
  global `surveys` to those two years so the savers' internal `min()/max()`
  resolves to your pair, then restores it.
- `prefix_set` — **optional**. Prefixes are auto-resolved per survey folder by
  `resolve_qs` (plain vs `new_`), so a mixed old/new pair (e.g. Ethiopia 2019 =
  `new_`, 2024 = plain) reads correctly without setting it. Pass one only to pin
  which model output is read (see the presets in `report_one.R`).
- `indicators` — which indicators (default: all in `infolist.csv`).
- `with_plots` — any of `"maps_ridge"`, `"cluster"`, `"interval_overlay"`,
  `"scatter"` (default: all).
- `with_tables` — also (re)write the `National*` tables once, into
  `estimates/<country>/<yr1>-<yr2>/` (default `FALSE`).

Existing outputs are never deleted; each pair writes into its own `<yr1>-<yr2>`
subfolder — plots under `ReportPlots/<country>/`, tables under
`estimates/<country>/`.
