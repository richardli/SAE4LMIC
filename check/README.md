# Checks & validation (`check/`)

Diagnostic scripts that validate the prepared data and fitted models — mainly by
comparing our estimates against the published DHS API. Most are run **manually**
by the administrator; the one exception (`check-modelresult.R`) is sourced by the
model scripts. None produce website output.

The API-consistency rounds compare **national** estimates (`res.natl`) against
the DHS API national files in `Gates-data/API/admin0/`. Their outputs are written
to `Gates-results/check/{country}/{year}/` — note that is a *results* folder,
separate from this code folder of the same name.

## Files

| File | Run / sourced | Purpose |
|------|---------------|---------|
| `download_api.R` | run (manual) | Download DHS API national (`admin0`) and subnational (`admin1`) estimates via `rdhs`, into `Gates-data/API/`. Run this **first** for any survey after summer 2025 so the checks below have API data to compare against. |
| `checkapi_data.R` | run (manual) | **API check, round 1 (data).** Recomputes the national direct estimate from the prepared data (`surveyPrev::directEST(data, admin = 0)`) and compares to the API. |
| `checkapi_model.R` | run (manual) | **API check, round 2 (model).** Reads the saved national model estimate (`res_adm0-{indicator}.qs` → `res.natl`) and compares to the API. |
| `check-modelresult.R` | **sourced** by every `models/model-<country>.R` | `check_model_results()`, `get_hyperpars()`, `check_FH_summary_hyperpar()` — verify which result files exist per country/indicator and inspect FH / INLA hyperparameters. Runs automatically at the end of each model script. |
| `check-varfix.R` | run (manual) | Compare variance-fixed (`new_`) vs plain Admin-2 results to evaluate the var-fix method. |
| `fix_indicator.R` | run (manual) | `get_dhs_filenames()`, `get_dhs_for_indicator()`, `CHECK_API()` — **single-indicator** follow-up to Round 1. Rebuilds one indicator fresh from raw DHS (`getDHSindicator`) and returns `[surveyPrev estimate, DHS API value]` for eyeballing. (`get_dhs_filenames` duplicates the copy in `prep/prepare_functions.R`.) |

## Order of use (API consistency)

Each script can be scoped to a subset by editing the `countryList` at its top.

| Order | Script | Checks | Needs first | Outputs |
|-------|--------|--------|-------------|---------|
| **0. fetch** | `download_api.R` | — | DHS API credentials (`rdhs`) | `Gates-data/API/admin0/*.rda`, `Gates-data/API/admin1/*.qs` |
| **Round 1** | `checkapi_data.R` | prepared **data** (national direct est on the fly) | `prep/prepare_data.R` outputs (`.qs` + `basic.Rdata`) | `{country}Checkapi0.csv` + `{country}check-national.png`; aggregated into `all_checkapi0` |
| **Round 2** | `checkapi_model.R` | fitted **model results** (national) | `models/` fitted | `{country}Checkapi1.csv`; aggregated into `all_checkapi1`, read by `report/Indicator.Rmd` |



### Fixing a failed indicator

`Checkapi0` is the **before-model** check (on the prepared data); `Checkapi1` is the **after-model** check (on the fitted model results).

Pre-modelling national estimates check: 

1. `checkapi_data.R` produces `{country}Checkapi0.csv` and `{country}check-national.png` in `GATES/Gates-results/check/{country}/{year}/`. If the API value is NA (i.e., not downloaded via `download_api.R`), double-check against STATcompiler (https://www.statcompiler.com/en/) — some new indicators may not be in api.dhsprogram.com yet.
2. After updating the processing function in surveyPrev, you can use `fix_indicator.R` to re-check a particular indicator × survey combination.
3. `HC_WIXQ_P_12Q` is not in the API; its value should be around 40%.


After-model national estimates check:

1. `checkapi_model.R` reads each fitted national result (`new_res_adm0-{indicator}.qs` → `res.natl$direct.est`) and compares it to the DHS API national value, writing `{country}Checkapi1.csv` to `GATES/Gates-results/check/{country}/{year}/`. Each row holds `survey_name, country, year, indicator, surveyPrev, API`, plus `diff` (= `surveyPrev − API`) and `match` (= 1 when `|diff| < 0.01`).
2. Several indicators get special API handling built into the script: `HC_WIXQ_P_12Q` has no API value (set to NA); `RH_DELP_C_PRV` is summed from `RH_DELP_C_PNN` + `RH_DELP_C_PNG` for certain surveys; `CH_DIAT_C_ORT` uses the "Five years preceding the survey" figure; multi-value indicators pick the "Two years preceding the survey" row; percentages are rescaled to 0–1 (and `CM_ECMR_C_NNR` is divided by 10).
3. The script then pools every `Checkapi1.csv` into `all_checkapi1` and splits out `..._nodata` (both NA), `..._eyeballcheck` (hand-verified passes), `..._issue` (`match == 0`, excluding `HC_WIXQ_P_12Q`), and `..._issue_smallset` (issues minus eyeballed / no-data). The pooled table is saved as `Checkapi_ALL_after.csv`; the issue list feeds `report/Indicator.Rmd`.



NO 

