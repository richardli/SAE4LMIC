# =============================================================================
# plots/report_years.R
# -----------------------------------------------------------------------------
# Make ALL report plots for a chosen (old, new) survey-year pair, written to a
# year-named subfolder so multiple comparisons coexist and old plots are kept:
#
#     Gates-results/ReportPlots/<country>/<yr1>-<yr2>/
#
# Builds on plots/report_one.R (sourced for its function loaders, prefix presets
# and per-indicator driver). It does NOT modify the plotting functions or
# report_one.R.
#
# How arbitrary year selection works
#   The split plotting functions (savemaps_ridge, saveinterval_overlay,
#   savescatter, saveclustermap) compute yr1/yr2 internally as
#   min()/max(surveys$year) for the country. To select any pair, this driver
#   temporarily restricts the global `surveys` to just the two chosen years for
#   that country while the savers run, then restores it (on.exit). For a
#   2-survey country like Malawi this is a no-op (min/max already == the pair).
#
# Prefix per survey (automatic)
#   The savers now read results through misc/resolve_qs.R (sourced by
#   report_one.R), which resolves the plain vs new_ prefix INDEPENDENTLY per
#   survey folder. So a mixed old/new pair (e.g. Ethiopia 2019 = new_ +
#   2024 = plain) reads correctly regardless of the `prefix_set` passed:
#   resolve_qs strips a leading "new_" from the base name and falls back
#   automatically (prefers plain). `prefix_set` now only supplies the base
#   names; the per-country default is fine, including for mixed pairs.
# =============================================================================

library(here)
git_path <- here::here()

# Load the savers + helpers + prefix presets (report_one.R sources the split
# files and defines report_one_indicator / prefix_set_* / default paths).
source(file.path(git_path, "plots", "report_one.R"))

# ---------------------------------------------------------------------------
# report_year_pair()
#
#   country      chr, must be in surveyslist.csv$country
#   yr1, yr2     old / new survey years; default = min / max for the country
#   prefix_set   named list of the 5 prefixes (see report_one.R presets);
#                default = prefix_set_for_country(country)
#   indicators   which indicators (default all)
#   with_plots   which families: any of
#                c("maps_ridge","cluster","interval_overlay","scatter")
#   with_tables  also (re)write National*.csv once (default FALSE for a plot run)
#   dpi          passed to the savers
#   out_root     ReportPlots root under source_path
#
# Returns (invisibly) the output directory. Existing plots are never deleted.
# ---------------------------------------------------------------------------
report_year_pair <- function(country,
                             yr1 = NULL, yr2 = NULL,
                             prefix_set  = NULL,
                             indicators  = infolist$ID,
                             with_plots  = c("maps_ridge", "cluster",
                                             "interval_overlay", "scatter"),
                             with_tables = FALSE,
                             dpi         = 150,
                             out_root    = "Gates-results/ReportPlots") {

  stopifnot(country %in% surveys$country)
  yrs_all <- sort(unique(surveys$year[surveys$country == country]))
  if (is.null(yr1)) yr1 <- min(yrs_all)
  if (is.null(yr2)) yr2 <- max(yrs_all)
  if (!all(c(yr1, yr2) %in% yrs_all))
    stop("yr1/yr2 must be survey years for ", country, " (have: ",
         paste(yrs_all, collapse = ", "), ")")
  if (yr1 == yr2) stop("yr1 and yr2 must differ")
  if (yr1 > yr2) { tmp <- yr1; yr1 <- yr2; yr2 <- tmp }   # old < new

  if (is.null(prefix_set)) prefix_set <- prefix_set_for_country(country)

  plot_path_c <- file.path(source_path, out_root, country, paste0(yr1, "-", yr2))
  dir.create(plot_path_c, recursive = TRUE, showWarnings = FALSE)
  message("[report_year_pair] ", country, ": ", yr1, " vs ", yr2,
          "  ->  ", plot_path_c)

  # --- temporarily restrict global `surveys` so the savers' internal
  #     min()/max() resolve to (yr1, yr2) for this country; restore on exit. ---
  surveys_full <- get("surveys", envir = globalenv())
  keep <- surveys_full$country != country |
          (surveys_full$country == country & surveys_full$year %in% c(yr1, yr2))
  assign("surveys", surveys_full[keep, , drop = FALSE], envir = globalenv())
  on.exit(assign("surveys", surveys_full, envir = globalenv()), add = TRUE)

  # --- render each indicator into the year folder (tables at most once) ---
  for (k in seq_along(indicators)) {
    report_one_indicator(country     = country,
                         indicator   = indicators[k],
                         prefix_set  = prefix_set,
                         plot_path_c = plot_path_c,
                         out_sub     = paste0(yr1, "-", yr2),
                         with_tables = isTRUE(with_tables) && k == 1L,
                         with_plots  = with_plots,
                         dpi         = dpi)
  }

  invisible(plot_path_c)
}

# ============================================================================
#  Example usage (NOT run on source; uncomment to invoke)
#
#  `prefix_set` is optional: prefixes are auto-resolved per survey folder by
#  resolve_qs (plain vs new_), so you normally omit it and the per-country
#  default is used. Pass one only to pin which model output is read.
# ============================================================================

# # --- Malawi smoke test: one indicator, 2015 vs 2024 ---
# report_year_pair("Malawi", 2015, 2024, indicators = "CH_VACC_C_MSL")
#
# # --- Malawi, all indicators, into ReportPlots/Malawi/2015-2024/ ---
report_year_pair("Malawi", 2015, 2024)

report_year_pair("Malawi", 2015, 2024,
                 with_plots  = c(
                   "interval_overlay"),
                 with_tables = T)

# # --- default years (min/max) for the country ---
# report_year_pair("Malawi")
#
# --- mixed old/new prefixes handled automatically ---
#     Ethiopia 2019 = new_ , 2024 = plain  -> each resolved independently
report_year_pair("Ethiopia", 2019, 2024)
#
# # --- pick a non-min/max pair from a 3+ survey country ---
# report_year_pair("Ethiopia", yr1 = 2016, yr2 = 2024)
