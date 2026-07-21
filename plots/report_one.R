# =============================================================================
# plots/report_one.R
# -----------------------------------------------------------------------------
# Step A of the plotting cleanup. ONE entry point that produces every plot and
# table consumed by report/report.Rmd for a single (country, indicator) pair.
#
# Why this exists
#   The canonical plot/table functions live in five split files
#     plots/report_map.R         -> savemaps_ridge()      ad1_map_, ad2_map_, ridge_
#     plots/report_interval.R    -> saveinterval_overlay() overlay-, interval-ad1-, interval-ad2-
#     plots/report_scatter.R     -> savescatter()          scatter-
#     plots/report_clustermap.R  -> saveclustermap()       Basic-<ind>-<yr>
#     plots/report_table.R       -> save_tab1/3/456()      National.csv, National3.csv, National_*.csv
#   Each split file already accepts a `indicatorlist` argument, so a single
#   indicator is achievable by passing `indicatorlist = indicator`. This file
#   wraps that uniformly behind one call, hides the prefix bookkeeping behind
#   a `prefix_set`, and adds convenience loops over indicators / countries.
#
# How the split files are sourced
#   The split files contain top-level test calls (e.g. savescatter("Nigeria",
#   ...)) at the bottom and in between function defs. A naive source() would
#   trigger a full re-render of every country. To avoid editing the split
#   files, source_functions_only() below parses each file and evaluates only
#   `<-`/`=` assignments whose RHS is a function literal, plus library()/
#   require() calls. Everything else is silently skipped.
# =============================================================================

# ---------------------------- environment setup ------------------------------

library(here)
library(qs)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(readr)
library(sf)
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(scales)
library(forcats)
library(rmapshaper)
library(surveyPrev)
library(SUMMER)

# These match what the split files assume in the global env.
source_path <- dirname(here::here())
git_path    <- here::here()

# Prefix-agnostic result reader used by the savers (resolves plain vs new_ per
# survey folder), so an old/new year pair with mixed prefixes reads correctly.
source(file.path(git_path, "misc", "resolve_qs.R"))

infolist   <- read.csv(file.path(git_path, "info", "infolist.csv"))
surveys    <- read.csv(file.path(git_path, "info", "surveyslist.csv"))
shapeused  <- read.csv(file.path(git_path, "info", "shapefileList.csv"))

# ----------------------- selective source of split files ---------------------

# Evaluate only top-level function definitions and library/require calls from
# `path`. Test/driver lines at top level are skipped.
source_functions_only <- function(path, envir = globalenv()) {
  exprs <- parse(file = path)
  for (e in exprs) {
    keep <- FALSE
    if (is.call(e)) {
      head <- as.character(e[[1]])
      # x <- function(...) {...}  /  x = function(...) {...}  /  assign("x", function(...) ...)
      # also chained: a = b <- function(...)  (unwrap nested assignments)
      if (head %in% c("<-", "=", "assign") && length(e) >= 3) {
        rhs <- e[[3]]
        while (is.call(rhs) && as.character(rhs[[1]])[1] %in% c("<-", "=") &&
               length(rhs) >= 3) rhs <- rhs[[3]]
        if (is.call(rhs) && identical(rhs[[1]], as.name("function"))) keep <- TRUE
      }
      if (head %in% c("library", "require", "requireNamespace")) keep <- TRUE
    }
    if (keep) eval(e, envir = envir)
  }
  invisible(NULL)
}

split_files <- c(
  "report_map.R",        # save_tab2_from_files, has_data, make_placeholder,
                         # ridgePlot1, exceedPlot1, exceedplot2, exceedplot3,
                         # savemaps_ridge
  "report_interval.R",   # saveinterval_overlay (+ helpers)
  "report_scatter.R",    # scatterPlot1, savescatter
  "report_clustermap.R", # sample_info_map_static, saveclustermap
  "report_table.R"       # save_tab1, save_tab3, save_tab456
)
for (f in split_files) {
  source_functions_only(file.path(git_path, "plots", f))
}

# --------------------------- prefix-set presets ------------------------------

# `prefix_set` packs the five model-output prefixes the plotters need into one
# named list. There are currently two regimes:
#   * core      -- from model-core.R           (no "new_" prefix)
#                  used by Malawi today.
#   * newvarfix -- from model-core-newvarfix.R ("new_" prefix)
#                  used by every other country.
# Slots:
#   adm0     national direct estimates       -> save_tab1, save_tab456,
#                                               threshold lookup for maps
#   adm1     admin-1 direct estimates        -> ad1 map, interval-ad1, overlay,
#                                               ridge
#   adm2_dir admin-2 direct (no var.fix)      -> currently unused by report.Rmd
#   adm2_fix admin-2 direct (var.fix = TRUE)  -> save_tab3, scatter left axis
#   adm2_fh  admin-2 smoothed (FH+BYM2)       -> ad2 map, interval-ad2, overlay,
#                                               scatter right axis

prefix_set_core <- function() list(
  adm0     = "res_adm0-",
  adm1     = "res_adm1-",
  adm2_dir = "res_adm2-",
  adm2_fix = "res_adm2_fix-",
  adm2_fh  = "FH_adm2_fix_nest-"
)

prefix_set_newvarfix <- function() list(
  adm0     = "new_res_adm0-",
  adm1     = "new_res_adm1-",
  adm2_dir = "new_res_adm2-",
  adm2_fix = "new_res_adm2_fix-",
  adm2_fh  = "new_FH_adm2_fix_nest-"
)

# Pick the right preset for a country. Override the lookup as more regimes
# appear; this keeps callers from memorising which country uses which prefix.
prefix_set_for_country <- function(country) {
  if (country == "Malawi") prefix_set_core() else prefix_set_newvarfix()
}

# ----------------------------- output paths ----------------------------------

default_plot_path_c <- function(country, middle_path_plots = "Gates-results/ReportPlots") {
  file.path(source_path, middle_path_plots, country)
}

default_out_middle <- "Gates-results/estimates"
default_middle_path <- "Gates-results/Results"

# ============================================================================
#  report_one_indicator()
#  -----------------------------------------------------------------------------
#  Produce every artifact report.Rmd reads for ONE indicator in ONE country.
#
#  Args:
#    country       - chr. Must appear in info/surveyslist.csv$country.
#    indicator     - chr. Must appear in info/infolist.csv$ID.
#    prefix_set    - named list of the five prefixes (see presets above).
#                    Default: prefix_set_for_country(country).
#    middle_path   - results dir for .qs inputs.
#    plot_path_c   - dir to write PNGs into.
#    out_middle    - dir to write CSV tables into.
#    with_tables   - if TRUE (default), also (re)write National.csv,
#                    National3.csv, National_clusters/events/samples.csv.
#                    These are inherently country-wide (one row per indicator),
#                    so they reflect ALL indicators currently on disk, not
#                    just `indicator`.
#    with_plots    - which plot families to render. Any subset of
#                    c("maps_ridge", "cluster", "interval_overlay", "scatter").
#    dpi           - passed through to the savers.
#
#  Returns: invisible character vector of files written (best effort).
# ============================================================================
report_one_indicator <- function(country,
                                 indicator,
                                 prefix_set  = NULL,
                                 middle_path = default_middle_path,
                                 plot_path_c = default_plot_path_c(country),
                                 out_middle  = default_out_middle,
                                 out_sub     = NULL,
                                 with_tables = TRUE,
                                 with_plots  = c("maps_ridge", "cluster",
                                                 "interval_overlay", "scatter"),
                                 dpi         = 150) {

  stopifnot(country %in% surveys$country)
  stopifnot(indicator %in% infolist$ID)
  if (is.null(prefix_set)) prefix_set <- prefix_set_for_country(country)

  if (!dir.exists(plot_path_c))
    dir.create(plot_path_c, recursive = TRUE, showWarnings = FALSE)
  out_dir_csv <- file.path(source_path, out_middle, country)
  if (!is.null(out_sub)) out_dir_csv <- file.path(out_dir_csv, out_sub)
  if (with_tables && !dir.exists(out_dir_csv))
    dir.create(out_dir_csv, recursive = TRUE, showWarnings = FALSE)

  message("[report_one_indicator] ", country, " | ", indicator)

  # ---- tables (country-wide; cheap to regenerate) --------------------------
  if (with_tables) {
    try(save_tab1(
      country     = country,
      adm_name    = prefix_set$adm0,
      ids         = infolist$ID,
      middle_path = middle_path,
      out_middle  = out_middle,
      out_sub     = out_sub
    ), silent = FALSE)

    try(save_tab3(
      country     = country,
      ad2_name    = prefix_set$adm2_fix,
      ids         = infolist$ID,
      middle_path = middle_path,
      out_middle  = out_middle,
      out_sub     = out_sub
    ), silent = FALSE)

    try(save_tab456(
      country     = country,
      adm_name    = prefix_set$adm0,
      ids         = infolist$ID,
      middle_path = middle_path,
      out_middle  = out_middle,
      out_sub     = out_sub
    ), silent = FALSE)
  }

  # ---- plots (per indicator) ----------------------------------------------
  ind <- indicator  # explicit short alias used in the savers' subset arg

  if ("maps_ridge" %in% with_plots) {
    # writes ad1_map_<ind>.png, ad2_map_<ind>.png, ridge_<ind>.png
    try(savemaps_ridge(
      country       = country,
      ad2_name      = prefix_set$adm2_fh,
      ad1_name      = prefix_set$adm1,
      adm_name      = prefix_set$adm0,
      indicatorlist = ind,
      middle_path   = middle_path,
      plot_path_c   = plot_path_c,
      dpi           = dpi
    ), silent = FALSE)
  }

  if ("cluster" %in% with_plots) {
    # writes Basic-<ind>-<yr1>.png, Basic-<ind>-<yr2>.png
    try(saveclustermap(
      country       = country,
      middle_path   = middle_path,
      plot_path_c   = plot_path_c,
      indicatorlist = ind
    ), silent = FALSE)
  }

  if ("interval_overlay" %in% with_plots) {
    # writes overlay-<ind>.png, interval-ad1-<ind>.png, interval-ad2-<ind>.png
    try(saveinterval_overlay(
      country       = country,
      ad1_name      = prefix_set$adm1,
      ad2_name      = prefix_set$adm2_fh,
      indicatorlist = ind,
      middle_path   = middle_path,
      plot_path_c   = plot_path_c,
      dpi           = dpi
    ), silent = FALSE)
  }

  if ("scatter" %in% with_plots) {
    # writes scatter-<ind>.png
    try(savescatter(
      country       = country,
      ad2_name      = prefix_set$adm2_fh,
      ad2_name_dir  = prefix_set$adm2_fix,
      middle_path   = middle_path,
      plot_path_c   = plot_path_c,
      indicatorlist = ind,
      dpi           = dpi
    ), silent = FALSE)
  }

  # Best-effort list of expected outputs (existence not guaranteed if a step
  # was skipped or the model result file was missing).
  yrs <- sort(surveys$year[surveys$country == country])
  expected_png <- c(
    file.path(plot_path_c, paste0("ad1_map_",      ind, ".png")),
    file.path(plot_path_c, paste0("ad2_map_",      ind, ".png")),
    file.path(plot_path_c, paste0("ridge_",        ind, ".png")),
    file.path(plot_path_c, paste0("overlay-",      ind, ".png")),
    file.path(plot_path_c, paste0("interval-ad1-", ind, ".png")),
    file.path(plot_path_c, paste0("interval-ad2-", ind, ".png")),
    file.path(plot_path_c, paste0("scatter-",      ind, ".png")),
    file.path(plot_path_c, paste0("Basic-", ind, "-", yrs, ".png"))
  )
  expected_csv <- if (with_tables) file.path(out_dir_csv,
    c("National.csv", "National3.csv",
      "National_clusters.csv", "National_events.csv", "National_samples.csv")
  ) else character()

  invisible(c(expected_png, expected_csv))
}

# ============================================================================
#  Convenience loops on top of report_one_indicator
# ============================================================================

# All indicators (or a subset) for one country.
report_country <- function(country,
                           indicators  = infolist$ID,
                           prefix_set  = NULL,
                           middle_path = default_middle_path,
                           plot_path_c = default_plot_path_c(country),
                           out_middle  = default_out_middle,
                           with_plots  = c("maps_ridge", "cluster",
                                           "interval_overlay", "scatter"),
                           dpi         = 150) {

  if (is.null(prefix_set)) prefix_set <- prefix_set_for_country(country)

  # Tables are country-wide; run once up front, then disable inside the loop.
  report_one_indicator(country     = country,
                       indicator   = indicators[1],
                       prefix_set  = prefix_set,
                       middle_path = middle_path,
                       plot_path_c = plot_path_c,
                       out_middle  = out_middle,
                       with_tables = TRUE,
                       with_plots  = with_plots,
                       dpi         = dpi)

  for (ind in indicators[-1]) {
    report_one_indicator(country     = country,
                         indicator   = ind,
                         prefix_set  = prefix_set,
                         middle_path = middle_path,
                         plot_path_c = plot_path_c,
                         out_middle  = out_middle,
                         with_tables = FALSE,
                         with_plots  = with_plots,
                         dpi         = dpi)
  }
  invisible(NULL)
}

# One indicator across many countries. Each country gets its own prefix_set
# from prefix_set_for_country() unless `prefix_overrides[[country]]` is set.
report_indicator_all_countries <- function(indicator,
                                           countries        = unique(surveys$country),
                                           prefix_overrides = list(),
                                           middle_path = default_middle_path,
                                           out_middle  = default_out_middle,
                                           with_plots  = c("maps_ridge", "cluster",
                                                           "interval_overlay", "scatter"),
                                           with_tables = TRUE,
                                           dpi         = 150) {
  for (cty in countries) {
    ps <- if (!is.null(prefix_overrides[[cty]])) prefix_overrides[[cty]]
          else prefix_set_for_country(cty)
    report_one_indicator(country     = cty,
                         indicator   = indicator,
                         prefix_set  = ps,
                         middle_path = middle_path,
                         plot_path_c = default_plot_path_c(cty),
                         out_middle  = out_middle,
                         with_tables = with_tables,
                         with_plots  = with_plots,
                         dpi         = dpi)
  }
  invisible(NULL)
}

# ============================================================================
#  Example usage (intentionally NOT run on source; uncomment to invoke)
# ============================================================================

# # Single indicator, single country (the canonical Step A target).
# report_one_indicator(
#   country   = "Burkina Faso",
#   indicator = "CH_VACC_C_MSL"
# )
#
# # Single indicator, single country, Malawi regime (auto-picked).
# report_one_indicator(
#   country   = "Malawi",
#   indicator = "CH_VACC_C_MSL"
# )
#
# # All indicators for one country.
# report_country(country = "Burkina Faso")
#
# # One indicator across every country in surveyslist.csv.
# report_indicator_all_countries(indicator = "CH_VACC_C_MSL")
