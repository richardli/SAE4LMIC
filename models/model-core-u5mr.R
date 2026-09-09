#
# model-core-u5mr.R
#
# Fits national, Admin-1 and Admin-2 direct estimates plus the Admin-2 FH model
# for child-mortality indicators built by surveyPrev >= 2.0.0
# (CM_ECMR_C_U5F, CM_ECMR_C_U5M, CM_ECMR_C_IMF, CM_ECMR_C_IMR). These data are
# in person-month format with an `age` column, and surveyPrev switches to the
# discrete-hazard estimator (SUMMER::getDirect) when it sees that column.
#
# Why a separate core script (do NOT run these indicators through model-core.R
# or model-core-newvarfix.R):
#   1. directEST(var.fix = TRUE) reroutes to directEST_varfix(), which has no
#      mortality path and returns values ~50x too small. var.fix is never used here.
#   2. misc/directEST_1030.R and misc/fhModel_1030.R (used by -newvarfix) predate
#      mortality support entirely.
#   3. The mortality direct estimator ignores alt.strata: getDirect() stratifies
#      on the `strata` column (v025 urban/rural). For the alt-strata countries we
#      copy v022 into `strata` so the design matches the binary pipeline's intent.
#   4. Admin-2 FH: areas with a single cluster get a ~1e-18 variance, which makes
#      INLA fail ("Matrix is not positive definite") or collapse the whole fit.
#      Their clusters are dropped before fhModel(); the areas are then predicted
#      by the spatial model and recorded in $fixed_areas, mirroring what var.fix
#      records for binary indicators. Zero-death areas are already NA on the
#      logit scale and are handled by fhModel() without any dropping.
#   5. surveyPrev::datainfo() counts person-months. data.info is rebuilt here so
#      n_samples = births (rows with age == "0"), n_events = deaths.
#
# Output files are identical in name and shape to model-core.R (plain prefix):
#   res_adm0-, res_adm1-, res_adm2-, res_adm2_fix-, FH_adm2_fix_nest-,
#   summary-FH_adm2_fix_nest-<indicator>.qs
# res_adm2_fix- holds the same direct estimates as res_adm2- but with the
# variance/CI of the dropped areas set to NA and $fixed_areas filled in.
#
# Expects in the calling environment (set by model-setup.R and model-loop-u5mr.R):
#   source_path, git_path, surveys, country_to_run, indicator_to_run
# Surveys without basic.Rdata or <indicator>.qs are skipped with a message.
# Optional:
#   year_to_run        integer vector; default = all years of country_to_run
#   u5mr_min_clusters  areas with fewer clusters are dropped from the FH fit; default 2
#   u5mr_logit_var_min areas with direct.logit.var below this are also dropped; default 1e-8
#

library(dplyr)
library(qs)
library(surveyPrev)

if (!exists("u5mr_min_clusters"))  u5mr_min_clusters  <- 2
if (!exists("u5mr_logit_var_min")) u5mr_logit_var_min <- 1e-8

.u5mr_alt_strata <- function(country, year) {
  country == "Rwanda" | country == "Malawi" |
    (country == "Tanzania" && year == 2015) |
    (country == "Mali" && year == 2023) |
    (country == "Ethiopia" && year %in% c(2019, 2024)) |
    country == "Sierra Leone"
}

# births / deaths / clusters per area, same columns as surveyPrev::datainfo()
.u5mr_datainfo <- function(data, cluster.info, admin.info1, admin.info2) {
  ci <- cluster.info$data[, c("cluster", "admin1.name", "admin2.name.full")]
  ci <- sf::st_drop_geometry(ci)
  d  <- data %>% left_join(ci, by = "cluster") %>% filter(!is.na(admin2.name.full))
  ad2 <- d %>% group_by(admin2.name.full) %>%
    summarise(n_samples = sum(age == "0"), n_events = sum(value), n_clusters = n_distinct(cluster), .groups = "drop")
  ad2 <- admin.info2$data %>% select(admin2.name.full) %>% left_join(ad2, by = "admin2.name.full") %>%
    mutate(across(c(n_samples, n_events, n_clusters), ~ ifelse(is.na(.x), 0L, .x))) %>% as.data.frame()
  ad1 <- d %>% group_by(admin1.name) %>%
    summarise(n_samples = sum(age == "0"), n_events = sum(value), n_clusters = n_distinct(cluster), .groups = "drop")
  ad1 <- admin.info1$data %>% select(admin1.name) %>% left_join(ad1, by = "admin1.name") %>%
    mutate(across(c(n_samples, n_events, n_clusters), ~ ifelse(is.na(.x), 0L, .x))) %>% as.data.frame()
  ad0 <- data.frame(n_samples = sum(d$age == "0"), n_events = sum(d$value), n_clusters = n_distinct(d$cluster))
  list(summary.ad0 = ad0, summary.ad1 = ad1, summary.ad2 = ad2)
}

.u5mr_survey_rows <- function() {
  sel <- surveys$country %in% country_to_run
  if (exists("year_to_run") && !is.null(year_to_run)) sel <- sel & surveys$year %in% year_to_run
  which(sel)
}

## -------------------------------------------------------------------------##
## ------------------- Direct estimation -----------------------------------##
## -------------------------------------------------------------------------##
for (i in .u5mr_survey_rows()) {
  country       <- surveys$country[i]
  year          <- surveys$year[i]
  country_short <- surveys$country_short[i]
  survey_name   <- surveys$survey_name[i]
  message("Processing survey: ", survey_name, " (", country, " ", year, ")")

  results_path <- file.path(source_path, "Gates-results/Results", country, year)
  if (!file.exists(file.path(results_path, "basic.Rdata"))) { message("Missing basic.Rdata for ", survey_name, "; skipped"); next }
  load(file.path(results_path, "basic.Rdata"))

  for (indicator in indicator_to_run) {
    qfile <- paste0(file.path(results_path, indicator), ".qs")
    if (!file.exists(qfile)) { message("Missing ", indicator); next }

    data <- qread(qfile)
    if (!("age" %in% names(data))) {
      message(indicator, " has no `age` column: not a mortality dataset. Use model-core.R. Skipped.")
      next
    }
    if (.u5mr_alt_strata(country, year)) {
      data$strata <- factor(data$v022)
      message("alt strata: using v022 as design strata")
    }

    data.info <- .u5mr_datainfo(data, cluster.info, admin.info1, admin.info2)

    res_adm2 <- surveyPrev::directEST(
      data = data, cluster.info = cluster.info, admin = 2,
      admin.info = admin.info2, aggregation = FALSE, var.fix = FALSE, CI = 0.9)
    res_adm2$data.info <- data.info$summary.ad2

    res_adm1 <- surveyPrev::directEST(
      data = data, cluster.info = cluster.info, admin = 1,
      admin.info = admin.info1, aggregation = FALSE, var.fix = FALSE, CI = 0.9)
    res_adm1$data.info <- data.info$summary.ad1

    res_adm0 <- surveyPrev::directEST(
      data = data, cluster.info = cluster.info, admin = 0,
      admin.info = admin.info1, aggregation = FALSE, var.fix = FALSE, CI = 0.9)
    res_adm0$data.info <- data.info$summary.ad0

    # Areas whose direct variance is degenerate (single cluster, or ~0 logit var).
    # Zero-death areas are NOT included: they are NA on the logit scale already.
    r2 <- res_adm2$res.admin2 %>% left_join(data.info$summary.ad2, by = "admin2.name.full")
    degenerate <- with(r2, is.finite(direct.logit.est) &
                         (n_clusters < u5mr_min_clusters | direct.logit.var < u5mr_logit_var_min))
    fixed_areas <- r2$admin2.name.full[degenerate]
    message(length(fixed_areas), " Admin-2 areas with degenerate variance (single cluster); ",
            sum(!is.finite(r2$direct.logit.est)), " with zero deaths")

    res_adm2_fix <- res_adm2
    idx <- res_adm2_fix$res.admin2$admin2.name.full %in% fixed_areas
    res_adm2_fix$res.admin2[idx, c("direct.var", "direct.logit.var", "direct.logit.prec",
                                   "direct.se", "direct.lower", "direct.upper", "cv")] <- NA
    res_adm2_fix$fixed_areas <- fixed_areas

    for (nm in c("res_adm0", "res_adm1", "res_adm2_fix", "res_adm2")) {
      out <- file.path(results_path, paste0(sub("^res_", "res_", nm), "-", indicator, ".qs"))
      qs::qsave(get(nm), file = out)
      message("Saved: ", out)
    }
    message("done: ", indicator, " ", year)
  }
}

## -------------------------------------------------------------------------##
## ---------------------------- FH model -----------------------------------##
## -------------------------------------------------------------------------##
for (i in .u5mr_survey_rows()) {
  country       <- surveys$country[i]
  year          <- surveys$year[i]
  survey_name   <- surveys$survey_name[i]
  message("Processing survey: ", survey_name, " (", country, " ", year, ")")

  results_path <- file.path(source_path, "Gates-results/Results", country, year)
  if (!file.exists(file.path(results_path, "basic.Rdata"))) { message("Missing basic.Rdata for ", survey_name, "; skipped"); next }
  load(file.path(results_path, "basic.Rdata"))

  for (indicator in indicator_to_run) {
    qfile <- paste0(file.path(results_path, indicator), ".qs")
    if (!file.exists(qfile)) { message("Missing ", indicator); next }
    data <- qread(qfile)
    if (!("age" %in% names(data))) { message(indicator, " has no `age` column. Skipped."); next }
    if (.u5mr_alt_strata(country, year)) data$strata <- factor(data$v022)

    fixfile <- file.path(results_path, paste0("res_adm2_fix-", indicator, ".qs"))
    if (!file.exists(fixfile)) { message("res_adm2_fix missing for ", indicator, "; run the direct step first"); next }
    res_adm2_fix <- qread(fixfile)
    fixed_areas  <- res_adm2_fix$fixed_areas
    data.info2   <- res_adm2_fix$data.info

    drop_clusters <- cluster.info$data$cluster[cluster.info$data$admin2.name.full %in% fixed_areas]
    data_fh <- data[!(data$cluster %in% drop_clusters), ]
    message("FH Admin-2: dropping ", length(drop_clusters), " clusters in ", length(fixed_areas),
            " degenerate areas; these areas are predicted by the model")

    FH_adm2_fix_nest <- tryCatch(
      surveyPrev::fhModel(
        data_fh,
        CI = 0.9,
        cluster.info = cluster.info,
        admin.info = admin.info2,
        admin = 2,
        model = "bym2",
        aggregation = FALSE,
        nested = TRUE
      ),
      error = function(e) {
        message("FH Admin-2 failed for ", indicator, ": ", conditionMessage(e))
        "failed"
      })

    if (!identical(FH_adm2_fix_nest, "failed")) {
      FH_adm2_fix_nest$fixed_areas <- fixed_areas
      FH_adm2_fix_nest$data.info   <- data.info2
      FH_adm2_fix_nest <- FH_adm2_fix_nest[c("res.admin2", "model", "admin2_post", "admin.info",
                                             "fixed_areas", "admin", "data.info")]
    }

    out <- file.path(results_path, paste0("FH_adm2_fix_nest-", indicator, ".qs"))
    qs::qsave(FH_adm2_fix_nest, file = out)
    message("Saved: ", out)

    out1 <- file.path(results_path, paste0("summary-FH_adm2_fix_nest-", indicator, ".qs"))
    qs::qsave(if (identical(FH_adm2_fix_nest, "failed")) "failed" else summary(FH_adm2_fix_nest$model$fit), file = out1)
    message("Saved: ", out1)
    message("done: ", indicator, " ", year)
  }
}
