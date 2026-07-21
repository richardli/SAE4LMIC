# ---------------------------------------------------------------------------
# resolve_qs(): result-file path resolver shared by the report pipeline
# (plots/report_helper.R) and the web pipeline (webplots/fct_reading_result.R).
#
# Dependency-free (base R only) so either pipeline can source it cheaply:
#   source(here::here("misc", "resolve_qs.R"))
#   # or, when git_path is already defined by a driver:
#   source(file.path(git_path, "misc", "resolve_qs.R"))
# ---------------------------------------------------------------------------

# Two naming conventions coexist in Gates-results/Results/<country>/<year>/:
#   - plain : written by model-core.R            e.g. "res_adm0-<ind>.qs"      [canonical, future]
#   - new_  : written by model-core-newvarfix.R  e.g. "new_res_adm0-<ind>.qs"  [legacy 2025 round]
#
# resolve_qs() returns the path to whichever exists, so a single report/plot can
# mix conventions across years (e.g. Ethiopia 2019 = new_, 2024 = plain) with no
# data renaming and no change to model-core.R.
#
# Arguments
#   results_path : folder ".../Results/<country>/<year>"
#   base         : model-name stem, plain OR new_-prefixed; a leading "new_" is
#                  stripped so callers may pass either (e.g. "res_adm0-" or
#                  "new_res_adm0-"). Use "" for un-prefixed files (raw survey qs).
#   indicator    : indicator ID (e.g. "CH_VACC_C_MSL").
#   prefer       : which convention wins when BOTH files exist in the folder;
#                  "plain" (default) so a fresh model-core.R refit supersedes a
#                  stale new_ file.
#
# Returns a single path string. When neither file exists, returns the plain
# candidate path (which does not exist) so downstream file.exists()/tryCatch()
# handling behaves exactly as before.
resolve_qs <- function(results_path, base, indicator, prefer = c("plain", "new")) {
  prefer <- match.arg(prefer)
  base_plain <- sub("^new_", "", base)

  cand_plain <- file.path(results_path, paste0(base_plain, indicator, ".qs"))
  cand_new   <- file.path(results_path, paste0("new_", base_plain, indicator, ".qs"))

  cands <- if (prefer == "plain") c(cand_plain, cand_new) else c(cand_new, cand_plain)
  hit <- cands[file.exists(cands)]
  if (length(hit) == 0L) return(cand_plain)
  hit[[1L]]
}
