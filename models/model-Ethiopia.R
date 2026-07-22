#
# This is a sub-script to run models in Ethiopia
#
# If only a subset of indicator needs to be updated, subset indicatorlist here
country_to_run <- c("Ethiopia")
print(indicatorlist)
indicatorlist <- indicatorlist
print(source_path)

# If special treatments are needed for some indicators, update the script below with if-statements
# 2024 round onward: use the canonical model-core.R (writes plain-prefixed results,
# e.g. res_adm0-<ind>.qs). The legacy model-core-newvarfix.R (new_ prefix) was used
# for the 2025 round; results with either prefix are read transparently via
# misc/resolve_qs.R in the report pipeline.
for (indicator_to_run in indicatorlist) {
  source(here(git_path, "models", "model-core.R"))
}



country_to_run   <- "Ethiopia"
year_to_run      <- 2024              # the one year to fit
indicator_to_run <- infolist$ID       # or a subset
source(here(git_path, "models", "model-core-oneyear.R"))

country_to_run   <- "Ethiopia"
year_to_run      <- 2019
indicator_to_run <- infolist$ID
source(here(git_path, "models", "model-core-oneyear.R"))


# indicatorlist1<- c( "CO_MOBB_W_MOB", "CM_ECMR_C_NNF", "ED_LITR_W_LIT")

# Legacy 2025-round path (new_ prefix) — kept for reference:
# for (indicator_to_run in indicatorlist) {
#   source(here(git_path, "models", "model-core-newvarfix.R"))
# }



## -------------------------------------------------------------------------##
## ---------------------------- check results ------------------------------##
## -------------------------------------------------------------------------##

source(here(git_path, "check", "check-modelresult.R"))

check_tab <- check_model_results(
  country_to_run = country_to_run,
  indicatorlist = indicatorlist,
  source_path    = source_path
)


print(check_tab)

out_check <- file.path(source_path, "Gates-results/Results",country_to_run ,paste0(country_to_run,"_check_models.csv"))
write.csv(check_tab, out_check, row.names = FALSE)
