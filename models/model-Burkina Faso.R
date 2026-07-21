#
# This is a sub-script to run models in Burkina Faso
#
# If only a subset of indicator needs to be updated, subset indicatorlist here
country_to_run <- c("Burkina Faso")
print(indicatorlist)
indicatorlist <- indicatorlist
print(source_path)

# # If special treatments are needed for some indicators, update the script below with if-statements
# for (indicator_to_run in indicatorlist) {
# 	source(here(git_path, "models", "model-core.R"))
# }



indicatorlist=c("RH_PCCT_C_DY2","CO_MOBB_W_MOB")
# If special treatments are needed for some indicators, update the script below with if-statements
# This loop use variace fix code in "model-core-newvarfix.R"
for (indicator_to_run in indicatorlist) {
  source(here(git_path, "models", "model-core-newvarfix.R"))
}



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
