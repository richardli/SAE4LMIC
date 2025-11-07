#
# This is a sub-script to run models in Kenya
#
# If only a subset of indicator needs to be updated, subset indicatorlist here
country_to_run <- c("Kenya")
print(indicatorlist)
indicatorlist <- indicatorlist
print(source_path)

# # If special treatments are needed for some indicators, update the script below with if-statements
# for (indicator_to_run in indicatorlist) {
# 	source(here(git_path, "models", "model-core.R"))
# }

# If special treatments are needed for some indicators, update the script below with if-statements
# This loop use variace fix code in "model-core-newvarfix.R"
for (indicator_to_run in indicatorlist) {
  source(here(git_path, "models", "model-core-newvarfix.R"))
}
