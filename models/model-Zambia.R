#
# This is a sub-script to run models in Zambia
#
# If only a subset of indicator needs to be updated, subset indicatorlist here
country_to_run <- c("Zambia")
print(indicatorlist)
indicatorlist <- indicatorlist

# If special treatments are needed for some indicators, update the script below with if-statements
for (indicator_to_run in indicatorlist) {
	source(here(git_path, "models", "model-core.R"))
}
