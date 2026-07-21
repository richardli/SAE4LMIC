



library(rmarkdown)

countrylist1=unique(surveys)





countrylist1= c(
  "Nigeria",
  "Burkina Faso",
  "Kenya",        
  "Mozambique" ,
  "Rwanda"  ,  
  "Senegal"  ,
  "Tanzania"   ,
  "Sierra Leone"  ,    
  "Ethiopia"  ,
  "Mali",
  "Zambia" ,
  "Congo Democratic Republic"        
)
output_dir  = "/Users/qianyu/Dropbox/binary_code/pcg/GATES/Gates-results/Reports"
countrylist1=c("Malawi")

for (cty in countrylist1) {
  message("Rendering report for: ", cty)
  render(
    input  = "/Users/qianyu/Dropbox/binary_code/pcg/GATES/SAE4LMIC/report/report.Rmd",
    output_format = rmarkdown::pdf_document(toc = TRUE, number_sections = FALSE, keep_tex = TRUE),
    params = list(
      country     = cty,
      source_path = "/Users/qianyu/Dropbox/binary_code/pcg/GATES",
      middle_path = "Gates-results/ReportPlots"
    ),
    output_file = paste0("Multi-Indicator-SAE-", cty, ".pdf"),
    output_dir  = output_dir,
    # clean = FALSE,
    envir  = new.env()
  )
}



countrylist1=c("Burkina Faso")

for (cty in countrylist1) {
  message("Rendering report for: ", cty)
  render(
    input  = "/Users/qianyu/Dropbox/binary_code/pcg/GATES/SAE4LMIC/report/report_short.Rmd",
    output_format = rmarkdown::pdf_document(toc = TRUE, number_sections = FALSE, keep_tex = TRUE),
    params = list(
      country     = cty,
      source_path = "/Users/qianyu/Dropbox/binary_code/pcg/GATES",
      middle_path = "Gates-results/ReportPlots"
    ),
    output_file = paste0("Mini-Multi-Indicator-SAE-", cty, ".pdf"),
    output_dir  = output_dir,
    # clean = FALSE,
    envir  = new.env()
  )
}



# ---------------------------------------------------------------------------
# report_anytwoyear.Rmd: choose ANY old/new survey-year pair. Reads the
# per-year-pair folders  ReportPlots/<country>/<yr1>-<yr2>/  (plots) and
# estimates/<country>/<yr1>-<yr2>/  (tables). yr1/yr2 default to min/max when
# left NULL. The plots + tables for that pair must already exist, e.g.:
#   plots/report_year_pair("Ethiopia", 2019, 2024, with_tables = TRUE)
# ---------------------------------------------------------------------------
countrylist1 = c("Malawi")
sel_yr1 <- 2015   # old year (NULL = min for the country)
sel_yr2 <- 2024   # new year (NULL = max for the country)
output_dir  = "/Users/qianyu/Dropbox/binary_code/pcg/GATES/Gates-results/Reports"

for (cty in countrylist1) {
  message("Rendering any-two-year report for: ", cty, " (", sel_yr1, "-", sel_yr2, ")")
  render(
    input  = "/Users/qianyu/Dropbox/binary_code/pcg/GATES/SAE4LMIC/report/report_anytwoyear.Rmd",
    output_format = rmarkdown::pdf_document(toc = TRUE, number_sections = FALSE, keep_tex = TRUE),
    params = list(
      country     = cty,
      source_path = "/Users/qianyu/Dropbox/binary_code/pcg/GATES",
      middle_path = "Gates-results/ReportPlots",
      yr1         = sel_yr1,
      yr2         = sel_yr2
    ),
    output_file = paste0("Multi-Indicator-SAE-", cty, "-", sel_yr1, "-", sel_yr2, ".pdf"),
    output_dir  = output_dir,
    # clean = FALSE,
    envir  = new.env()
  )
}


