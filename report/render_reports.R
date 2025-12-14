



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
countrylist1=c("Burkina Faso")

for (cty in countrylist1) {
  message("Rendering report for: ", cty)
  render(
    input  = "/Users/qianyu/Dropbox/binary_code/pcg/GATES/Gates-results/Reports/report.Rmd", 
    output_format = "bookdown::pdf_book",
    params = list(
      country     = cty,
      source_path = "/Users/qianyu/Dropbox/binary_code/pcg/GATES",
      middle_path = "Gates-results/ReportPlots"
    ),
    output_file = paste0("Multi-Indicator-SAE-", cty, ".pdf"),
    # output_dir  = output_dir,   
    # clean = FALSE,   
    envir  = new.env()
  )
}

