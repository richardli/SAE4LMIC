



library(rmarkdown)


countrylist= c(
  "Kenya",        
  "Mozambique" ,
  "Rwanda"  ,     
  "Sierra Leone"  ,    
  "Burkina Faso",
  "Ethiopia"  ,
  "Congo Democratic Republic"        
)

output_dir  = "/Users/qianyu/Dropbox/binary_code/pcg/GATES/Gates-results/Reports"


countrylist1= c(
  "Kenya",        
  "Mozambique" ,
  "Rwanda"  ,     
  "Sierra Leone"  ,    
  "Burkina Faso",
  "Ethiopia"  ,
  "Congo Democratic Republic"        
)



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


library(tinytex)
#not used
for (cty in countrylist1) {
  texfile <- here(source_path,"Gates-results/Reports", paste0("Multi-Indicator-SAE-", cty, ".tex"))
  message("Re-compiling LaTeX for: ", cty)
  latexmk(texfile)   # runs pdflatex (and bibtex etc) as needed
}


