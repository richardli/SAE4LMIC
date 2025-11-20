



library(rmarkdown)

# list of countries you want to render
countryList <-c(      
  "Nigeria",
  "Burkina Faso",            
  "Congo Democratic Republic" ,
  "Ethiopia"   ,                  
  "Kenya"       ,     
  "Mozambique"   ,              
  "Rwanda"      ,     
  "Senegal"   ,                 
  "Tanzania"      ,             
  "Mali"  ,                   
  "Sierra Leone" 
)

# for (cty in countryList) {
#   message("Rendering report for: ", cty)
#   
#   render(
#     input  = "/Users/qianyu/Dropbox/binary_code/pcg/GATES/SAE4LMIC/report/report.Rmd", 
#     output_format = "bookdown::pdf_book",
#     params = list(
#       country     = cty,
#       source_path = "/Users/qianyu/Dropbox/binary_code/pcg/GATES",
#       middle_path = "Gates-results/ReportPlots"
#     ),
#     output_file = paste0("Multi-Indicator-SAE-", cty, ".pdf"),
#     envir  = new.env()   # clean environment for each country
#   )
# }



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


for (cty in countryList) {
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


countrylist1= c(
  "Kenya",        
  "Mozambique" ,
  "Rwanda"  ,     
  "Sierra Leone"  ,    
  "Burkina Faso",
  "Ethiopia"  ,
  "Congo Democratic Republic"        
)

library(tinytex)

for (cty in countrylist1) {
  texfile <- here("Gates-results/Reports", paste0("Multi-Indicator-SAE-", cty, ".tex"))
  message("Re-compiling LaTeX for: ", cty)
  latexmk(texfile)   # runs pdflatex (and bibtex etc) as needed
}


