#Setup 
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs, "MODIStsp", "svMisc")

#install_github("lbusett/MODIStsp")
MODIStsp(
  gui = FALSE,
  opts_file = json_temp,
  verbose = TRUE,
  parallel = TRUE
)
