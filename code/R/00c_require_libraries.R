#we also need to install easypackages from ...
source("code/R/00b_libraries.R")

# Function to install and load packages
install_and_load <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

# Apply the function to each package
invisible(lapply(all_libs, install_and_load))
