required_packages <- c(
  "box",
  "config",
  "cluster",
  "corrr",
  "doParallel",
  "dplyr",
  "fastshap",
  "forcats",
  "ggplot2",
  "ggpubr",
  "ggtext",
  "glue",
  "hardhat",
  "janitor",
  "logger",
  "lubridate",
  "maps",
  "MCS",
  "parsnip",
  "progress",
  "purrr",
  "ranger",
  "Rcpp",
  "readxl",
  "recipes",
  "reshape2",
  "rnaturalearth",
  "rnaturalearthdata",
  "rpart",
  "sf",
  "solarPos",
  "stringr",
  "tidyr",
  "tune",
  "utils",
  "vip",
  "workflows",
  "xgboost",
  "zoo"
)

check_installed_package <- function(package_name) {
  package_location <- find.package(package_name, quiet=TRUE)
  
  return(length(package_location) > 0)
}

install_packages <- function(required_packages){
  # function to attach/install packages
  for (p in required_packages) {
    if (check_installed_package(p)) {
      print(paste("Package", p, "is already installed!"))
    } else {
      print(paste("Start install of package", p, "..."))
      install.packages(p)
      if (!check_installed_package(p)) {
        print(paste("Package", p, "is not correctly installed!"))
      }
    }
  }
}

# install all package if not installed yet
install_packages(required_packages)

# this is a special package (based on c++ code), name of package == cpprfast
# first go to the folder 'cpp' and source("__install__.R")
if (check_installed_package("cpprfast")) {
  print("Package cpprfast is already installed!")
} else {
  print("Package cpprfast is not correctly installed!")
}


