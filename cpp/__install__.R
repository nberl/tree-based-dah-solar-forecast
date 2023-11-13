box::use(
  logger,
  Rcpp,
  utils
)

install_package_from_cpp_files <- function(package_name, cpp_files) {
  current_dir <- getwd()
  on.exit(setwd(current_dir))
  setwd(box::file())
  temp_dir <- tempdir()
  env <- new.env() # to use Rcpp package skeleton within function

  # Create a package containing to-be-compiled source code
  Rcpp$Rcpp.package.skeleton(
    name = package_name,
    environment = env,
    path = temp_dir,
    cpp_files = file.path(".", cpp_files),
    example_code = FALSE
  )
  path_package <- file.path(temp_dir, package_name)
  stopifnot(dir.exists(path_package))
  setwd(path_package)
  # Generates the bindings required to call C++ functions from R
  Rcpp$compileAttributes()

  logger$log_info("Package {package_name} created!")

  setwd("..")
  R <- shQuote(file.path(R.home(component = "bin"), "R"))
  exitcode <- system(paste(R, "CMD build", package_name))
  stopifnot(exitcode == 0L)

  logger$log_info("Package {package_name} build!")

  tar_file <- paste0(package_name, "_1.0.tar.gz")
  utils$install.packages(tar_file, repos = NULL, type = "source")

  stopifnot(require(package_name, character.only = TRUE))
  
  logger$log_info("Package {package_name} installed!")

  return(TRUE)
}

install_package_from_cpp_files(
  package_name = "cpprfast",
  cpp_files = c("weighted_kmeans.cpp")
)
