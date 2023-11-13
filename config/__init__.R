box::use(
  config,
  logger
)

#' @export
load_config <- function(file, mode) {
  path <- box::file(file)
  
  logger$log_info("Loading file: {path}")
  config <- config$get(
    file = path,
    config = Sys.getenv("R_CONFIG_ACTIVE", mode)
  )

  return(config)
}