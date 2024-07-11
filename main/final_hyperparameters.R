box::use(
  model/common,
  dplyr[...],
  glue,
  purrr,
  readr,
  tidyr
)

create_table_hyperparameters <- function() {
  main_dir <- glue$glue(common$main_dir_hyperparameters())
  
  dirs <- list.dirs(main_dir)[-1]
  
  df_hp <- dirs %>%
    purrr$map(
      ~{
        dir <- .x
        df_x <- list.files(dir) %>%
          purrr$map(
            ~ {
              model_name <- .x
              df_hp <- readr$read_rds(file.path(dir, model_name)) %>%
                mutate(
                  model_name = model_name,
                  tune_metric = gsub(glue$glue(main_dir, "/"), "", dir)
                )
            }
          ) %>%
            bind_rows()
      }
    ) %>%
      bind_rows() %>%
      select(model_name, tune_metric, trees, everything()) %>%
      mutate(model_name = gsub("solar_load_factor_be_|.rds", "", model_name)) %>%
      tidyr$separate(model_name, into = c("fs", "cl", "m"), sep = "_")
  
  list_hyperparameters <- df_hp %>%
    split(f = .$m)
  
  not_all_na <- function(x) any(!is.na(x))
  
  list_hyperparameters <- list_hyperparameters %>%
    purrr$map(
      ~ {
        df_hp <- .x %>%
          select(-m) %>%
          select(where(not_all_na))
        
        if ("trees" %in% colnames(df_hp)) {
          df_hp <- df_hp %>%
            arrange(fs, cl, trees, tune_metric)
        } else {
          df_hp <- df_hp %>%
            arrange(fs, cl, tune_metric)
        }
        
        return(df_hp)
      }
    )
  
  return(list_hyperparameters)  
}

list_df_hyperparameters_per_model_type <- create_table_hyperparameters()
list_df_hyperparameters_per_model_type

