box::use(
  dplyr[...],
  recipes
)

#' @export
get_recipe <- function(df, config_model) {
  if (!("target" %in% colnames(df))) {
    df <- df %>%
      rename("target" = !!sym(config_model$target_var))
  }
  
  recipe <- recipes$recipe(target ~ ., data = df[0, ]) %>%
    recipes$step_rm(one_of(!!config_model$other_vars)) 
  
  # if ("hour" %in% colnames(df)) {
  #   recipe <- recipe %>%
  #     recipes$step_mutate_at(hour, fn = factor) %>% 
  #     recipes$step_dummy(hour)
  # }
  
  return(recipe)
}