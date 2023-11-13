box::use(
  dplyr[...],
  recipes,
  stats,
  vip
)

#' @export
table_variable_importance <- function(fitted_model) {
  df_vip <- fitted_model %>%
    vip$vi()
  
  return()
}

get_pfun <- function(model_type) {
  if (model_type == "xgboost") {
    pfun <- function(object, newdata = NULL) {
      if (is.null(newdata)) {
        return(stats$predict(object))
      } else {
        return(stats$predict(object, newdata = newdata))
      }
    }
  } else {  
    pfun <- function(object, newdata) {
      stats$predict(object, data = newdata)$predictions
    }
  }
  
  return(pfun)
}

#' @export
variable_importance_shap <- function(train_object, df_test, nsim = 30) {
  fitted_model <- train_object$fitted_model[[1]]$fit$fit$fit 
  
  df_train <- train_object$recipe[[1]] %>% 
    recipes$prep(training = train_object$df_train[[1]]) %>% 
    recipes$bake(NULL) %>%
    select(-target)
  
  df_test <- df_test %>%
    select(all_of(colnames(df_train)))
  
  pfun <- get_pfun(model_type = train_object$model_engine)
  
  df_shap <- vip$vi_shap(
    object = fitted_model,
    train = as.matrix(df_train), 
    pred_wrapper = pfun, 
    nsim = nsim, 
    newdata = as.matrix(df_test)
  ) %>%
    mutate(variable = names(df_train))
  
  return(df_shap)
}

# #' @export
# variable_importance_shap <- function(fitted_model, df_train, pfun, nsim_shap, newdata){
#   df_vip <- fitted_model %>%
#     vip$vi_shap(
#       train = as.matrix(df_train), 
#       pred_wrapper = pfun, 
#       nsim=nsim_shap, 
#       newdata = newdata
#     )
# }