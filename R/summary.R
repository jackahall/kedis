#' Summarize kd_model
#'
#' @param object a kd_model object
#' @param ... additional parameters
#'
#' @export
summary.kd_model <- function(object, ...){
  object <- object$train_model
  summary(object)
}

#' Summarize kd_data
#'
#' @param object a kd_data object
#' @param ... additional parameters
#'
#' @export
summary.kd_data <- function(object, ...){
  cat("Data for Keras-Disaggregation (kedis) model")
  cat("\nTotal number of regions:\t", nrow(object$response))
  cat("\nTotal number of pixels:\t\t", nrow(object$full_df))
  cat("\nLargest region:\t\t\t", object$max_length)
  cat("\nCovarites used:\t", paste(object$names$covariates, collapse = ", "))
}
