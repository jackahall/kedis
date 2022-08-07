#' Print kd_model
#'
#' @param x a kd_model object
#' @param ... additional parameters
#'
#' @export
print.kd_model <- function(x, ...){
  summary(x)
}

#' Print kd_data
#'
#' @param x a kd_data object
#' @param ... additional parameters
#'
#' @export
print.kd_data <- function(x, ...){
  summary(x)
}

#' Print kd_cv
#'
#' @param x a kd_cv object
#' @param ... additional parameters
#'
#' @export
print.kd_cv <- function(x, ...){
  summary(x)
}
