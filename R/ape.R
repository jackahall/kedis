#' Absolute Percentage Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
ape <- function(actual, predicted){
  UseMethod("ape")
}

#' Absolute Percentage Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
ape.default <- function(actual, predicted){
  Metrics::ape(actual, predicted)
}

#' Absolute Percentage Error of kd_model
#'
#' @param actual a fitted kd_model object
#' @param predicted the predicted rates, default is from kd_model
#'
#' @export
ape.kd_model <- function(actual,
                         predicted = predict(actual$predict_model,
                                             actual$data$inputs,
                                             verbose = 0)[[4]]){
  actual_rate = actual$data$response$rate
  Metrics::ape(actual_rate, predicted)
}

#' Absolute Percentage Error of kd_cv
#'
#' @param actual a fitted kd_cv object
#' @param predicted the predicted rates, default is from kd_cv
#'
#' @export
ape.kd_cv <- function(actual,
                      predicted = join_cv_predictions(actual)$predicted){
  Metrics::ape(join_cv_predictions(actual)$actual, predicted)
}
