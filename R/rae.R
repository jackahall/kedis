#' Relative Absolute Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
rae <- function(actual, predicted){
  UseMethod("rae")
}

#' Relative Absolute Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
rae.default <- function(actual, predicted){
  Metrics::rae(actual, predicted)
}

#' Relative Absolute Error of kd_model
#'
#' @param actual a fitted kd_model object
#' @param predicted the predicted rates, default is from kd_model
#'
#' @export
rae.kd_model <- function(actual,
                         predicted = predict(actual$predict_model,
                                             actual$data$inputs,
                                             verbose = 0)[[4]]){
  actual_rate = actual$data$response$rate
  Metrics::rae(actual_rate, predicted)
}

#' Relative Absolute Error of kd_cv
#'
#' @param actual a fitted kd_cv object
#' @param predicted the predicted rates, default is from kd_cv
#'
#' @export
rae.kd_cv <- function(actual,
                      predicted = join_cv_predictions(actual)$predicted){
  Metrics::rae(join_cv_predictions(actual)$actual, predicted)
}
