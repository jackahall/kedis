#' Root Relative Squared Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
rrse <- function(actual, predicted){
  UseMethod("rrse")
}

#' Root Relative Squared Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
rrse.default <- function(actual, predicted){
  Metrics::rrse(actual, predicted)
}

#' Root Relative Squared Error of kd_model
#'
#' @param actual a fitted kd_model object
#' @param predicted the predicted rates, default is from kd_model
#'
#' @export
rrse.kd_model <- function(actual,
                          predicted = predict(actual$predict_model,
                                              actual$data$inputs,
                                              verbose = 0)[[4]]){
  actual_rate = actual$data$response$rate
  Metrics::rrse(actual_rate, predicted)
}

