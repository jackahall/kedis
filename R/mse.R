#' Mean Squared Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
mse <- function(actual, predicted){
  UseMethod("mse")
}

#' Mean Squared Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
mse.default <- function(actual, predicted){
  Metrics::mse(actual, predicted)
}

#' Mean Squared Error of kd_model
#'
#' @param actual a fitted kd_model object
#' @param predicted the predicted rates, default is from kd_model
#'
#' @export
mse.kd_model <- function(actual,
                         predicted = predict(actual$predict_model,
                                             actual$data$inputs,
                                             verbose = 0)[[4]]){
  actual_rate = actual$data$response$rate
  Metrics::mse(actual_rate, predicted)
}

