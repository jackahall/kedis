#' Percent Bias
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
percent_bias <- function(actual, predicted){
  UseMethod("percent_bias")
}

#' Percent Bias
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
percent_bias.default <- function(actual, predicted){
  Metrics::percent_bias(actual, predicted)
}

#' Percent Bias of kd_model
#'
#' @param actual a fitted kd_model object
#' @param predicted the predicted rates, default is from kd_model
#'
#' @export
percent_bias.kd_model <- function(actual,
                          predicted = predict(actual$predict_model,
                                              actual$data$inputs,
                                              verbose = 0)[[4]]){
  actual_rate = actual$data$response$rate
  Metrics::percent_bias(actual_rate, predicted)
}

