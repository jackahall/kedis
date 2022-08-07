#' Bias
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
bias <- function(actual, predicted){
  UseMethod("bias")
}

#' Bias
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
bias.default <- function(actual, predicted){
  Metrics::bias(actual, predicted)
}

#' Bias of kd_model
#'
#' @param actual a fitted kd_model object
#' @param predicted the predicted rates, default is from kd_model
#'
#' @export
bias.kd_model <- function(actual,
                          predicted = predict(actual$predict_model,
                                              actual$data$inputs,
                                              verbose = 0)[[4]]){
  actual_rate = actual$data$response$rate
  Metrics::bias(actual_rate, predicted)
}

#' Bias of kd_cv
#'
#' @param actual a fitted kd_cv object
#' @param predicted the predicted rates, default is from kd_cv
#'
#' @export
bias.kd_cv <- function(actual,
                       predicted = join_cv_predictions(actual)$predicted){
  Metrics::bias(join_cv_predictions(actual)$actual, predicted)
}

