#' Mean Absolute Percentage Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
mape <- function(actual, predicted){
  UseMethod("mape")
}

#' Mean Absolute Percentage Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
mape.default <- function(actual, predicted){
  Metrics::mape(actual, predicted)
}

#' Mean Absolute Percentage Error of kd_model
#'
#' @param actual a fitted kd_model object
#' @param predicted the predicted rates, default is from kd_model
#'
#' @export
mape.kd_model <- function(actual,
                          predicted = predict(actual$predict_model,
                                              actual$data$inputs,
                                              verbose = 0)[[4]]){
  actual_rate = actual$data$response$rate
  Metrics::mape(actual_rate, predicted)
}

#' Mean Absolute Percentage Error of kd_cv
#'
#' @param actual a fitted kd_cv object
#' @param predicted the predicted rates, default is from kd_model
#'
#' @export
mape.kd_cv <- function(actual,
                       predicted = join_cv_predictions(actual)$predicted){
  Metrics::mape(join_cv_predictions(actual)$actual, predicted)
}
