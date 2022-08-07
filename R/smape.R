#' Symmetric Mean Absolute Percentage Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
smape <- function(actual, predicted){
  UseMethod("smape")
}

#' Symmetric Mean Absolute Percentage Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
smape.default <- function(actual, predicted){
  Metrics::smape(actual, predicted)
}

#' Symmetric Mean Absolute Percentage Error of kd_model
#'
#' @param actual a fitted kd_model object
#' @param predicted the predicted rates, default is from kd_model
#'
#' @export
smape.kd_model <- function(actual,
                           predicted = predict(actual$predict_model,
                                               actual$data$inputs,
                                               verbose = 0)[[4]]){
  actual_rate = actual$data$response$rate
  Metrics::smape(actual_rate, predicted)
}

#' Symmetric Mean Absolute Percentage Error of kd_cv
#'
#' @param actual a fitted kd_cv object
#' @param predicted the predicted rates, default is from kd_cv
#'
#' @export
smape.kd_cv <- function(actual,
                        predicted = join_cv_predictions(actual)$predicted){
  Metrics::smape(join_cv_predictions(actual)$actual, predicted)
}
