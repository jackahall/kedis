#' Sum of Squared Errors
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
sse <- function(actual, predicted){
  UseMethod("sse")
}

#' Sum of Squared Errors
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
sse.default <- function(actual, predicted){
  Metrics::sse(actual, predicted)
}

#' Sum of Squared Errors of kd_model
#'
#' @param actual a fitted kd_model object
#' @param predicted the predicted rates, default is from kd_model
#'
#' @export
sse.kd_model <- function(actual,
                         predicted = predict(actual$predict_model,
                                             actual$data$inputs,
                                             verbose = 0)[[4]]){
  actual_rate = actual$data$response$rate
  Metrics::sse(actual_rate, predicted)
}

#' Sum of Squared Errors of kd_cv
#'
#' @param actual a fitted kd_cv object
#' @param predicted the predicted rates, default is from kd_cv
#'
#' @export
sse.kd_cv <- function(actual,
                      predicted = join_cv_predictions(actual)$predicted){
  Metrics::sse(join_cv_predictions(actual)$actual, predicted)
}
