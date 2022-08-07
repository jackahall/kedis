#' Root Mean Squared Log Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
rmsle <- function(actual, predicted){
  UseMethod("rmsle")
}

#' Root Mean Squared Log Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
rmsle.default <- function(actual, predicted){
  Metrics::rmsle(actual, predicted)
}

#' Root Mean Squared Log  Error of kd_model
#'
#' @param actual a fitted kd_model object
#' @param predicted the predicted rates, default is from kd_model
#'
#' @export
rmsle.kd_model <- function(actual,
                           predicted = predict(actual$predict_model,
                                               actual$data$inputs,
                                               verbose = 0)[[4]]){
  actual_rate = actual$data$response$rate
  Metrics::rmsle(actual_rate, predicted)
}

#' Root Mean Squared Log Error of kd_cv
#'
#' @param actual a fitted kd_cv object
#' @param predicted the predicted rates, default is from kd_cv
#'
#' @export
rmsle.kd_cv <- function(actual,
                        predicted = join_cv_predictions(actual)$predicted){
  Metrics::rmsle(join_cv_predictions(actual)$actual, predicted)
}
