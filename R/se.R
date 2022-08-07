#' Squared Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
se <- function(actual, predicted){
  UseMethod("se")
}

#' Squared Error
#'
#' @param actual  The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector is a prediction for the corresponding element in actual.
#'
#' @export
se.default <- function(actual, predicted){
  Metrics::se(actual, predicted)
}

#' Squared Error of kd_model
#'
#' @param actual a fitted kd_model object
#' @param predicted the predicted rates, default is from kd_model
#'
#' @export
se.kd_model <- function(actual,
                        predicted = predict(actual$predict_model,
                                            actual$data$inputs,
                                            verbose = 0)[[4]]){
  actual_rate = actual$data$response$rate
  Metrics::se(actual_rate, predicted)
}

#' Squared Error of kd_cv
#'
#' @param actual a fitted kd_cv object
#' @param predicted the predicted rates, default is from kd_cv
#'
#' @export
se.kd_cv <- function(actual,
                     predicted = join_cv_predictions(actual)$predicted){
  Metrics::se(join_cv_predictions(actual)$actual, predicted)
}
