#' Test for Association/Correlation Between Predicted Rates and Actual Rates in Kedis
#'
#' @param x a kd_model
#' @param ... additional parameters
#'
#' @return a htest object
#' @export
cor.test.kd_model <- function(x, ...){
  test <- cbind(prediction = predict(x$predict_model, x$data$inputs, verbose = 0)[[4]],
                actual = x$data$response$rate) %>%
    as.data.frame %>%
    stats::cor.test(~ actual + prediction, data = ., ...)

  class(test) <- c("kd_model_htest", class(test))
  test$model <- x
  return(test)

}

#' Test for Association/Correlation Between Predicted Rates and Actual Rates in Kedis Cross-Validation
#'
#' @param x a kd_cv object
#' @param ... additional parameters
#'
#' @return a htest object
#' @export
cor.test.kd_cv <- function(x, ...){
  test <- join_cv_predictions(x, ...) %>%
    stats::cor.test(~ actual + predicted, data = ., ...)

  class(test) <- c("kd_model_htest", class(test))
  test$model <- x
  return(test)
}
