#' Correlation Plot
#'
#' @param x model
#' @param ... additional parameters
#'
#' @return a plot
#' @export
cor.plot <- function(x, ...){
  UseMethod("cor.plot")
}

#' Correlation Plot for kd_model
#'
#' @param x a kd_model object
#' @param ... additional parameters
#'
#' @return a plot
#' @export
cor.plot.kd_model <- function(x, ...){
  cbind(prediction = predict(x$predict_model, x$data$inputs, verbose = 0)[[4]],
        actual = x$data$response$rate) %>%
    as.data.frame %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = actual, y = prediction))
}

#' Correlation Plot for kd_model_htest
#'
#' @param x a kd_model_htest object
#' @param ... additional parameters
#'
#' @return a plot
#' @export
cor.plot.kd_model_htest <- function(x, ...){
  x <- x$model
  cor.plot.kd_model(x)
}
