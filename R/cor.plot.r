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
#' @param identity_line include a line of x=y, default TRUE
#' @param ... additional parameters
#'
#' @return a plot
#' @export
cor.plot.kd_model <- function(x, identity_line = TRUE, ...){
  plot <- cbind(prediction = predict(x$predict_model, x$data$inputs, verbose = 0)[[4]],
        actual = x$data$response$rate) %>%
    as.data.frame %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = actual, y = prediction), ...)
  if(identity_line){
    plot <- plot +
      ggplot2::geom_function(fun = function(x) x)
  }
  plot
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

#' Correlation Plot for kd_cv
#'
#' @param x a kd_cv object
#' @param identity_line include a line of x=y, default TRUE
#' @param ... additional parameters
#'
#' @return a plot
#' @export
cor.plot.kd_cv <- function(x, identity_line = TRUE, ...){
  pred <- list()
  for(i in seq_len(x$k)){
    pred[[i]] <- cbind(
      prediction = x$prediction[[i]][[4]],
      actual = x$sub_data[[i]]$test$response$rate,
      fold = i)
  }
  plot <- do.call(rbind, pred) %>%
    as.data.frame() %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = actual, y  = prediction))
  if(identity_line){
    plot <- plot +
      ggplot2::geom_function(fun = function(x) x)
  }
  plot
}
