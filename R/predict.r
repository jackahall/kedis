#' predict
#'
#' @param object kd_model
#' @param data kd_data
#' @param reagg reaggregate output
#' @param xy return field output, only works if model has layers_xy declared, otherwise returns prediction
#' @param ... for predict generic
#'
#' @return a kd_predict object
#' @export
predict.kd_model <- function(object, data = object$data, reagg = FALSE, xy = FALSE, ...){
  if(reagg){
    shapes <- data$shapes %>%
      (sf::st_as_sf) %>%
      cbind(prediction = predict(object$predict_model, data$inputs, verbose = 0)[[2]]) %>%
      (terra::vect)
    class(shapes) <- c("kd_predict_vect", "kd_predict", class(shapes))
    return(shapes)
  } else {
    if(xy){
      pred <- matrix(predict(object$predict_model, data$inputs, verbose = 0)[[3]], ncol = 3)
    } else {
      pred <- matrix(predict(object$predict_model, data$inputs, verbose = 0)[[1]], ncol = 3)
    }

    kd_predict_rast <- pred[which(pred[,2] != 0 & pred[,3] != 0), c(2, 3, 1)] %>%
      as.data.frame %>%
      (stats::setNames)(c("x", "y", "prediction"))
    class(kd_predict_rast) <- c("kd_predict_rast", "kd_predict", class(kd_predict_rast))
    return(kd_predict_rast)
  }
}
