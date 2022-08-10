#' plot a kd_predict
#'
#' @param x a kd_predict object, made from predict.kd_model
#' @param layer which layer to plot. One of either "output_disag", "output_agg", "output_xy", "output_rate", or any column from the "shapes" SpatVector
#' @param ... for plot generic
#'
#' @export
plot.kd_predict <- function(x, layer = "output_disag", ...){
  stopifnot(inherits(layer, "character"))

  pred_names <- c("output_disag", "output_agg", "output_xy", "output_rate")

  if(length(layer) != 1){
    stop("Length of pred must be 1")
  } else if(layer %in% pred_names){
    pred <- x[[layer]]
  } else {
    pred <- x$data$shapes[, layer]
  }

  if(inherits(pred, "SpatRaster")){
    pred %>%
      as.data.frame(xy = TRUE) %>%
      ggplot2::ggplot() +
      ggplot2::geom_tile(ggplot2::aes_string(x = "x", y = "y", fill = names(pred))) +
      ggplot2::coord_equal() +
      ggplot2::scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = NA) +
      ggplot2::theme_bw()
  } else if(inherits(pred, "SpatVector")){
    pred %>%
      sf::st_as_sf() %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(ggplot2::aes_string(fill = layer)) +
      ggplot2::scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = NA) +
      ggplot2::theme_bw()
  } else {
    plot(pred)
  }
}

#' Plot a kd-model
#'
#' @param x a kd_model object
#' @param layers which layer to plot. One of either "output_disag", "output_agg", "output_xy", "output_rate", or any column from the "shapes" SpatVector
#' @param data a kd_Data object, default to data from x
#' @param ... additional parameters to pass to predict
#'
#' @export
plot.kd_model <- function(x, layers = "output_disag", data = x$data, ...){
  plot(predict(x, data, ...), layers)
}

#' Plot the history of a kd_cv object
#'
#' @param x a kd_cv object
#' @param ... parameters to pass to plot.keras_training_history
#'
#' @export
plot.kd_cv <- function(x, ...){
  ggpubr::ggarrange(plotlist = lapply(x$history, plot, ...))
}
