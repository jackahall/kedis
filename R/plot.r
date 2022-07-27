#' plot
#'
#' @param x kd_model
#' @param data kd_data, defaults to data from kd_model
#' @param gglayers list of layers to add to ggplot
#' @param reagg reaggregate output
#' @param xy plot field, only valid if layers_xy included otherwise returns prediction
#' @param compare compare to actual data, only works tfor reagg = TRUE
#' @param ... for plot generic
#'
#' @return a kd_plot object
#' @export
plot.kd_model <- function(x, data = x$data, gglayers = NULL, reagg = FALSE,
                    xy = FALSE, compare = FALSE, ...){
  if(reagg){
    kd_plot <- predict(x, reagg = TRUE) %>%
      (sf::st_as_sf)()
    if(compare){
      kd_plot <- kd_plot %>%
      tidyr::pivot_longer(dplyr::all_of(c("prediction", data$names$response_var)))
    } else {
        kd_plot <- kd_plot %>%
          (dplyr::mutate)(value = prediction)
      }
    kd_plot <- kd_plot %>%
      (ggplot2::ggplot)() +
      ggplot2::geom_sf(ggplot2::aes(fill = value)) +
      ggplot2::scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = NA) +
      ggplot2::theme_bw() +
      gglayers
    if(compare){
      name.labs <- c("Actual", "Prediction")
      names(name.labs) <- c(data$names$response_var, "kd_predict")
      kd_plot <- kd_plot +
        ggplot2::facet_wrap(~name,
                            labeller = ggplot2::labeller(name = name.labs))
    }
  } else {
    kd_plot <- predict(x, data, xy = xy) %>%
      (ggplot2::ggplot)() +
      ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = prediction)) +
      ggplot2::coord_equal() +
      ggplot2::scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = NA) +
      ggplot2::theme_bw() +
      gglayers
  }
  class(kd_plot) <- c("kd_plot", class(kd_plot))
  return(kd_plot)
}
