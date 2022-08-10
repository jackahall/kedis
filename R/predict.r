#' predict
#'
#' @param object a kd_model object
#' @param data a kd_data object. defaults to the data from object
#' @param ... for predict generic
#' @param as.data.frame If FALSE, returns as SpatRasters or SpatVectors. If TRUE, returns as data.frames for rasters and sf for vectors
#'
#' @return a kd_predict object
#' @export
predict.kd_model <- function(object, data = object$data, as.data.frame = FALSE, ...){
  if(inherits(data, "kd_data")){
    pred <- predict(object$predict_model, data$inputs, verbose = 0, ...)
  } else if(inherits(data, "kd_data_for_predict")){

  } else {
    stop("Data not a valid class. Must be either kd_data or kd_data_for_predict")
  }

  names(pred) <- c("output_disag", "output_agg", "output_xy", "output_rate")

  pred[[1]] <- pred[[1]] %>%
    matrix(ncol = 3) %>%
    .[which(.[,2] != 0 & .[,3] != 0), c(2, 3, 1)]

  pred[[3]] <- pred[[3]] %>%
    matrix(ncol = 3) %>%
    .[which(.[,2] != 0 & .[,3] != 0), c(2, 3, 1)]

  pred$output_agg <- data$raw$shapes %>%
    sf::st_as_sf() %>%
    cbind(output_agg = pred$output_agg)

  pred$output_rate <- data$raw$shapes %>%
    sf::st_as_sf() %>%
    cbind(output_rate = pred$output_rate)

  if(as.data.frame){
    pred$output_disag %<>%
      terra::as.data.frame(xy = TRUE) %>%
      setNames(c("x", "y", "output_disag"))
    pred$output_xy %<>%
      terra::as.data.frame(xy = TRUE) %>%
      setNames(c("x", "y", "output_xy"))
    rtn <- c(pred, data = list(data))
    class(rtn) <- c("kd_predict_df", class(rtn))
  } else {
    pred$output_disag <- terra::rasterize(pred$output_disag[,c(1, 2)],
                                          data$raw$covariates,
                                          values = pred$output_disag[,3]) %>%
      setNames("output_disag")

    pred$output_xy <- terra::rasterize(pred$output_xy[,c(1, 2)],
                                       data$raw$covariates,
                                       values = pred$output_xy[,3]) %>%
      setNames("output_xy")

    pred$output_agg %<>%
      terra::vect()

    pred$output_rate %<>%
      terra::vect()

    rtn <- c(pred,
             data = list(data))
    class(rtn) <- c("kd_predict", class(rtn))
  }
  return(rtn)
}
