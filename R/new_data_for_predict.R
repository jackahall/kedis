#' Format new data to predict using a kedis model
#'
#' @param model the kd_model object for prediction (will not work if xy is included as covariates)
#' @param covariates a SpatRaster of covariates (must be the same covariates as in the model)
#' @param population a SpatRaster of population
#'
#' @export
new_data_for_predict <- function(model, covariates, population){
  if(!all(length(names(covariates)) == length(model$data$names$covariates) &&
          all(names(covariates) %in% model$data$names$covariates))){
    stop("Error: covariates must match those in the model")
  }

  if(any(c("x", "y", "x_norm", "y_norm") %in% names(covariates))){
    stop("Error: new_data_for_predict will not work in models which include xy")
  }

  if(!is.null(model$layers_xy)){
    warning("Warning: new_data_for_predict will not be accurate in models which include xy layers")
  }

  if(terra::nlyr(population) != 1){
    stop("Error: number of layers in population raster is not 1")
  }

  names(population) <- "population"
  stack <- c(covariates, population)

  regions <- ceiling(stack %>% terra::ncell() / model$data$length_pad)

  full_df <- stack %>%
    terra::as.data.frame(xy = TRUE) %>%
    pad_zeros(plyr::round_any(stack %>% terra::ncell(), model$data$length_pad, f = ceiling)) %>%
    cbind(ID = rep(1:regions)) %>%
    data.frame %>%
    dplyr::mutate(dplyr::across(c("x", "y"), ~scale(.x), .names = "{.col}_norm"))

  data_cov <- full_df %>%
    dplyr::select(dplyr::all_of(names(covariates)), .data$ID) %>%
    dplyr::group_split(.data$ID, .keep = FALSE) %>%
    sapply(as.matrix, simplify = "array") %>%
    aperm(c(3, 1, 2))

  data_pop <- full_df %>%
    dplyr::select(population, .data$ID) %>%
    dplyr::group_split(.data$ID, .keep = FALSE) %>%
    sapply(as.matrix, simplify = "array") %>%
    aperm(c(3, 1, 2))

  data_xy <- full_df %>%
    dplyr::select(.data$x, .data$y, .data$ID) %>%
    dplyr::group_split(.data$ID, .keep = FALSE) %>%
    sapply(as.matrix, simplify = "array") %>%
    aperm(c(3, 1, 2))

  data_xy_norm <- full_df %>%
    dplyr::select(.data$x_norm, .data$y_norm, .data$ID) %>%
    dplyr::group_split(.data$ID, .keep = FALSE) %>%
    sapply(as.matrix, simplify = "array") %>%
    aperm(c(3, 1, 2))

  rtn <- list(inputs = list(input_cov = data_cov,
                            input_pop = data_pop,
                            input_xy = data_xy,
                            input_xy_norm = data_xy_norm),
              model = model,
              covariates = covariates,
              population = population)
  class(rtn) <- c("kd_new_data", class(rtn))
  return(rtn)
}
