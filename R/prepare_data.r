#' prepare_data
#'
#' @param shapes SpatVector of shapefile, or address to .shp file
#' @param covariates SpatRaster of covariates, or address of .tif or folder of .tifs.
#' @param population SpatRaster of population, or addtess of .tif
#' @param filter_var Variable in shape file to filter by
#' @param response_var Response in shape file
#' @param cov_names Covariates to include, defaults to all covariates in covariates argument. Can include x and y
#' @param na.action If true, fills missing data on rasters (using formula cov_missing and pop_missing). If false, only pixels with non-missing values on all layers are included. Default TRUE.
#' @param cov_missing formula for missing covariates. performed column-wise
#' @param pop_missing formula for missing population. performed column-wise
#' @param max_length max length of zero padding, if null (default) calculated automatically
#'
#' @return a kd_data object
#' @export
prepare_data <- function(shapes, covariates, population = NULL, filter_var,
                         response_var, cov_names  = NULL, max_length = NULL,
                         na.action = TRUE,
                         cov_missing = function(.x){
                           median(.x, na.rm = TRUE)
                         },
                         pop_missing = function(.x){
                           0
                         }){

  if(!inherits(shapes, "SpatVector")){
    if(inherits(shapes, "character")){
      shapes <- terra::vect(shapes)
    } else {
      stop("shapes must be either a SpatVector or a file address")
    }
  }

  if(!inherits(population, "SpatRaster")){
    if(inherits(population, "character")){
      population <- terra::rast(population)
    } else {
      stop("population must be either a SpatRaster or a file address")
    }
  }

  if(!inherits(covariates, "SpatRaster")){
    if(inherits(covariates, "character")){
      if(endsWith(covariates, ".tif")){
        covariates <- terra::rast(covariates)
      } else {
        stopifnot(dir.exists("data/covariates"))
        covariate_files <- list.files("data/covariates", pattern = ".tif$",
                                      full.names = TRUE)
        stopifnot(length(covariate_files) != 0)
        covariates <- terra::rast(covariate_files)
      }
      covariates <- terra::crop(covariates, population)
    } else {
      stop("covariates must be either a SpatRaster or a folder address of .tif filess")
    }
  }

  raw <- list(shapes = shapes,
              covariates = covariates,
              population = population)

  if(is.null(cov_names)){
    cov_names <- names(covariates)
  }

  if(na.action){
    population <- population %>%
      terra::as.data.frame(xy = TRUE, na.rm = FALSE) %>%
      dplyr::mutate(dplyr::across(population,
                                  ~ifelse(is.na(.x), pop_missing(.x), .x))) %>%
      terra::rast(type = "xyz", crs = terra::crs(population))

    covariates <- covariates %>%
      terra::as.data.frame(xy = TRUE, na.rm = FALSE) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(cov_names),
                                  ~ifelse(is.na(.x), cov_missing(.x), .x))) %>%
      terra::rast(type = "xyz", crs = terra::crs(covariates))
  }

  stack <- c(population, covariates)

  full_df <- dplyr::left_join(
    terra::extract(stack, shapes, xy = TRUE),
    shapes %>%
      terra::as.data.frame() %>%
      dplyr::mutate(ID = dplyr::row_number()),
    by = "ID") %>%
    dplyr::select(dplyr::all_of(filter_var),
                  ID,
                  x,
                  y,
                  dplyr::all_of(response_var),
                  dplyr::all_of(cov_names),
                  population)


  startendindex <- lapply(unique(full_df[, "ID"]),
                          function(x) range(which(full_df[, "ID"] == x))) %>%
    do.call(rbind, .)
  startendindex <- startendindex[, ] - 1L


  length <- full_df %>%
    dplyr::select(dplyr::all_of(cov_names), ID) %>%
    dplyr::group_split(ID, .keep = FALSE) %>%
    sapply(nrow) %>%
    max

  if(is.null(max_length)){
    max_length <- length
  }

  data_cov <- full_df %>%
    dplyr::select(dplyr::all_of(cov_names), ID) %>%
    dplyr::group_split(ID, .keep = FALSE) %>%
    lapply(pad_zeros, max_length) %>%
    simplify2array %>%
    aperm(c(3, 1, 2))

  data_pop <- full_df %>%
    dplyr::select(population, ID) %>%
    dplyr::group_split(ID, .keep = FALSE) %>%
    lapply(pad_zeros, max_length) %>%
    simplify2array %>%
    aperm(c(3, 1, 2))

  data_xy <- full_df %>%
    dplyr::select(x, y, ID) %>%
    dplyr::group_split(ID, .keep = FALSE) %>%
    lapply(pad_zeros, max_length) %>%
    simplify2array %>%
    aperm(c(3, 1, 2))

  response <- full_df %>%
    dplyr::group_split(ID) %>%
    lapply(function(x){x[1, c("ID", dplyr::all_of(c(filter_var, response_var)))]}) %>%
    do.call(rbind, .)

  output <- response %>%
    dplyr::pull(dplyr::all_of(response_var))

  kd_data <- list(
    inputs = list(input_cov = data_cov,
                  input_pop = data_pop,
                  input_xy = data_xy),
    outputs = list(output),
    response = response,
    full_df = full_df,
    covariates = covariates,
    population = population,
    shapes = shapes,
    raw = raw,
    names = list(covariates = cov_names,
                 filter_var = filter_var,
                 response_var = response_var),
    crs = terra::crs(covariates, proj = TRUE, describe = TRUE),
    startendindex = startendindex,
    max_length = length,
    length_pad = max_length
  )
  class(kd_data) <- c("kd_data", "list")
  return(kd_data)
}
