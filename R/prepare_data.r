#' prepare_data
#'
#' @param shapes SpatVector of shapefile, or address to .shp file
#' @param covariates SpatRaster of covariates, or address of .tif or folder of .tifs.
#' @param population SpatRaster of population, or addtess of .tif
#' @param filter_var Variable in shape file to filter by
#' @param response_var Response in shape file
#' @param cov_names Covariates to include, defaults to all covariates in covariates argument. Can include x and y
#' @param na.action Fill NA cells in rasters
#'
#' @return a kd_data object
#' @export
prepare_data <- function(shapes, covariates, population = NULL, filter_var,
                         response_var, cov_names  = NULL, na.action = FALSE){

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

  if(is.null(cov_names)){
    cov_names <- names(covariates)
  }

  if(na.action){
    population[is.na(population)] <- 0

    for(i in 1:dim(covariates)[3]){
      covariates[[i]][is.na(covariates[[i]])] <-
        sapply(covariates[[i]], function(x){
          x <- as.data.frame(x)
          stats::median(x[,1], na.rm = TRUE)
        })
    }
  }

  population <- terra::mask(population, shapes)
  covariates <- terra::mask(covariates, shapes)

  shapes_df <- terra::as.data.frame(shapes)
  n_regions = nrow(shapes_df)

  names(population) <- "population"
  stack <- c(population, covariates)

  full_df <- data.frame()
  for(i in 1:n_regions){
    full_df <- rbind(full_df,
                     cbind(shapes_df[i, c(filter_var, response_var)],
                           terra::as.data.frame(terra::crop(stack, shapes[i,], mask = TRUE), xy = TRUE),
                           row.names = NULL))
  }

  startendindex <- lapply(unique(full_df[, filter_var]),
                          function(x) range(which(full_df[, filter_var] == x))) %>%
    do.call(rbind, .)
  startendindex <- startendindex[, ] - 1L

  max_length <- full_df %>%
    (dplyr::select)(dplyr::all_of(cov_names), ID = dplyr::all_of(filter_var)) %>%
    dplyr::group_split(ID, .keep = FALSE) %>%
    sapply(nrow) %>%
    max

  data_cov <- full_df %>%
    dplyr::select(dplyr::all_of(cov_names), ID = dplyr::all_of(filter_var)) %>%
    dplyr::group_split(ID, .keep = FALSE) %>%
    lapply(pad_zeros, max_length) %>%
    simplify2array %>%
    aperm(c(3, 1, 2))

  data_pop <- full_df %>%
    dplyr::select(ID = dplyr::all_of(filter_var), population) %>%
    dplyr::group_split(ID, .keep = FALSE) %>%
    lapply(pad_zeros, max_length) %>%
    simplify2array %>%
    aperm(c(3, 1, 2))

  data_xy <- full_df %>%
    dplyr::select(ID = dplyr::all_of(filter_var), x, y) %>%
    dplyr::group_split(ID, .keep = FALSE) %>%
    lapply(pad_zeros, max_length) %>%
    simplify2array %>%
    aperm(c(3, 1, 2))

  response <- full_df %>%
    dplyr::select(ID = (dplyr::all_of)(filter_var), (dplyr::all_of)(names(full_df))) %>%
    dplyr::group_split(ID) %>%
    lapply(function(x){x[1, c("ID", dplyr::all_of(response_var))]}) %>%
    do.call(rbind, .)

  output <- response %>%
    (dplyr::pull)(dplyr::all_of(response_var))

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
    names = list(covariates = cov_names,
                 filter_var = filter_var,
                 response_var = response_var),
    crs = terra::crs(covariates, proj = TRUE, describe = TRUE),
    startendindex = startendindex,
    max_length = max_length
  )
  class(kd_data) <- c("kd_data", "list")
  return(kd_data)
}
