

#' Build a kedis model
#'
#' @param formula on object of class "formula". To include a variable with the special "pop(x)", which specifies the population raster and "agg(x)", which specifies the aggregation variable.
#' @param shape SpatVector of shapefile, or address to .shp file, which includes the response variable and geometries of the aggregation areas.
#' @param rasters SpatRaster of covariates, or address of .tif or folder of .tifs. Should include the population raster and all covariates used in the model.
#' @param family a descriptionn of the error distribution and link function ot be used in the model. Kedis currently only supports a "poisson" family.
#' @param layers_cov list of "keras" layers for covariate training. If NULL, pass through with no additional layers.
#' @param layers_xy list of "keras" layers for xy training, otherwise if NULL, pass through with no additional layers.
#' @param loss Manually specify a "keras" loss function to use. If NULL, will use the default loss of the family.
#' @param optimizer The "keras" optimiser to use.
#' @param metrics a vector of "keras" metrics.
#' @param x.center Center the x values before model fitting. Default TRUE.
#' @param x.scale Scale the x values before model fitting. Default TRUE.
#' @param y.center Center the y values before model fitting. Default TRUE.
#' @param y.scale Scale the y values before model fitting. Default TRUE.
#'
#' @return A kd_model object.
#' @export
kedis <- function(formula,
                  shape,
                  rasters,
                  family = poisson(link = "log"),
                  layers_cov = NULL,
                  layers_xy = NULL,
                  loss = NULL,
                  optimizer = keras::optimizer_adam(),
                  metrics = NULL,
                  x.center = TRUE,
                  x.scale = TRUE,
                  y.center = TRUE,
                  y.scale = TRUE){

  # Check shapes is correct format
  if(!inherits(shape, "SpatVector")){
    if(inherits(shape, "character")){
      shape <- terra::vect(shape)
    } else {
      stop("shape must be either a SpatVector or a file address")
    }
  }

  # Check rasters is correct format
  if(!inherits(rasters, "SpatRaster")){
    if(inherits(rasters, "character")){
      if(!any(!grepl("^.+\\.tif$", rasters))){
        rasters <- terra::rast(rasters)
      } else {
        stopifnot(dir.exists(rasters))
        raster_files <- list.files(rasters,
                                   pattern = ".tif$",
                                   full.names = TRUE,
                                   recursive = TRUE)
        stopifnot(length(raster_files) != 0)
        rasters <- terra::rast(raster_files)
      }
    } else {
      stop("rasters must be either a SpatRaster or a folder address of .tif filess")
    }
  }

  # Check family is correct (currently only support poisson)
  if (is.character(family))
    family <- get(family, mode = "function", envir = parent.frame())
  if (is.function(family))
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }
  if(!(family$family %in% c("poisson"))){
    print(family)
    stop("'family' must be 'poisson'")
  }

  full_formula <- stats::update.formula(formula, ~ . + xy(x) + xy(y) + xy_norm(x_norm) + xy_norm(y_norm))
  full_terms <- stats::terms(full_formula, c("pop", "xy", "xy_norm", "agg"))

  if(length(attr(full_terms, "specials")$pop) > 1)
    stop("Only one population raster allowed")
  if(length(attr(full_terms, "specials")$pop) == 0)
    stop("Population raster must be specified with 'pop(var)'")

  if(length(attr(full_terms, "specials")$agg) > 1)
    stop("Only one aggregate variable allowed")
  if(length(attr(full_terms, "specials")$agg) == 0)
    stop("Aggregate variable must be specified with 'agg(var)'")

  shape_df <- as.data.frame(shape)
  shape_df$ID <- seq_len(nrow(shape_df))
  df <- merge(shape_df, terra::extract(terra::crop(rasters, shape, mask = TRUE), shape, xy = TRUE), by = "ID", all = TRUE)
  df <- transform(df,
                  x_norm = scale(df$x, x.center, x.scale),
                  y_norm = scale(df$y, y.center, y.scale))

  df <- stats::model.frame(full_terms, data = df)

  to_kedis_array <- function(df, id_name, length){
    aperm(simplify2array(lapply(split(df, df[, id_name]), function(df, length) {
      rbind(as.matrix(df[, -which(id_name %in% names(df)), drop = FALSE]),
            matrix(0, nrow = length - nrow(df), ncol = ncol(df) - 1))
    }, length)), c(3, 1, 2))
  }

  id_assign <- attr(full_terms, "specials")$agg
  df_id <- df[, id_assign, drop = FALSE]
  names(df_id) <- sapply(df_id, function(v) attr(v, "name"))
  id_var <- names(df)[id_assign]
  id_name <- sapply(df_id, function(v) attr(v, "name"))
  length <- max(sapply(split(df_id, df_id[, id_name]), function(group) nrow(group)))

  df_pop <- df[, c(id_assign, attr(full_terms, "specials")$pop), drop = FALSE]
  names(df_pop) <- sapply(df_pop, function(v) attr(v, "name"))
  df_pop <- to_kedis_array(df_pop, id_name, length)

  df_xy <- df[, c(id_assign, attr(full_terms, "specials")$xy), drop = FALSE]
  names(df_xy) <- sapply(df_xy, function(v) attr(v, "name"))
  df_xy <- to_kedis_array(df_xy, id_name, length)

  df_xy_norm <- df[, c(id_assign, attr(full_terms, "specials")$xy_norm), drop = FALSE]
  names(df_xy_norm) <- sapply(df_xy_norm, function(v) attr(v, "name"))
  df_xy_norm <- to_kedis_array(df_xy_norm, id_name, length)

  cov_formula <- stats::update.formula(formula, stats::as.formula(paste("~ . + 0 -", paste(names(df)[c(id_assign, unlist(attr(full_terms, "specials")))], collapse = " - "))))
  cov_terms <- stats::terms(cov_formula, c("pop", "xy", "xy_norm", "agg"))
  df_cov <- cbind(df_id, stats::model.matrix(cov_terms, df))
  df_cov <- to_kedis_array(df_cov, id_name, length)

  response_formula <- stats::update.formula(formula, stats::as.formula(paste("~ 0 +", names(df)[id_assign])))
  response_var <- names(df)[attr(stats::terms(df), "response")]
  df_response <- terra::aggregate(response_formula, data = shape, FUN = sum)
  response <- df_response[[response_var]]

  names(df) <- sapply(seq_along(df), function(i) ifelse(is.null(attr(df[[i]], "name")), names(df)[i], attr(df[[i]], "name")))

  add_layer <- function(x, layer, ...){
    if(is.null(layer)){
      x
    } else {
      stopifnot(inherits(layer, "keras.engine.base_layer.Layer"))
      layer(x, ...)
    }
  }

  input_cov <- keras::layer_input(shape = dim(df_cov)[-1], name = "input_cov")
  input_pop <- keras::layer_input(shape = dim(df_pop)[-1], name = "input_pop")
  input_xy <- keras::layer_input(shape = dim(df_xy)[-1], name = "input_xy")
  input_xy_norm <- keras::layer_input(shape = dim(df_xy_norm)[-1], name = "input_xy_norm")

  training_layers_cov <- input_cov
  for(i in 1:length(layers_cov)){
    training_layers_cov <- add_layer(training_layers_cov, layers_cov[[i]])
  }
  training_layers_cov <- keras::layer_dense(training_layers_cov, units = 1, name = "training_layers_cov_final")

  if(is.null(layers_xy)){
    spatial_summing_layer <- training_layers_cov
  } else {
    training_layers_xy <- input_xy_norm
    if(inherits(layers_xy, "list")){
      for(i in 1:length(layers_xy)){
        training_layers_xy <- add_layer(training_layers_xy, layers_xy[[i]])
      }
    } else {
      stop("Enter valid value for layers_xy")
    }
    training_layers_xy <- keras::layer_dense(training_layers_xy, units = 1, name = "training_layers_xy_final")
    spatial_summing_layer <- keras::layer_add(list(training_layers_cov,
                                                   training_layers_xy)) # Can't add name as error - fix in future build
  }

  if(family$family == "poisson"){
    linkinv <- function(eta){
      exp(eta)
    }
  }

  link_layer <- keras::layer_lambda(spatial_summing_layer, linkinv,
                                    name = "link_layer")

  output_agg <- keras::layer_dot(c(link_layer,
                                   input_pop),
                                 axes = 1,
                                 name = "output_agg")

  output_disag <- keras::layer_concatenate(c(link_layer,
                                             input_xy),
                                           axis = -1,
                                           name = "output_disag")
  if(!is.null(layers_xy)){
    output_xy <- keras::layer_concatenate(c(training_layers_xy,
                                            input_xy),
                                          axis = -1,
                                          name = "output_xy")
  } else {
    output_xy <- NULL
  }

  train_model <- keras:: keras_model(
    inputs = c(input_cov,
               input_pop,
               input_xy,
               input_xy_norm),
    outputs = c(output_agg),
    name = "train_model"
  )

  predict_model <- keras::keras_model(
    inputs = c(input_cov,
               input_pop,
               input_xy,
               input_xy_norm),
    output = c(output_disag,
               output_agg,
               output_xy),
    name = "predict_model"
  )

  if(is.null(loss)){ # Then get default loss function for family
    if(family$family == "poisson")
      loss <- keras::loss_poisson()
  }

  keras::compile(train_model,
                 optimizer,
                 loss,
                 metrics
  )

  rtn <- list(
    inputs = list(input_cov = df_cov,
                  input_pop = df_pop,
                  input_xy = df_xy,
                  input_xy_norm = df_xy_norm),
    outputs = list(response),
    data = df,
    shape = shape,
    rasters = rasters,
    crs = terra::crs(rasters, proj = TRUE, describe = TRUE),
    train_model = train_model,
    predict_model = predict_model,
    layers_cov = layers_cov,
    layers_xy = layers_xy,
    call = match.call(),
    formula = cov_formula
  )
  class(rtn) <- c("kd_model", class(rtn))
  return(rtn)
}
