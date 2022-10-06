#' build_model generic
#'
#' @param ... parameters
#'
#' @export
build_model <- function(...) {
  UseMethod("build_model")
}

#' build_model
#'
#' @param data kd_data
#' @param layers_cov list of layers for covariate training. If NULL, pass through with no additional layers.
#' @param layers_xy list of layers for xy training, otherwise if TRUE, pass through with no additional layers. If NULL, do not include xy layers. Default NULL. If NULL, output_xy will be the same as output_disag
#' @param optimizer keras optimizer
#' @param loss keras loss, if included model will compile
#' @param metrics keras metrics
#' @param ... other parameters to pass to keras::compile
#' @param seed seed, default NULL
#' @param clear_session default TRUE, clears keras session before running
#' @param link link function to be used. default "identity", can be one of "logit", "probit", "cauchit", "cloglog", "identity", "log", "sqrt", "1/mu^2", "inverse".
#'
#' @return a kd_model object
#' @export
build_model.kd_data <- function(data,
                                layers_cov = NULL,
                                layers_xy = NULL,
                                link = "identity",
                                optimizer = NULL,
                                loss = NULL,
                                metrics = NULL,
                                ...,
                                seed = NULL,
                                clear_session = TRUE){

  add_layer <- function(x, layer){
    if(is.null(layer)){
      x
    } else {
      stopifnot(inherits(layer, "keras.engine.base_layer.Layer"))
      x %>% layer
    }
  }

  if(clear_session){
    keras::k_clear_session()
  }

  if(!is.null(seed)){
    set.seed(seed)
    tensorflow::set_random_seed(seed)
  }

  input_cov <- keras::layer_input(shape = dim(data$input$input_cov)[-1], name = "input_cov")
  input_pop <- keras::layer_input(shape = dim(data$input$input_pop)[-1], name = "input_pop")
  input_xy <- keras::layer_input(shape = dim(data$input$input_xy)[-1], name = "input_xy")
  input_xy_norm <- keras::layer_input(shape = dim(data$input$input_xy_norm)[-1], name = "input_xy_norm")

  training_layers_cov <- input_cov
  for(i in 1:length(layers_cov)){
    training_layers_cov <- training_layers_cov %>%
      add_layer(layers_cov[[i]])
  }
  training_layers_cov <- training_layers_cov %>%
    keras::layer_dense(units = 1, name = "training_layers_cov_final")

  if(is.null(layers_xy)){
    spatial_summing_layer <- training_layers_cov
  } else {
    training_layers_xy <- input_xy_norm
    if(inherits(layers_xy, "list")){
      for(i in 1:length(layers_xy)){
        training_layers_xy <- training_layers_xy %>%
          add_layer(layers_xy[[i]])
      }
    } else if(layers_xy){

    } else {
      stop("Enter valid value for layers_xy")
    }
    training_layers_xy <- training_layers_xy %>%
      keras::layer_dense(units = 1, name = "training_layers_xy_final")
    spatial_summing_layer <- keras::layer_add(c(training_layers_cov,
                                                training_layers_xy),
                                              name = "spatial_summing_layer")
  }

  if(link == "logit"){
    linkinv <- function(eta){
      log(eta / (1 - eta))
    }
  } else if(link == "probit"){
    linkinv <- function(eta){
      stats::pnorm(eta)
    }
  } else if(link == "cauchit"){
    linkinv <- function(eta){
      stats::pcauchy(eta)
    }
  } else if(link == "cloglog"){
    linkinv <- function(eta){
      -expm1(-exp(eta))
    }
  } else if(link == "identity"){
    linkinv <- function(eta){
      eta
    }
  } else if(link == "log"){
    linkinv <- function(eta){
      exp(eta)
    }
  } else if(link == "sqrt"){
    linkinv <- function(eta){
      eta^2
    }
  } else if(link == "1/mu^2"){
    linkinv <- function(eta){
      1/sqrt(eta)
    }
  } else if(link == "inverse"){
    linkinv <- function(eta){
      1/eta
    }
  } else {
    stop("Link not valid")
  }

  link_layer <- spatial_summing_layer %>%
    keras::layer_lambda(linkinv,
                        name = "link_layer")

  output_agg <- keras::layer_dot(c(link_layer,
                                   input_pop),
                                 axes = 1,
                                 name = "output_agg")

  agg_pop <- keras::layer_average_pooling_1d(input_pop,
                                             pool_size = data$max_length,
                                             name = "average_pop") %>%
    keras::layer_lambda(function(x){
      x * data$max_length
    },
    name = "agg_pop")

  inv_agg_pop <- keras::layer_lambda(agg_pop,
                                     function(x){
                                       1 / x
                                     },
                                     name = "inv_agg_pop")

  output_rate <- keras::layer_multiply(c(output_agg,
                                         inv_agg_pop),
                                       name = "output_rate")

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
    output_xy <- output_disag
  }

  train_model <- keras:: keras_model(
    inputs = c(input_cov,
               input_pop,
               input_xy,
               input_xy_norm),
    outputs = c(output_agg),
    name = "train_model"
  )

  if(!is.null(loss)){
    train_model %>% keras::compile(
      optimizer,
      loss,
      metrics,
      ...
    )
  }

  predict_model <- keras::keras_model(
    inputs = c(input_cov,
               input_pop,
               input_xy,
               input_xy_norm),
    output = c(output_disag,
               output_agg,
               output_xy,
               output_rate),
    name = "predict_model"
  )

  kd_model <- list(train_model = train_model,
                   predict_model = predict_model,
                   layers_cov = layers_cov,
                   layers_xy = layers_xy,
                   data = data,
                   call = match.call())
  class(kd_model) <- c("kd_model", "list")
  return(kd_model)
}
