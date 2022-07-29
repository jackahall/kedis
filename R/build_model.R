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
#' @param layers_cov list of layers for covariate training
#' @param layers_xy list of layers for xy training
#' @param inverse_link_function inverse link function, defaults to exponential
#' @param optimizer keras optimizer
#' @param loss keras loss, if included model will compile
#' @param metrics keras metrics
#' @param ... other parameters to pass to keras::compile
#' @param seed seed, default NULL
#' @param clear_session default TRUE, clears keras session before running
#'
#' @return a kd_model object
#' @export
build_model.kd_data <- function(data,
                                layers_cov = NULL,
                                layers_xy = NULL,
                                inverse_link_function = function(x){exp(x)},
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

      if(inherits(layer, "keras.layers.regularization.dropout.Dropout")){
        if(layer$rate == 0){
          x
        } else {
          x %>% layer
        }
      }

      else if (inherits(layer, "keras.layers.core.dense.Dense")){
        if(layer$units == 0){
          x
        } else {
          x %>% layer
        }
      }

      else {
        x
      }
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

  training_layers_cov <- input_cov
  for(i in 1:length(layers_cov)){
    training_layers_cov <- training_layers_cov %>%
      add_layer(layers_cov[[i]])
  }
  training_layers_cov <- training_layers_cov %>%
    keras::layer_dense(units = 1, name = "training_layers_cov_final")

  if(!is.null(layers_xy)){
    training_layers_xy <- input_xy
    for(i in 1:length(layers_xy)){
      training_layers_xy <- training_layers_xy %>%
        add_layer(layers_xy[[i]])
    }
    training_layers_xy <- training_layers_xy %>%
      keras::layer_dense(units = 1, name = "training_layers_xy_final")
    spatial_summing_layer <- keras::layer_add(c(training_layers_cov,
                                                training_layers_xy),
                                              name = "spatial_summing_layer")
  } else {
    spatial_summing_layer <- training_layers_cov
  }

  link_layer <- spatial_summing_layer %>%
    keras::layer_lambda(inverse_link_function,
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

  output_disagg <- keras::layer_concatenate(c(link_layer,
                                              input_xy),
                                            axis = -1,
                                            name = "output_disagg")
  if(!is.null(layers_xy)){
    output_xy <- keras::layer_concatenate(c(training_layers_xy,
                                            input_xy),
                                          axis = -1,
                                          name = "output_xy")
  } else {
    output_xy <- output_disagg
  }

  train_model <- keras:: keras_model(
    inputs = c(input_cov,
               input_pop,
               input_xy),
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
               input_xy),
    output = c(output_disagg,
               output_agg,
               output_xy,
               output_rate),
    name = "predict_model"
  )

  kd_model <- list(train_model = train_model,
                   predict_model = predict_model,
                   layers_cov = layers_cov,
                   layers_xy = layers_xy,
                   data = data)
  class(kd_model) <- c("kd_model", "list")
  return(kd_model)
}
