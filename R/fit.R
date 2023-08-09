#' Train a Kedis model
#'
#' @param object Model to train. A kd_model object.
#' @param x Passed to fit.keras.engine.training.Model. A vector, matrix or array of training data.
#' @param y Passed to fit.keras.engine.training.Model. A vector, amtrix or array of target (label) data.
#' @param batch_size Passed to fit.keras.engine.training.Model. Integer or NULL. Number of samples per gradient update. If unspecified, batch_size will default to 32.
#' @param epochs Number of epochs to train the model. Note that in conjunction with initial_epoch, epochs is to be understood as "final epoch". The model is not trained for a number of iterations given by epochs, but merely until the epoch of index epochs is reached.
#' @param verbose Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch).
#' @param callbacks List of callbacks to be called during training.
#' @param view_metrics View realtime plot of training metrics (by epoch). The default ("auto") will display the plot when running within RStudio, metrics were specified during model compile(), epochs > 1 and verbose > 0. Use the global keras.view_metrics option to establish a different default.
#' @param validation_split Float between 0 and 1. Fraction of the training data to be used as validation data. The model will set apart this fraction of the training data, will not train on it, and will evaluate the loss and any model metrics on this data at the end of each epoch. The validation data is selected from the last samples in the x and y data provided, before shuffling.
#' @param validation_data Data on which to evaluate the loss and any model metrics at the end of each epoch. The model will not be trained on this data. This could be a list (x_val, y_val) or a list (x_val, y_val, val_sample_weights). validation_data will override validation_split.
#' @param shuffle Logical (whether to shuffle the training data before each epoch) or string (for "batch"). "batch" is a special option for dealing with the limitations of HDF5 data; it shuffles in batch-sized chunks. Has no effect when steps_per_epoch is not NULL.
#' @param initial_epoch Integer, Epoch at which to start training (useful for resuming a previous training run).
#' @param steps_per_epoch Total number of steps (batches of samples) before declaring one epoch finished and starting the next epoch. When training with input tensors such as TensorFlow data tensors, the default NULL is equal to the number of samples in your dataset divided by the batch size, or 1 if that cannot be determined.
#' @param validation_steps Only relevant if steps_per_epoch is specified. Total number of steps (batches of samples) to validate before stopping.
#' @param predictions Boolean. Should predictions be made at each epoch. Useful to visualise training. Very slow. Defaults to FALSE.
#' @param ... Passed to fit.keras.engine.training.Model. Unused.
#'
#' @return A kd_history object that contains all information collected during training.
#' @export
fit.kd_model <- function(object,
                         x = object$inputs,
                         y = object$outputs,
                         batch_size = NULL,
                         epochs = 1000,
                         verbose = getOption("keras.fit_verbose", default = "auto"),
                         callbacks = NULL,
                         view_metrics = getOption("keras.view_metrics", default = "auto"),
                         validation_split = 0,
                         validation_data = NULL,
                         shuffle = TRUE,
                         initial_epoch = 0,
                         steps_per_epoch = NULL,
                         validation_steps = NULL,
                         predictions = FALSE,
                         ...){

  if(predictions){
    pred_disag <- list()
    pred_agg <- list()
    pred_xy <- list()
    epoch_names <- NULL

    callbacks <- append(keras::callback_lambda(on_epoch_end = function(epoch, logs){

      prediction <- predict.kd_model(object, verbose = 0, as_terra = FALSE)

      if(is.null(epoch_names)){
        epoch_names <<- epoch
        pred_disag <<- prediction$output_disag
        pred_agg <<- prediction$output_agg
        if(!is.null(object$layers_xy))
          pred_xy <<- prediction$output_xy
      } else {
        epoch_names <<- c(epoch_names, epoch)
        pred_disag <<- cbind(pred_disag, prediction$output_disag[, 3])
        pred_agg <<- cbind(pred_agg, prediction$output_agg)
        if(!is.null(object$layers_xy))
          pred_xy <<- cbind(pred_xy, prediction$output_xy[, 3])
      }


    }),
    callbacks)
  }

  # Change to NextMethod("fit") once packaged
  history <- keras::fit(object = object$train_model,
                        x = x,
                        y = y,
                        batch_size = batch_size,
                        epochs = epochs,
                        verbose = verbose,
                        callbacks = callbacks,
                        view_metrics = view_metrics,
                        validation_split = validation_split,
                        validation_data = validation_data,
                        shuffle = shuffle,
                        class_weight = NULL,
                        sample_weight = NULL,
                        initial_epoch = initial_epoch,
                        steps_per_epoch = steps_per_epoch,
                        validation_steps = validation_steps,
                        ...)

  if(predictions){
    pred_disag <- terra::rasterize(pred_disag[, c(1, 2)], object$rasters, values = pred_disag[, -c(1, 2)])
    pred_disag <- stats::setNames(pred_disag, epoch_names)

    colnames(pred_agg) <- epoch_names
    out <- object$shape
    sapply(colnames(pred_agg), function(n){
      out[[n]] <<- pred_agg[, n]
    })
    pred_agg <- out

    history$pred_disag <- pred_disag
    history$preg_agg <- pred_agg

    if(!is.null(object$layers_xy)){
      pred_xy <- terra::rasterize(pred_xy[, c(1, 2)], object$rasters, values = pred_xy[, -c(1, 2)])
      pred_xy <- stats::setNames(pred_xy, epoch_names)
      rtn$pred_xy <- pred_xy
    }
  }
  class(history) <- c("kd_history", class(history))
  return(history)
}
