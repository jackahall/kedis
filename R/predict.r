#' Generate predictions from a Kedis model
#'
#' @param object Kedis model.
#' @param batch_size Integer. If unspecified, it will default to 32.
#' @param verbose Verbosity mode, 0, 1, 2, or "auto". "auto" defaults to 1 for for most cases and defaults to verbose=2 when used with ParameterServerStrategy or with interactive logging disabled.
#' @param steps Total number of steps (batches of samples) before declaring the evaluation round finished. Ignored with the default value of NULL.
#' @param callbacks List of callbacks to apply during prediction.
#' @param as_terra Convert prediciton to SpatRaster and SpatVector. Default TRUE.
#' @param ... Passed to predict.keras.engine.training.Model. Unused.
#'
#' @return A kd_prediction object.
#' @export
predict.kd_model <- function(object,
                             batch_size = NULL,
                             verbose = "auto",
                             steps = NULL,
                             callbacks = NULL,
                             as_terra = TRUE,
                             ...){

  pred <- stats::predict(object = object$predict_model,
                         x = object$inputs,
                         batch_size = batch_size,
                         verbose = verbose,
                         steps = steps,
                         callbacks = callbacks,
                         ...)

  pred_to_rast <- function(pred, var, object, name){
    out <- matrix(pred[[var]], ncol = 3)[, c(2, 3, 1)]
    out <- out[out[, 1] != 0 & out[, 2] != 0, ]
    if(as_terra){
      out <- terra::rasterize(out[, c(1, 2)], object$rasters, values = out[, 3])
      out <- stats::setNames(out, name)
    }
    return(out)
  }

  pred_to_vect <- function(pred, var, object, name){
    if(as_terra){
      out <- object$shape
      out[[name]] <- pred[[var]]
    } else {
      out <- pred[[2]]
    }
    return(out)
  }

  out <- list(output_disag = pred_to_rast(pred, 1, object, "output_disag"),
              output_agg = pred_to_vect(pred, 2, object, "output_agg"))

  if(!is.null(object$layers_xy)){
    out <- append(out,
                  list(output_xy = pred_to_rast(pred, 3, object, "output_xy")))
  }

  class(out) <- c("kd_prediction", class(out))
  return(out)
}




