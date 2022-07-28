#' Kedis hyperparameters to string
#'
#' @param hypers lsit of keras layers
#'
#' @return a string
#' @export
hypers_str <- function(hypers){
  layer_attr_to_str <- function(layer){
    if(inherits(layer, "keras.layers.core.dense.Dense")){
      str <- paste0("dense(units=", layer$units, ")")
    } else if (inherits(layer, "keras.layers.regularization.dropout.Dropout")){
      str <- paste0("dropout(rate=", layer$rate, ")")
    } else {
      stop()
    }
    str
  }
  paste(sapply(hypers, layer_attr_to_str), collapse="|")
}
