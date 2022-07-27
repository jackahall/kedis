#' @importFrom generics train
#' @export
generics::train

#' train
#'
#' @param x kd_model
#' @param data kd_data
#' @param ... arguments to keras::fit
#' @param verbose verbosity, defaults to 0
#'
#' @return a kd_history object
#' @export
train.kd_model <- function(x, data = x$data, ..., verbose = 0){
  kd_exec_time <- system.time({
  kd_history <- x$train_model %>% keras::fit (
    x = data$inputs,
    y = data$outputs,
    verbose = verbose,
    ...
  )
  })
  kd_history$exec_time <- kd_exec_time
  class(kd_history) <- c("kd_history", "keras_training_history")
  return(kd_history)
}
