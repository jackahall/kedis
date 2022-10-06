#' Cross-Validation Generic
#'
#' @param object object to cross-validate
#' @param ... additional parameters
#'
#' @export
cv <- function(object, ...){
  UseMethod("cv")
}


#' Cross-Validation of a kd_model object
#'
#' @param object a kd_model object
#' @param k k, must include if idx is NULL
#' @param idx a kd_cv_index object, must include if k is NA
#' @param seed seed
#' @param loss the loss function, passed to kedis::loss
#' @param silent omit all output
#' @param ... additional parameters passed to kedis::train
#' @param data data. default data from object. must be a kd_data object.
#'
#' @return a kd_cv object
#' @export
cv.kd_model <- function(object, data = object$data, k = NA, idx = NULL,
                        seed = NULL, loss = "poisson", silent = FALSE, ...){
  stopifnot(inherits(object, "kd_model"))
  if(!is.null(seed)){
    stopifnot(inherits(seed, "numeric"))
    set.seed(seed)
  }
  if(is.null(idx)){
    stopifnot(!is.na(k))
    stopifnot(inherits(k, "numeric"))
    idx <- make_index(data, k, seed = seed)
  } else {
    stopifnot(inherits(idx, "kd_cv_index"))
    k <- max(idx)
  }
  init_weights <- keras::get_weights(object$predict_model)
  history <- list()
  sub_data <- list()
  loss_history <- list()
  for(i in seq_len(k)){
    if(!silent) cat("\nFold", i)
    sub_data[[i]] <- get_subset(data, idx, i)
    history[[i]] <- train(object,
                          sub_data[[i]]$train,
                          validation_data = sub_data[[i]]$test,
                          ...)
    loss_history[[i]] <- loss(object, sub_data[[i]]$test, loss)
    if(!silent) cat("\tValidation Loss:", loss_history[[i]]$difference, "\tElapsed Time:", history[[i]]$exec_time["elapsed"])
    keras::set_weights(object$predict_model, init_weights)
  }

  rtn <- list(history = history,
              idx = idx,
              k = k,
              loss = cbind(k = seq_len(k), do.call(rbind, loss_history)),
              call = match.call())
  class(rtn) <- c("kd_cv", class(rtn))
  rtn
}

