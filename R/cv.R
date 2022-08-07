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
#' @param ... additional parameters passed to kedis::train
#'
#' @return a kd_cv object
#' @export
cv.kd_model <- function(object, k = NA, idx = NULL, seed = NULL, ...){
  stopifnot(inherits(object, "kd_model"))
  if(!is.null(seed)){
    stopifnot(inherits(seed, "numeric"))
    set.seed(seed)
  }
  if(is.null(idx)){
    stopifnot(!is.na(k))
    stopifnot(inherits(k, "numeric"))
    idx <- make_index(object$data, k, seed = seed)
  } else {
    stopifnot(inherits(idx, "kd_cv_index"))
    k <- max(idx)
  }
  history <- list()
  prediction <- list()
  sub_data <- list()
  model <- list()
  loss <- list()
  for(i in seq_len(k)){
    sub_data[[i]] <- get_subset(object$data, idx, i)
    model[[i]] <- clone_model(object)
    history[[i]] <- train(model[[i]],
                          sub_data[[i]]$train,
                          validation_data = sub_data[[i]]$test,
                          ...)
    prediction[[i]] <- predict(model[[i]]$predict_model, sub_data[[i]]$test$inputs)
    loss[[i]] <- loss(model[[i]], sub_data[[i]]$test)
  }
  rtn <- list(prediction = prediction,
              history = history,
              idx = idx,
              k = k,
              sub_data = sub_data,
              loss = loss,
              models = model,
              call = match.call())
  class(rtn) <- c("kd_cv", class(rtn))
  rtn
}

