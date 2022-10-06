#' Generic clone a model instance.
#'
#' @param model model
#' @param input_tensors input_tensors
#' @param clone_function clone_function
#' @param ... additional parameters
#'
#' @export
clone_model <- function(model, input_tensors = NULL, clone_function = NULL, ...){
  UseMethod("clone_model")
}

#' Default clone a model instance.
#'
#' @param model model
#' @param input_tensors input_tensors
#' @param clone_function clone_function
#' @param ... additional parameters
#'
#' @export
clone_model.default <- function(model, input_tensors = NULL, clone_function = NULL, ...){
  keras::clone_model(model, input_tensors, clone_function, ...)
}

#' Clone a kd_model instance
#'
#' @param model a kd_model object
#' @param ... additional parameters
#'
#' @export
clone_model.kd_model <- function(model, ...){
  call <- model$call
  call[1]<- call("build_model")
  eval(call)
}
