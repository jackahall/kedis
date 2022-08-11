#' loss generic
#'
#' @param model the model object
#' @param ... additional parameters
#'
#' @export
loss <- function(model, ...){
  UseMethod("loss")
}



#' loss
#'
#' @param model a kd_model object
#' @param data a kd_data object, defaults to data from model
#' @param loss the loss function to apply. either a string "poisson" (default) which is the poisson loss, or any function which takes actual and predicted as parameters
#' @param ... additional parameters
#'
#' @return a data.frame
#' @export
loss.kd_model <- function(model, data = model$data, loss = "poisson", ...){
  if(inherits(loss, "function")){
    loss_fn <- loss
  } else if(loss == "poisson"){
    loss_fn <- function(actual, predicted){
      sum(predicted - actual * log(predicted))/length(actual)
    }
  } else {
    stop("Invalid loss function")
  }

  response <- compare_response(model, data)

  data.frame(loss = loss_fn(response$actual, response$predicted),
             optim = loss_fn(response$actual, response$actual)) %>%
    dplyr::mutate(difference = loss - optim)
}
