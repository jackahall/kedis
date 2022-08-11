#' Compare the actual response to the predicted
#'
#' @param model a kd_model object
#' @param data a kd_data object, defaults to the data from model

#' @export
compare_response <- function(model, data = model$data){
  predict(model, data, as.data.frame = TRUE)$output_agg %>%
    dplyr::select(actual = model$data$names$response_var, predicted = output_agg)
}
