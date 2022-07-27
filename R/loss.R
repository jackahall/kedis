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
#' @param full return the full dataframe, default FALSE
#' @param min include the minimum (optimum) loss
#'
#' @return a data.frame
#' @export
loss.kd_model <- function(model, data = model$data, full = FALSE, min = TRUE){
  df <- data.frame(cbind(
    predict(model$predict_model, data$inputs, verbose = 0)[[2]],
    data$outputs[[1]])) %>%
    stats::setNames(c("Predicted", "Actual")) %>%
    dplyr::mutate("Poisson_Loss" = (Predicted - Actual * log(Predicted)),
                  "Min_Poisson_Loss" = (Actual - Actual * log(Actual)),
                  "Difference" = Poisson_Loss - Min_Poisson_Loss)

  if(full){
    if(min){
      df
    } else {
      df %>% dplyr::select(-c("Min_Poisson_Loss", "Difference"))
    }
  } else {
    if(min){
      df %>%
        dplyr::select(-c("Predicted", "Actual")) %>%
        colSums %>%
        `/`(nrow(df))
    } else{
      colSums(df)["Poisson_Loss"] %>%
        `/`(nrow(df))
    }
  }

}

