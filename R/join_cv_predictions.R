#' Joins Kedis Cross-Validation predicted rates with actual rates
#'
#' @param x a kd_cv object
#' @param ... additional parameters
#'
#' @return a data.frame
#' @export
join_cv_predictions <- function(x, ...){
  pred <- list()
  for(i in seq_len(x$k)){
    pred[[i]] <- cbind(
      predicted = x$prediction[[i]][[4]],
      actual = x$sub_data[[i]]$test$response$rate,
      fold = i)
  }
  do.call(rbind, pred) %>%
    as.data.frame()
}
