#' Summarize kd_model
#'
#' @param object a kd_model object
#' @param ... additional parameters
#'
#' @export
summary.kd_model <- function(object, ...){
  object <- object$train_model
  summary(object)
}

#' Summarize kd_data
#'
#' @param object a kd_data object
#' @param ... additional parameters
#'
#' @export
summary.kd_data <- function(object, ...){
  cat("Data for Keras-Disaggregation (kedis) model")
  cat("\nTotal number of regions:\t", nrow(object$response))
  cat("\nTotal number of pixels:\t\t", nrow(object$full_df))
  cat("\nLargest region:\t\t\t", object$max_length)
  cat("\nCovarites used:\t", paste(object$names$covariates, collapse = ", "))
}

#' Summarize kd_cv
#'
#' @param object a kd_cv object
#' @param ... additional parameters
#'
#' @export
summary.kd_cv <- function(object, ...){
  cat("\nCross-Validation of a Kedis model")
  cat("\nk =", object$k, "\n")

  cat("\nLosses per fold:\n")
  print(object$loss, row.names = FALSE)

  cat("\nMean Loss Difference: ")
  cat(object$loss %>%
          dplyr::summarize(mean = mean(difference)) %>%
            dplyr::pull(mean))
}

#' Summarise kd_ncv
#'
#' @param object a kd_ncv object
#' @param ... additional parameters
#
#' @export
print.kd_ncv <- function(object, ...){
  cat("\nNested Cross-Validation of Kedis Models")

  cat("\n\nOuter Folds:", object$n_out_loop,
      "\tInner Folds:", object$n_in_loop)
  cat("\nHyperparameter sets tested", length(object$hypers))

  cat("\n\nMean loss of outer loops:", mean(object$outer_losses$difference))
}
