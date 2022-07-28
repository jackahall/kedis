#' make_index generic
#'
#' @param data data
#' @param ... additional parameters
#'
#' @export
make_index <- function(data, ...){
  UseMethod("make_index")
}


#' Make index for kedis cross validation
#'
#' @param data a kd_data object
#' @param k_out number of outer loops
#' @param k_in number of inner loops (NULL for non-nested cross-validation, default)
#' @param seed seed
#' @param ... additional parameters
#'
#' @return a kd_index object
#' @export
make_index.kd_data <- function(data, k_out, k_in = NULL, seed = NULL, ...){

  if(!is.null(seed)) set.seed(seed)

  name <- data$names$response_var
  y <- data$response %>%
    dplyr::pull(dplyr::all_of(name))

  outer = caret::createFolds(y,
                             k = k_out,
                             list = FALSE)

  class(outer) <- c("kd_cv_index", "kd_index", "list")

  if(!is.null(k_in)){

    idx <- list(outer = outer,
                inner = list())

    for(i in 1:k_out){
      y_in <- y[which(outer != i)]

      idx$inner[[i]] <- caret::createFolds(y_in,
                                           k = k_in,
                                           list = FALSE)
      class(idx$inner[[i]]) <- c("kd_cv_index", "kd_index", "list")
      class(idx) <- c("kd_ncv_index", "list")
    }
  } else {
    idx <- outer
  }

  return(idx)
}
