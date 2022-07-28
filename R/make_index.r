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

  y <- data$shapes
  name = data$names$response_var

  outer = caret::createFolds(y %>%
                               terra::as.data.frame() %>%
                               dplyr::select(out = dplyr::all_of(name)) %>%
                               .$out,
                             k = k_out,
                             list = FALSE)

  if(!is.null(k_in)){

    idx <- list(outer = outer,
                inner = list())

    for(i in 1:k_out){
      y <- y[which(outer == i),]

      idx$inner[[i]] <- caret::createFolds(y %>%
                                             terra::as.data.frame() %>%
                                             dplyr::select(out = dplyr::all_of(name)) %>%
                                             .$out,
                                           k = k_in,
                                           list = FALSE)
      class(idx) <- c("kd_ncv_index", "kd_index", "list")
    }
  } else {
    idx <- outer
    class(idx) <- c("kd_cv_index", "kd_index", "list")
  }

  return(idx)
}
