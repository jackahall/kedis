#' Get subset of kd_data
#'
#' @param data a kd_data object
#' @param idx a kd_index object
#' @param n_out outer loop
#' @param ... additional parameters
#'
#' @return either a kd_cv_data or a kd_ncv_data, depending on the index and n_in
#' @export
get_subset <- function(data, idx, n_out, ...){
  UseMethod("get_subset", idx)
}

#' Get subset of kd_data with kd_ncv_index
#'
#' @param data a kd_data object
#' @param idx a kd_ncv_index object
#' @param n_out outer loop
#' @param n_in inner loop
#' @param ... additional parameters
#'
#' @return a kd_ncv_data object, which is a list of kd_data objects
#' @export
get_subset.kd_ncv_index <- function(data, idx, n_out, n_in, ...){
  rtn <- get_subset.kd_cv_index(data, idx$outer, n_out)
  rtn$train <- get_subset.kd_cv_index(rtn$train, idx$inner[[n_out]], n_in)
  class(rtn) <- c("kd_ncv_data", "list")
  rtn
}

#' Get subset of kd_data with kd_cv_index
#'
#' @param data a kd_data object
#' @param idx a kd_cv_index object
#' @param n_out outer loop
#' @param ... additional parameters
#'
#' @return a kd_cv_data object, which is a list of kd_data objects
#' @export
get_subset.kd_cv_index <- function(data, idx, n_out, ...){
  train <- kedis::prepare_data(data$raw$shapes[which(idx != n_out), ],
                               data$raw$covariates,
                               data$raw$population,
                               data$names$filter_var,
                               data$names$response_var,
                               data$names$covariates,
                               max_length = data$length_pad)

  test <- kedis::prepare_data(data$raw$shapes[which(idx == n_out), ],
                              data$raw$covariates,
                              data$raw$population,
                              data$names$filter_var,
                              data$names$response_var,
                              data$names$covariates,
                              max_length = data$length_pad)

  rtn <- list(train = train,
              test = test,
              full = data,
              size = length(which(idx != n_out)))
  class(rtn) <- c("kd_cv_data", "list")
  rtn
}
