#' get_subset generic
#'
#' @param data data to subset
#' @param ... additional parameters
#'
#' @export
get_subset <- function(data, ...){
  UseMethod("get_subset")
}

#' Get subset of kd_data
#'
#' @param data a kd_data object
#' @param idx a kd_index object
#' @param n_out number of outer loops
#' @param n_in number of inner loops (NULL for non-nested Cross-Validation)
#' @param ... additional parameters
#'
#' @return either a kd_cv_data or a kd_ncv_data, depending on the index and n_in
#' @export
get_subset.kd_data <- function(data, idx, n_out, n_in = NULL, ...){

  # Terrible classs handling, but it works for this quick use
  get_subset.kd_ncv_index <- function(data, idx, n_out, n_in){
    rtn <- get_subset.kd_cv_index(data, idx$outer, n_out)
    rtn$train <- get_subset.kd_cv_index(rtn$train, idx$inner[[n_out]], n_in)
    class(rtn) <- c("kd_ncv_data", "list")
    rtn
  }

  get_subset.kd_cv_index <- function(data, idx, n){
    train <- kedis::prepare_data(data$shapes[which(idx != n), ],
                                 data$covariates,
                                 data$population,
                                 data$names$filter_var,
                                 data$names$response_var,
                                 data$names$covariates,
                                 max_length = data$max_length)

    test <- kedis::prepare_data(data$shapes[which(idx == n), ],
                                data$covariates,
                                data$population,
                                data$names$filter_var,
                                data$names$response_var,
                                data$names$covariates,
                                max_length = data$max_length)

    rtn <- list(train = train,
                test = test,
                full = data,
                size = length(which(idx != n)))
    class(rtn) <- c("kd_cv_data", "list")
    rtn
  }

  stopifnot(inherits(idx, "kd_index"))
  if(inherits(idx, "kd_ncv_index")){
    if(is.null(n_in)){
      stop("n_in must be supplied for nested cross-validation")
    } else {
      get_subset.kd_ncv_index(data, idx, n_out, n_in)
    }
  } else if(inherits(idx, "kd_cv_index")) {
    get_subset.kd_cv_index(data, idx, n_out)
  } else {
    stop("idx not of correct class")
  }
}
