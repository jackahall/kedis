#' Population Variable
#'
#' @description Defines the population variable to use in the kedis model.
#' @param x input variable
#' @export
pop <- function(x){
  name <- deparse(substitute(x))
  attr(x, "name") <- name
  x
}

#' Aggregation Variable
#'
#' @description Specifies the variable on the shapefile that the response is to be aggregated over.
#' @param x input variable
#' @export
agg <- function(x){
  name <- deparse(substitute(x))
  attr(x, "name") <- name
  x
}

#' Spatial Covariates
#'
#' @param x input variable.
#' @export
xy <- function(x){
  name <- deparse(substitute(x))
  attr(x, "name") <- name
  x
}

#' Normalised Spatial Covariates
#'
#' @param x input variable.
#' @export
xy_norm <- function(x){
  name <- deparse(substitute(x))
  attr(x, "name") <- name
  x
}


