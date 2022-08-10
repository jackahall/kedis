#' Append rows of zeros to a data.frame
#'
#' @param df the dataframe
#' @param length total length after padding
#' @param pad value to pad with, default 0
#'
#' @export
pad_zeros <- function(df, length, pad = 0){
  rbind(as.matrix(df),
        matrix(0, nrow = length - nrow(df), ncol = ncol(df)))
}

