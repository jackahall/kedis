pad_zeros <- function(df, length, pad = 0){
  rbind(as.matrix(df),
        matrix(0, nrow = length - nrow(df), ncol = ncol(df)))
}

