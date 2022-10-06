#' Nested cross Validation
#'
#' @param model the model
#' @param ... additional parameters
#'
#' @return the results, duh...
#' @export
ncv <- function(model, ...){
  UseMethod("ncv")
}

#' Nested Cross-Validation for a Kedis Model
#'
#' @param model a kd_model to be used as template
#' @param data kd_data, if not supplied data from kd_model is used
#' @param n_out_loop number of outer loops
#' @param n_in_loop numer of inner loops
#' @param hypers list of hyperparameter sets
#' @param loss loss to calculate, passed to kedis::loss
#' @param seed random seed
#' @param file_path file path to save csv file. if NULL file will not be saved
#' @param silent omit all output
#' @param ... additional parameters to pass to train
#'
#' @export
ncv.kd_model <- function(model, data = model$data, n_out_loop, n_in_loop,
                         hypers, loss = "poisson", seed = NULL,
                         file_path = NULL, silent = FALSE, ...){

  models <- lapply(hypers, function(x){
    do.call(build_model, append(as.list(model$call)[-1], x))
  })

  if(!is.null(seed)) set.seed(seed)

  idx <- make_index(data, n_out_loop, n_in_loop)

  sub_data <- list()
  cv_history <- list()

  for(out_loop in seq_len(n_out_loop)){

    cv_history[[out_loop]] <- list()
    if(!silent) cat("\nOuter Loop:", out_loop)
    sub_data[[out_loop]] <- get_subset(data, idx, out_loop)

    for(hyper_idx in seq_along(hypers)){
      if(!silent) cat("\nHyper Idx:", hyper_idx)
      cv_history[[out_loop]][[hyper_idx]] <- cv(models[[hyper_idx]],
                                                data = sub_data[[out_loop]]$train,
                                                idx = idx$inner[[out_loop]],
                                                silent = silent, ...)
    }
  }

  cv_history_to_df <- function(x, ...){
    # This REALLY needs sorting out, but it works...
    FUN <- function(x, ...){
      hyper_idx <- 0
      lapply(x, function(x, ...){
        hyper_idx <<- hyper_idx + 1
        cbind(hyper_idx = hyper_idx,
              magrittr::extract2(x, ...))
      }, "loss")
    }

    FUN2 <- function(x, ...){
      out_loop <- 0
      lapply(x, function(x){
        out_loop <<- out_loop + 1
        cbind(out_loop = out_loop,
              do.call(rbind, FUN(x)))
      })
    }

    FUN3 <- function(x, ...){
      do.call(rbind, FUN2(x))
    }

    FUN3(x)
  }

  inner_losses <- cv_history_to_df(cv_history)

  best_hyper_sets <- inner_losses %>%
    dplyr::group_by(out_loop, hyper_idx) %>%
    dplyr::summarize(dplyr::across(dplyr::any_of(c("loss", "optim", "difference")), mean)) %>%
    dplyr::group_by(out_loop) %>%
    dplyr::slice(which.min(.data$difference)) %>%
    dplyr::pull(hyper_idx) %>%
    suppressMessages()

  outer_history <- list()
  outer_models <- list()
  outer_losses <- list()

  for(out_loop in seq_len(n_out_loop)){
    if(!silent) cat("\nFitting outer model", out_loop)
    outer_models[[out_loop]] <- clone_model(models[[best_hyper_sets[out_loop]]])
    outer_history[[out_loop]] <- train(outer_models[[out_loop]],
                                       data = sub_data[[out_loop]]$train,
                                       validation_data =  sub_data[[out_loop]]$test,
                                       ...)
    outer_losses[[out_loop]] <- cbind(out_loop = out_loop,
                                      hyper_idx = best_hyper_sets[out_loop],
                                      loss(outer_models[[out_loop]],
                                           sub_data[[out_loop]]$test,
                                           loss))
    if(!silent) {
      cat("\n\tValidation Loss",
        outer_losses[[out_loop]]$difference,
        "\tElapsed Time:",
        outer_history[[out_loop]]$exec_time["elapsed"])
    }
  }

  if(!is.null(file_path)){
    if(!dir.exists(file_path)){
      dir.create(file_path, recursive = TRUE)
    }
    filename_suff <- paste0(format(Sys.time(), "%y%m%d-%H%M%S"),".csv")
    if(!silent) cat("\n\nWriting .csv files")
    write.csv(inner_losses,
              file.path(file_path,
                        paste0("inner_losses-ncv-", filename_suff)),
              row.names = TRUE)
    write.csv(outer_losses,
              file.path(file_path,
                        paste0("outer_losses-ncv-", filename_suff)),
              row.names = TRUE)
  }

  rtn <- list(inner_history = cv_history,
              outer_history = outer_history,
              inner_losses = inner_losses,
              outer_losses = do.call(rbind, outer_losses),
              n_out_loop = n_out_loop,
              n_in_loop = n_in_loop,
              hypers = hypers,
              call = match.call())
  class(rtn) <- c("kd_ncv", class(rtn))
  rtn
}
