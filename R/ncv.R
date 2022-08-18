#' Nested cross Validation
#'
#' @param data data
#' @param ... additional parameters
#'
#' @return the results, duh...
#' @export
ncv <- function(data, ...){
  UseMethod("ncv")
}

#' DEPRECIATED Nested Cross Validation for Kedis
#'
#' @param data a kd_data object
#' @param n_out_loop number of outer loops
#' @param n_in_loop number of inner loops
#' @param hypers list of hyperparameters
#' @param seed seed
#' @param csv_folder folder address to save results csv to. if na, will not save, but return a data frame
#' @param ... additional parameters
#' @param out_loop which outer loop to run, defaults to all
#'
#' @return a data.frame and a kd_ncv_index object
#' @export
ncv.kd_data <- function(data, n_out_loop, n_in_loop, hypers, seed, csv_folder = NA, out_loop = NA, ...){

  kd_wrapper <- function(data_full, data_train, data_validate, layers_cov, seed){

    kd_model <- build_model(data_full,
                            layers_cov,
                            optimizer = keras::optimizer_rmsprop(),
                            loss = keras::loss_poisson(),
                            link = "log",
                            seed = seed)

    kd_history <- train(kd_model,
                        sub_data$train$test,
                        epochs = 10000,
                        verbose = 0,
                        callbacks = list(keras::callback_early_stopping(monitor = "loss",
                                                                        min_delta = 0.001,
                                                                        patience = 50,
                                                                        restore_best_weights = TRUE)),
                        validation_set = data_validate)

    kd_loss <- loss(kd_model, data_validate)

    rtn <- list(layers_cov = kd_model$layers_cov,
                history = kd_history,
                loss = kd_loss)
    rm(kd_model)
    keras::k_clear_session()
    invisible(gc())
    rtn
  }

  idx <- kedis::make_index(data, n_out_loop, n_in_loop, seed = seed)

  out_seq <- 1:n_out_loop
  if(!anyNA(out_loop)){
    if(all(out_loop %in% out_seq)){
      out_seq <- out_loop
    } else {
      stop("Outer loop must be within correct range")
    }
  }

  n_models <- length(out_seq) * ((length(hypers) * n_in_loop) + 1)

  cat("This nested cross-validation will train", crayon::red(n_models), "models")

  if(!is.null(seed)){
    set.seed(seed)
  }
  train_seed <- cbind(expand.grid(
    in_loop_ = c(1:n_in_loop, 0),
    hyper_idx_ = 1:length(hypers),
    out_loop_ = 1:n_out_loop),
    seed = sample(10000000,
                  size =  n_out_loop * length(hypers) * (n_in_loop+1),
                  replace = TRUE))

  loss_archive <- data.frame()
  overview <- list()

  for(out_loop in out_seq){
    cat(crayon::red("\n\nOuter Loop", out_loop))
    for(hyper_idx in 1:length(hypers)){
      cat(crayon::blue(paste0("\nHyperparameter Set ", hyper_idx, ":")),
          crayon::green(paste0("\n", kedis::hypers_str(hypers[[hyper_idx]]))))
      for(in_loop in 1:n_in_loop){

        inner_seed <-   train_seed %>%
          dplyr::filter(in_loop_ == in_loop,
                        hyper_idx_ == hyper_idx,
                        out_loop_ == out_loop) %>%
          dplyr::pull(seed)

        cat("\nSeed:", inner_seed)


        cat("\nOuter Loop:", out_loop, "\tHypers Set:", hyper_idx, "\tInner Loop:", in_loop, "\t")

        sub_data <- kedis::get_subset(data, idx, out_loop, in_loop)

        model <- kd_wrapper(sub_data$train$full,
                            sub_data$train$train,
                            sub_data$train$test,
                            hypers[[hyper_idx]],
                            inner_seed)


        loss <- data.frame(as.list(model$loss))
        loss_archive <- rbind(loss_archive,
                              cbind(out_loop = out_loop,
                                    hyper_idx = hyper_idx,
                                    in_loop = in_loop,
                                    hyper_str = kedis::hypers_str(hypers[[hyper_idx]]),
                                    covariates = paste(sub_data$full$names$covariates, collapse=","),
                                    set = "train",
                                    tf_seed = inner_seed,
                                    outer_seed = seed,
                                    loss))
        cat(paste("Loss Difference:", round(loss[3], 2), "\tExec Time:", round(model$history$exec_time[3], 2)))

      }

      average_loss <- loss_archive %>%
        dplyr::filter(out_loop == !!out_loop & hyper_idx == !!hyper_idx) %>%
        dplyr::group_by(hyper_idx) %>%
        dplyr::summarize("Mean_Difference" = mean(difference)) %>%
        dplyr::select(Mean_Difference) %>%
        as.numeric

      cat(paste0("\nAverage loss for ", out_loop, "-", hyper_idx, ": ", crayon::blue(round(average_loss, 2))))

    }

    best_hyper_set <- loss_archive %>%
      dplyr::filter(out_loop == !!out_loop) %>%
      dplyr::group_by(hyper_idx) %>%
      dplyr::summarize("Mean_Difference" = mean(difference)) %>%
      dplyr::summarize("Min_Mean_Difference_Location" = hyper_idx[which.min(Mean_Difference)]) %>%
      as.numeric

    inner_seed <-   train_seed %>%
      dplyr::filter(in_loop_ == 0,
                    hyper_idx_ == hyper_idx,
                    out_loop_ == out_loop) %>%
      dplyr::pull(seed)

    cat(crayon::magenta("\nTraining optimal hyperparameter set on outer loop"),
        crayon::green(paste0("\n", kedis::hypers_str(hypers[[best_hyper_set]]))))
    cat("\nOuter Loop:", out_loop, "\tHypers Set:", best_hyper_set, "\tInner Loop: NA", "\t")

    model <- kd_wrapper(sub_data$full, sub_data$train$full, sub_data$test, hypers[[best_hyper_set]],
                        inner_seed)


    loss <- data.frame(as.list(model$loss))
    loss_archive <- rbind(loss_archive,
                          cbind(out_loop = out_loop,
                                hyper_idx = best_hyper_set,
                                in_loop = NA,
                                hyper_str = kedis::hypers_str(hypers[[best_hyper_set]]),
                                covariates = paste(sub_data$full$names$covariates, collapse=","),
                                set = "test",
                                tf_seed = inner_seed,
                                outer_seed = seed,
                                loss))

    cat(paste("Loss Difference:", round(loss[3], 2), "\tExec Time:", round(model$history$exec_time[3], 2)))

  }

  test_loss <- loss_archive %>%
    dplyr::filter(set == "test") %>%
    dplyr::summarize(test_loss = mean(difference)) %>%
    as.numeric()

  cat(crayon::red("\n\nAverage loss difference of test sets:"), round(test_loss, 2))

  if(!is.na(csv_folder)){
    if(!dir.exists(csv_folder)){
      dir.create(csv_folder, recursive = TRUE)
    }
    filename <- paste0("ncv-", format(Sys.time(), "%y%m%d-%H%M%S"),".csv")
    cat("\n\nWriting .csv file:", file.path(csv_folder,
                                            filename))
    write.csv(loss_archive,
              file.path(csv_folder,
                        filename),
              row.names = TRUE)
  }

  list(loss_archive = loss_archive,
       idx = idx,
       train_seed = train_seed)
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
#' @param ... additional parameters to pass to train
#'
#' @export
ncv.kd_model <- function(model, data = model$data, n_out_loop, n_in_loop,
                         hypers, loss = "poisson", seed = NULL,
                         file_path = NULL, ...){

  models <- lapply(hypers, function(x){
    do.call(build_model, append(as.list(model$call)[-1], x))
  })

  if(!is.null(seed)) set.seed(seed)

  idx <- make_index(data, n_out_loop, n_in_loop)

  sub_data <- list()
  cv_history <- list()

  for(out_loop in seq_len(n_out_loop)){

    cv_history[[out_loop]] <- list()
    cat("\nOuter Loop:", out_loop)
    sub_data[[out_loop]] <- get_subset(data, idx, out_loop)

    for(hyper_idx in seq_along(hypers)){
      cat("\nHyper Idx:", hyper_idx)
      cv_history[[out_loop]][[hyper_idx]] <- cv(models[[hyper_idx]],
                                                data = sub_data[[out_loop]]$train,
                                                idx = idx$inner[[out_loop]])
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
    dplyr::slice(which.min(difference)) %>%
    dplyr::pull(hyper_idx) %>%
    suppressMessages()

  outer_history <- list()
  outer_models <- list()
  outer_losses <- list()

  for(out_loop in seq_len(n_out_loop)){
    cat("\nFitting outer model", out_loop)
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
    cat("\n\tValidation Loss",
        outer_losses[[out_loop]]$difference,
        "\tElapsed Time:",
        outer_history[[out_loop]]$exec_time["elapsed"])
  }

  if(!is.null(file_path)){
    if(!dir.exists(file_path)){
      dir.create(file_path, recursive = TRUE)
    }
    filename_suff <- paste0(format(Sys.time(), "%y%m%d-%H%M%S"),".csv")
    cat("\n\nWriting .csv files")
    write.csv(inner_losses,
              file.path(file_path,
                        paste0("inner_losses-ncv-", filename)),
              row.names = TRUE)
    write.csv(inner_losses,
              file.path(file_path,
                        paste0("outer_losses-ncv-", filename)),
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
