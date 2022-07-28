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

#' Nested Cross Validation for Kedis
#'
#' @param data a kd_data object
#' @param n_out_loop number of outer loops
#' @param n_in_loop number of inner loops
#' @param hypers list of hyperparameters
#' @param seed seed
#' @param csv_folder folder address to save results csv to. if na, will not save, but return a data frame
#' @param ... additional parameters
#'
#' @return a data.frame and a kd_ncv_index object
#' @export
ncv.kd_data <- function(data, n_out_loop, n_in_loop, hypers, seed, csv_folder = NA, ...){

  kd_wrapper <- function(data_full, data_train, data_validate, layers_cov, seed){

    kd_model <- build_model(data_full,
                            layers_cov,
                            optimizer = optimizer_rmsprop(),
                            loss = loss_poisson(),
                            metrics = "poisson",
                            seed = seed)

    kd_history <- train(kd_model,
                        data_train,
                        epochs = 10000,
                        verbose = 0,
                        callbacks = list(callback_early_stopping(monitor = "poisson",
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

  n_models <- n_out_loop * ((length(hypers) * n_in_loop) + 1)

  train_seed <- sample(10000000, n_models)

  cat("This nested cross-validation will train", crayon::red(n_models), "models")

  for(out_loop in 1:n_out_loop){
    cat(crayon::red("\n\nOuter Loop", out_loop))
    for(hyper_idx in 1:length(hypers)){
      cat(crayon::blue(paste0("\nHyperparameter Set ", hyper_idx, ":")),
          crayon::green(paste0("\n", kedis::hypers_str(hypers[[hyper_idx]]))))
      for(in_loop in 1:n_in_loop){

        if(out_loop == 1){
          overview <- list()
          if(hyper_idx == 1){
            if(in_loop == 1){
              loss_archive <- data.frame()
              seed_idx <- 1
            }
          }
        }




        cat("\nOuter Loop:", out_loop, "\tHypers Set:", hyper_idx, "\tInner Loop:", in_loop, "\t")

        sub_data <- kedis::get_subset(data, idx, out_loop, in_loop)

        model <- kd_wrapper(sub_data$train$full, sub_data$train$train, sub_data$train$test, hypers[[hyper_idx]], train_seed[seed_idx])


        loss <- data.frame(as.list(model$loss))
        loss_archive <- rbind(loss_archive,
                              cbind(out_loop = out_loop,
                                    hyper_idx = hyper_idx,
                                    in_loop = in_loop,
                                    hyper_str = kedis::hypers_str(hypers[[hyper_idx]]),
                                    covariates = paste(sub_data$full$names$covariates, collapse=","),
                                    set = "train",
                                    tf_seed = train_seed[seed_idx],
                                    outer_seed = seed,
                                    loss))
        seed_idx <- seed_idx + 1
        cat(paste("Loss Difference:", round(loss[3], 2), "\tExec Time:", round(model$history$exec_time[3], 2)))

      }

      average_loss <- loss_archive %>%
        dplyr::filter(out_loop == !!out_loop & hyper_idx == !!hyper_idx) %>%
        dplyr::group_by(hyper_idx) %>%
        dplyr::summarize("Mean_Difference" = mean(Difference)) %>%
        dplyr::select(Mean_Difference) %>%
        as.numeric

      cat(paste0("\nAverage loss for ", out_loop, "-", hyper_idx, ": ", crayon::blue(round(average_loss, 2))))

    }

    best_hyper_set <- loss_archive %>%
      dplyr::filter(out_loop == !!out_loop) %>%
      dplyr::group_by(hyper_idx) %>%
      dplyr::summarize("Mean_Difference" = mean(Difference)) %>%
      dplyr::summarize("Min_Mean_Difference_Location" = hyper_idx[which.min(Mean_Difference)]) %>%
      as.numeric

    cat(crayon::magenta("\nTraining optimal hyperparameter set on outer loop"),
        crayon::green(paste0("\n", kedis::hypers_str(hypers[[best_hyper_set]]))))
    cat("\nOuter Loop:", out_loop, "\tHypers Set:", best_hyper_set, "\tInner Loop: NA", "\t")

    model <- kd_wrapper(sub_data$full, sub_data$train$full, sub_data$test, hypers[[best_hyper_set]], train_seed[seed_idx])


    loss <- data.frame(as.list(model$loss))
    loss_archive <- rbind(loss_archive,
                          cbind(out_loop = out_loop,
                                hyper_idx = best_hyper_set,
                                in_loop = NA,
                                hyper_str = kedis::hypers_str(hypers[[best_hyper_set]]),
                                covariates = paste(sub_data$full$names$covariates, collapse=","),
                                set = "test",
                                tf_seed = train_seed[seed_idx],
                                outer_seed = seed,
                                loss))
    seed_idx <- seed_idx + 1

    cat(paste("Loss Difference:", round(loss[3], 2), "\tExec Time:", round(model$history$exec_time[3], 2)))

  }

  test_loss <- loss_archive %>%
    dplyr::filter(set == "test") %>%
    dplyr::summarize(test_loss = mean(Difference)) %>%
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
       idx = idx)
}
