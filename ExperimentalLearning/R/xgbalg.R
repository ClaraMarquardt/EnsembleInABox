#----------------------------------------------------------------------------#

#' @title *Fit a gradient boosted tree (classification regression) to a subset of the data and return predictions for (i) a (different) subset of the data and (ii) the original data.
#' 
#' @description *Gradient boosted tree constructed using xgb.train() [xgboost]. Observation weights (weight_var) are handled internally (method: Observation with heigher weights are selected with higher probability in the bootstrap samples used to construct the individual learners) in the training stage (not taken into account at the prediction stage).
#' 
#' @export
#' @import ranger
#' @param df *Datatable containing the data [data.table].
#' @param LHS *Name of the independent variable [character].
#' @param RHS *Names of all dependent variables (may incl. group_var, control_var and id_var) [character].
#' @param from *Observations on which to train an algorithm [integer].
#' @param to *Observations on which to predict [integer].
#' @param task *The nature of the prediction task  [regression (regression), classification (classification)].
#' @param weight *Vector of observation weights [integer or numeric].
#' @param alg_param *Tuning parameters [list - misc].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @param alg_mode *Algorithm specific argument specifying different excecution modes [character].
#' @param losstype *Loss function used to tune the individiual learners and the ensemble neg_mse_brier, loglik, auc, accuracy, balanced_accuracy, r2].
#' @return *Vector of predictions (predicted value of Y  OR  predicted probability of Y==1) [pred pred_is] and the fitted learner [learner].
#' @examples

xgbalg <- function(df, LHS, RHS,from, to, weight_var=ExpLearning.Default$weight_var, 
  task=ExpLearning.Default$task, alg_param=ExpLearning.MLParameter[["xgb_param"]][[1]], 
  quiet=ExpLearning.Default$quiet, alg_mode=ExpLearning.Default$alg_mode,
  losstype=ExpLearning.Default$losstype, ...) {


  if (alg_mode=="default") {

    # status
    if(quiet==0) {
      cat(paste0("XGB with ", paste(c(rbind(names(alg_param), ": ", unlist(alg_param), " / ")),
        collapse = ""), "\n"))
      start_time_sub <- Sys.time()
    }

    # parameters
    alg_param$colsample_bytree <- ifelse(alg_param$colsample_bytree * length(RHS)<1, 1, 
                                      alg_param$colsample_bytree)

    # data
    df_temp <- copy(df)

    ## need to ensure that at least one numeric var
    if (sum(sapply(df_temp[, mget(c(RHS))], class)=="integer")>0) {
      integer_var <- RHS[sapply(df_temp[, mget(c(RHS))], class)=="integer"]
      df_temp[, c(integer_var):=lapply(.SD, function(x) as.numeric(x)), 
        .SDcols=integer_var]
    }
   

    df_temp_data <- xgb.DMatrix(data.matrix(df_temp[from, mget(c(RHS))]), 
                        label=data.matrix(df_temp[from, get(c(LHS))]), 
                        weight=df_temp[from, get(weight_var)], 
                        missing=NaN)
       
    # task
    if (task=="regression") {
      alg_param$objective <- "reg:linear"
    } else if (task=="classification") {
      alg_param$objective <- "binary:logistic"
    }
   
    # execute algorithm
    thisxgb <- xgb.train(data=df_temp_data, params=alg_param[!(names(alg_param) %in% c("nrounds", "objective"))], 
                          prediction=TRUE, nrounds=alg_param$nrounds, 
                          verbose=0, objective=alg_param$objective, nthread=200) 
   
    df_temp_data    <- xgb.DMatrix(data.matrix(df_temp[to, mget(c(RHS))]))
    df_temp_data_is <- xgb.DMatrix(data.matrix(df_temp[from, mget(c(RHS))]))
                 

    # generate predictions
    pred    <- predict(thisxgb, df_temp_data, missing=NaN)    
    pred_is <- predict(thisxgb, df_temp_data_is, missing=NaN)    
   
    # status
    if(quiet==0) { cat(paste0("Done in ", Sys.time() - start_time_sub, "\n")) }
    
    return(list(pred=pred, learner=thisxgb, pred_is=pred_is))


  } else if (alg_mode=="tree_cv") {

    # status
    if(quiet==0) {
      cat(paste0("XGB (Tree CV)\n"))
      start_time_sub <- Sys.time()
    }

    # confirm that losstype is valid/supported
    assert("valid losstype (xgb() - tree_cv)", losstype %in% c("neg_mse_brier", "loglik", "auc", 
      "accuracy", "balanced_accuracy"))

    # prepare parameters
    eta_range <- c(unlist(sapply(alg_param, function(x) x$eta)))
    alg_param <- alg_param[[1]]

    # parameters
    alg_param$colsample_bytree <- ifelse(alg_param$colsample_bytree * length(RHS)<1, 1, 
                                      alg_param$colsample_bytree)

    # data
    df_temp <- copy(df)

    ## need to ensure that at least one numeric var
    if (sum(sapply(df_temp[, mget(c(RHS))], class)=="integer")>0) {
      integer_var <- RHS[sapply(df_temp[, mget(c(RHS))], class)=="integer"]
      df_temp[, c(integer_var):=lapply(.SD, function(x) as.numeric(x)), 
        .SDcols=integer_var]
    }

    df_temp_data <- xgb.DMatrix(data.matrix(df_temp[from, mget(c(RHS))]), 
                        label=data.matrix(df_temp[from, get(c(LHS))]), 
                        weight=df_temp[from, get(weight_var)], 
                        missing=NaN)

    # obtain folds (pre-defined)
    folds <- lapply(unique(df_temp[from,]$inner_fold), 
      function(x) which(df_temp[from, get("inner_fold")] == x))

    # obtain the xgb loss
    if (losstype=="auc") {
      losstype_xgb <- "auc"
    } else if (losstype=="neg_mse_brier") {
      losstype_xgb <- "rmse"
    } else if (losstype=="loglik") {
      losstype_xgb <- "logloss"
    } else if (losstype %in% c("balanced_accuracy","accuracy")) {
      losstype_xgb <- "error"
    }

    # task
    if (task=="regression") {
      alg_param$objective <- "reg:linear"
    } else if (task=="classification") {
      alg_param$objective <- "binary:logistic"
    }


    # execute algorithm
    cv_xgb_list <- lapply(1:length(eta_range), function(j) {

      if (quiet==0.5 & j %in% seq(1,length(eta_range),5)) cat(sprintf("Tuning - %d/%d\n", 
          j, length(eta_range)))

      eta <- eta_range[[j]]

      thisxgb <- xgb.cv(data=df_temp_data, params=alg_param[!(names(alg_param) %in% c("nrounds", 
                        "early_stopping", "objective", "eta"))], nrounds=alg_param$nrounds_max, folds=folds, 
                        early_stopping=alg_param$early_stopping, eta=eta,
                        metrics=losstype_xgb, verbose=ifelse(quiet==0.5,1,0), nthread=200,
                        objective=alg_param$objective)

      # obtain the number of trees
      tree_opt <- thisxgb$best_iteration
      cv_loss  <- thisxgb$evaluation_log[, get(grep("test_.*_mean", names(thisxgb$evaluation_log), 
                    value=T))][thisxgb$best_iteration]

      # format loss
      if (losstype=="auc") {
        cv_loss <- cv_loss
      } else if (losstype=="neg_mse_brier") {
        cv_loss <- (cv_loss^2) * (-1)
      } else if (losstype=="loglik") {
        cv_loss <- cv_loss * (-1)
      } else if (losstype %in% c("balanced_accuracy","accuracy")) {
        cv_loss <- 1-cv_loss
      } 

      return(list(cv_loss=cv_loss, tree_opt=tree_opt))

    })

    pick_me         <- which.max(c(unlist(sapply(cv_xgb_list, function(x) x$cv_loss))))

    pick_me_nrounds <- c(unlist(sapply(cv_xgb_list, function(x) x$tree_opt)))[pick_me]
    pick_me_eta     <- eta_range[pick_me]

    cv_loss         <- c(unlist(sapply(cv_xgb_list, function(x) x$cv_loss)))
    
    # status
    if(quiet==0) { cat(paste0("Done in ", Sys.time() - start_time_sub, "\n")) }

    if (quiet==0.5) {
      cat(sprintf("\neta - %s\n", paste0(eta_range, collapse=" - ")))
      cat(sprintf("tree_opt - %s\n", paste0(c(unlist(sapply(cv_xgb_list, function(x) 
        x$tree_opt))), collapse=" - ")))
      cat(sprintf("cv loss - %s\n\n", paste0(c(unlist(sapply(cv_xgb_list, function(x) 
        x$cv_loss))), collapse=" - ")))
    }

    return(list(mean_loss=cv_loss,pick_me=pick_me, pick_me_eta=pick_me_eta, 
      pick_me_nrounds=pick_me_nrounds))

  }

}

#----------------------------------------------------------------------------#

    




