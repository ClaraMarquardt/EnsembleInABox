#----------------------------------------------------------------------------#

#' @title *Construct an ensemble model using a training subset of the data and generate out of sample predictions based on this model.
#'
#' @description *.
#' 
#' @export
#' @param df *Datatable containing the data [data.table].
#' @param LHS *Name of the independent variable [character].
#' @param RHS *Names of all dependent variables (may incl. group_var, control_var and id_var) [character].
#' @param RHS_ext *Names of all dependent variables (may incl. group_var, control_var and id_var) [character].
#' @param cluster_var *Name of variable containing observation level cluster identifiers (integer) [character].
#' @param holdout *Whether or not to construct and use a holdout set [logical - TRUE, FALSE].
#' @param prediction *Vector of predictions [numeric].
#' @param id_var
#' @param predictor *List of algorithms included in ensemble learner [ols (linear regression), rf (random forest (classification OR regression)), avg (simple mean), elnet (elastic net), logit (logistic regression), tree (tree (classification or regression)), xgb (gradient boosted tree (classification or regression))] [character].
#' @param innerfold *Number of CV folds used in inner CV routine [integer].
#' @param weight *Vector of observation weights [integer or numeric].
#' @param fold_method *Method used to generate CV folds (and the train-holdout split) [cluster_count (target cluster count), obs_count (target observation count), stratification (target stratification)].
#' @param task *The nature of the prediction task  [regression (regression), classification (classification)].
#' @param fold_var *Name of variable containing observation level fold identifiers (integer) [character].
#' @param rand_grid_iter *Number of random grid search iterations (performed if tuning_method==""rand_grid"") [integer]. 
#' @param losstype *Loss function used to tune the individiual learners and the ensemble neg_mse_brier, loglik, auc, accuracy, balanced_accuracy, r2].
#' @param tuning_method *Tuning method [character].
#' @param execution_id *Unique ID generated at the beginning of each model construction process - all output is associated with this ID [character].
#' @param custom_param *Custom tuning parameters supplied to the prediction function and used in place of the default values (if tuning_method==""no_tuning"") [list - misc]. 
#' @param output_path *Directory within which all model output (e.g. tuning graphs) is stored [path].
#' @param ensemble_agg *Method used to aggregate the individual learners into an ensemble learner [ols  (linear regression on individual learners predictions in the case of a regression task vs. logit regression on individual learner predictions in the case of a classification task), nnls (weights derived through non-negative least squares)]. [character].
#' @param insample *Whether (with holdout==TRUE) the holdout  set predictions are based on the whole sample (TRUE) or only the holdoutset (FALSE) [logical - TRUE, FALSE].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @param max_core *The maximum number of cores to be used in all parallelised loops [integer].
#' @return *A list with out of sample predictions as well as hyperparameters and ensemble weights.
#' @examples


holdout_predict <- function(df, LHS, RHS, RHS_ext, 
                    custom_param=ExpLearning.Default$custom_param,
                    cluster_var=ExpLearning.Default$cluster_var,
                    prediction_var=ExpLearning.Default$prediction_var,
                    ensemble_agg=ExpLearning.Default$ensemble_agg,
                    weight_var=ExpLearning.Default$weight_var,
                    id_var=ExpLearning.Default$id_var,
                    tuning_method=ExpLearning.Default$tuning_method,
                    fold_method=ExpLearning.Default$fold_method,
                    rand_grid_iter=ExpLearning.Default$rand_grid_iter,
                    losstype=ExpLearning.Default$losstype, 
                    task=ExpLearning.Default$task,
                    max_core=ExpLearning.Default$max_core,
                    holdout_var=ExpLearning.Default$holdout_var,
                    execution_id=eval(parse(text=ExpLearning.Default$exceution_id)),
                    predictor=ExpLearning.Default$predictor,
                    innerfold=ExpLearning.Default$innerfold,
                    insample=ExpLearning.Default$insample,
                    quiet=ExpLearning.Default$quiet,
                    output_path=eval(parse(text=ExpLearning.Default$output_path)),
                    fold_var=ExpLearning.Default$fold_var,
                    round_digit=ExpLearning.Default$round_digit) {
  
  # Build ensemble model (training set)
  # ---------------------------------
  df_temp <- copy(df)
  ensemblereturn <- ensemble_predict(df=df_temp[get(holdout_var)==FALSE],LHS=LHS,RHS=RHS,
                                      cluster_var=cluster_var, predictor=predictor, 
                                      innerfold=innerfold, ensemble_agg=ensemble_agg,
                                      tuning_method = tuning_method, 
                                      rand_grid_iter = rand_grid_iter, 
                                      fold_var = fold_var, 
                                      task= task, 
                                      max_core=max_core, 
                                      custom_param=custom_param, 
                                      fold_method = fold_method, 
                                      losstype = losstype, 
                                      output_path = output_path, 
                                      weight_var = weight_var, 
                                      quiet=quiet, 
                                      execution_id=execution_id, 
                                      tune_only=TRUE)


  ensemblereturn$param <-  lapply(ensemblereturn$param, function(x) lapply(x, function(y) 
                              unlist(y)))
  ensemblereturn$param[which(sapply( ensemblereturn$param,function(x) 
    is.null(unlist(x)))==TRUE)] <- "/"

  # Get ensemble predictions ((i) re-estimate individual learners on entire training set
  # (ii) combine re-estimated predictions with ensemble weights derived above -> predictions
  # ---------------------------------
  df_temp <- copy(df)

  ## Ensemble predictions on holdout set only
  if(insample==FALSE) {

    oos_predict_value <- oos_predict(df=df_temp,LHS=LHS,RHS=RHS, cluster_var=cluster_var,
                      from=which(df_temp[,get(holdout_var)]==FALSE),to=which(df_temp[,get(holdout_var)]==TRUE),
                      tuning_method = tuning_method, 
                      weight_var = weight_var, 
                      fold_method=fold_method,
                      task= task, 
                      losstype=losstype, 
                      ensemble_agg=ensemble_agg,
                      custom_param=custom_param, 
                      max_core=max_core,
                      output_path=output_path, 
                      stack_model=ensemblereturn$stack_model, 
                      rand_grid_iter=rand_grid_iter, 
                      predictor=predictor,innerfold=innerfold,quiet=quiet,
                      execution_id=execution_id)

    df_temp[get(holdout_var)==TRUE,c(prediction_var):=oos_predict_value$ensemble_pred]
    df_temp[get(holdout_var)==TRUE,c(paste0(prediction_var, "_nnls")):= 
      oos_predict_value$ensemble_pred_nnls]

    outdf      <- data.table(data.frame(copy(df_temp)[get(holdout_var)==TRUE], 
                    oos_predict_value[setdiff(names(oos_predict_value), 
                    c("ensemble_pred", grep("param", names(oos_predict_value), value=T),
                    grep("model_predict", names(oos_predict_value), value=T), 
                    grep("learner_list", names(oos_predict_value), value=T)))]))
    outdf      <-  outdf[,mget(c(LHS,prediction_var,c(paste0(prediction_var, "_nnls")), RHS,RHS_ext, 
                    cluster_var, weight_var, grep("ind_pred", names(outdf), value=T)))]

    oos_predict_value$param <- oos_predict_value[grep("param", names(oos_predict_value), value=T)]
    names(oos_predict_value$param) <- predictor
    oos_predict_value$param <- lapply(oos_predict_value$param, function(x) x[[1]])

    model_predict_function <- oos_predict_value$model_predict
    learner_list           <- oos_predict_value[grep("learner_list", names(oos_predict_value), value=T)]

  ## Ensemble predictions on entire data set
  } else if (insample==TRUE) {

    oos_predict_value <- oos_predict(df=df_temp,LHS=LHS,RHS=RHS,cluster_var=cluster_var,
                          from=which(df_temp[,get(holdout_var)]==FALSE),to=c(1:nrow(df_temp)),
                          weight_var = weight_var,
                          tuning_method = tuning_method, 
                          fold_method=fold_method,
                          task= task, 
                          losstype=losstype, 
                          custom_param=custom_param, 
                          output_path=output_path, 
                          ensemble_agg=ensemble_agg,
                          max_core=max_core,
                          rand_grid_iter=rand_grid_iter, 
                          stack_model=ensemblereturn$stack_model, 
                          predictor=predictor,innerfold=innerfold,quiet=quiet, 
                          execution_id=execution_id)

    df_temp[, c(prediction_var):= oos_predict_value$ensemble_pred]
    df_temp[, c(paste0(prediction_var, "_nnls")):= oos_predict_value$ensemble_pred_nnls]
    
    outdf   <- data.table(data.frame(copy(df_temp), oos_predict_value[setdiff(names(oos_predict_value),   
                 c("ensemble_pred", grep("param", names(oos_predict_value), value=T), 
                  grep("model_predict", names(oos_predict_value), value=T),
                  grep("learner_list", names(oos_predict_value), value=T)))]))
    outdf  <-  outdf[,mget(c(LHS,prediction_var, c(paste0(prediction_var, "_nnls")), RHS, RHS_ext, 
                 cluster_var, weight_var, grep("ind_pred", names(outdf), value=T)))]

    oos_predict_value$param <- oos_predict_value[grep("param", names(oos_predict_value), value=T)]
    names(oos_predict_value$param) <- predictor
    oos_predict_value$param <- lapply(oos_predict_value$param, function(x) x[[1]])
    
    model_predict_function <- oos_predict_value$model_predict
    learner_list           <- oos_predict_value[grep("learner_list", names(oos_predict_value), value=T)]

  }

  ## Construct agg df
  # --------------------------
  agg_df <- copy(df)
  agg_df[, c(prediction_var):=model_predict_function(df=agg_df)]
  for (i in predictor) {
    agg_df[, c(paste0("ind_pred_", i)):=model_predict_function(df=agg_df,learner=i)]
  }
  agg_df <- agg_df[, mget(c(id_var, LHS, holdout_var, prediction_var, weight_var,
    grep("ind_pred", names(agg_df), value=T)))]

  # print the key IS results
  # --------------------------
  if (quiet==0.5) {

    # individual learner is losses
    ind_loss <- sapply(grep("ind_pred", names(agg_df), value=T), function(pred_var) {

      temp_loss <- loss(agg_df[get(holdout_var)==FALSE, get(pred_var)],agg_df[get(holdout_var)==FALSE,get(LHS)], 
                    task=task, losstype=losstype, weight=agg_df[get(holdout_var)==FALSE,get(weight_var)])

      return(temp_loss)

    })

    if (length(ind_loss)>0) {
      names(ind_loss) <- predictor
    }

    ind_loss_auc <- sapply(grep("ind_pred", names(agg_df), value=T), function(pred_var) {

      temp_loss <- loss(agg_df[get(holdout_var)==FALSE, get(pred_var)],agg_df[get(holdout_var)==FALSE,get(LHS)], 
                    task=task, losstype="auc", weight=agg_df[get(holdout_var)==FALSE,get(weight_var)])
      return(temp_loss)

    })

    if (length(ind_loss_auc)>0) {
      names(ind_loss_auc) <- predictor
    }

    cat("\n## IS LOSS - Ind Learner \n")
      
    cat(paste0("\n# LOSS: ", losstype,"\n\n"))   
    inv_lapply(1:length(predictor), function(x) cat(paste0(names(ind_loss)[x], ":", 
      round(ind_loss[x],5), "\n")))
    cat(paste0("\n# LOSS: ","AUC","\n\n"))  
    inv_lapply(1:length(predictor), function(x) cat(paste0(names(ind_loss_auc)[x], ":", 
      round(ind_loss_auc[x],5), "\n")))

    cat("\n## IS LOSS - Ensemble \n")
    
    cat(paste0("\n# LOSS: ", losstype,"\n\n"))   
    cat(paste0("ensemble:", round(loss(agg_df[get(holdout_var)==FALSE,prediction], 
      agg_df[get(holdout_var)==FALSE,get(LHS)], task=task,
      losstype=losstype, weight=agg_df[get(holdout_var)==FALSE,get(weight_var)]),4)))
    cat("\n")
    cat(paste0("\n# LOSS: ","AUC","\n\n"))  
    cat(paste0("ensemble:", round(loss(agg_df[get(holdout_var)==FALSE,prediction], 
      agg_df[get(holdout_var)==FALSE,get(LHS)], task=task,
      losstype="auc", weight=agg_df[get(holdout_var)==FALSE,get(weight_var)]),4)))
    cat("\n\n")

  }

  # Return predictions and actual labels - holdoutdf
  # ---------------------------------
  drawreturn <- list(outdf=outdf, ensemble_weight=round(ensemblereturn$ensemble_weight,round_digit), 
                    ensemble_weight_nnls=round(ensemblereturn$ensemble_weight_nnls,round_digit), 
                    inner_param=ensemblereturn$param, outer_param=oos_predict_value$param, 
                    model_predict=model_predict_function, learner_list=learner_list, 
                    agg_df=agg_df)
              
  return(drawreturn)

}

#----------------------------------------------------------------------------#



