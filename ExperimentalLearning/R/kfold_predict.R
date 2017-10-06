#----------------------------------------------------------------------------#

#' @title *Construct an ensemble model using the entire data set and generate CV-based (out of sample predictions) based on this model.
#'
#' @description *.
#' 
#' @export
#' @param df *Datatable containing the data [data.table].
#' @param LHS *Name of the independent variable [character].
#' @param RHS
#' @param RHS_ext 
#' @param cluster_var *Name of variable containing observation level cluster identifiers (integer) [character].
#' @param holdout *Whether or not to construct and use a holdout set [logical - TRUE, FALSE].
#' @param prediction *Vector of predictions [numeric].
#' @param predictor *List of algorithms included in ensemble learner [ols (linear regression), rf (random forest (classification OR regression)), avg (simple mean), elnet (elastic net), logit (logistic regression), tree (tree (classification or regression)), xgb (gradient boosted tree (classification or regression))] [character].
#' @param innerfold *Number of CV folds used in inner CV routine [integer].
#' @param weight *Vector of observation weights [integer or numeric].
#' @param fold_method *Method used to generate CV folds (and the train-holdout split) [cluster_count (target cluster count), obs_count (target observation count), stratification (target stratification)].
#' @param task *The nature of the prediction task  [regression (regression), classification (classification)].
#' @param fold_var *Name of variable containing observation level fold identifiers (integer) [character].
#' @param rand_grid_iter *Number of random grid search iterations (performed if tuning_method==""rand_grid"") [integer]. 
#' @param losstype *Loss function used to tune the individiual learners and the ensemble neg_mse_brier, loglik, auc, accuracy, balanced_accuracy, r2].
#' @param tuning_method *Tuning method [character].
#' @param id_var
#' @param execution_id *Unique ID generated at the beginning of each model construction process - all output is associated with this ID [character].
#' @param custom_param *Custom tuning parameters supplied to the prediction function and used in place of the default values (if tuning_method==""no_tuning"") [list - misc]. 
#' @param output_path *Directory within which all model output (e.g. tuning graphs) is stored [path].
#' @param ensemble_agg *Method used to aggregate the individual learners into an ensemble learner [ols  (linear regression on individual learners predictions in the case of a regression task vs. logit regression on individual learner predictions in the case of a classification task), nnls (weights derived through non-negative least squares)]. [character].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @param permutation *Number of permutations performed to generate the p-values [integer].
#' @param control_var *Vector of control variable names (balanced across treatment conditions ) (RHS) [character].
#' @param control_var_unbalanced *Vector of control variable names ((potentially) unabalanced across treatment conditions, i.e. random assignemnt is assumed to hold conditional on these variables) (RHS) [character].
#' @param perm_strategy *Whether to permute the assignment variable by (a) exchanging variable values at the cluster level (entire_cluster) or (b) exchanging lvariable values within clusters (within_cluster) [character].
#' @param perm_test *(Permutation) test(s) to perform [character].
#' @param max_core *The maximum number of cores to be used in all parallelised loops [integer].
#' @param permute *Whther to construct the ensemble using the original data or a permuted version [logical].
#' @return *A list with CV-based (out of sample predictions) as well as hyperparameters and ensemble weights.
#' @examples

kfold_predict <- function(df, LHS, RHS, RHS_ext, permute, perm_test, control_var, control_var_unbalanced,
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
                    quiet=ExpLearning.Default$quiet,
                    perm_strategy=ExpLearning.Default$perm_strategy,
                    output_path=eval(parse(text=ExpLearning.Default$output_path)),
                    fold_var=ExpLearning.Default$fold_var,
                    permutation=ExpLearning.Default$permutation, 
                    round_digit=ExpLearning.Default$round_digit) {


  # Permute if permutation run 
  # ---------------------------------
  if(permute==TRUE) {
    
    df_temp <- perm(df, LHS, cluster_var=cluster_var, 
                    fold_var=fold_var, within_fold=TRUE,
                    perm_strategy=perm_strategy, 
                    perm_test=perm_test,
                    control_var=control_var, 
                    control_var_unbalanced=control_var_unbalanced)
  } else {
    df_temp <- copy(df)
  }
  
  # Ensemble predictions
  # ---------------------------------
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
                                      execution_id=execution_id)
  

  # store the parameters
  ensemblereturn$param <-  lapply(ensemblereturn$param, function(x) lapply(x, function(y) 
                              unlist(y)))
  ensemblereturn$param[which(sapply( ensemblereturn$param,function(x) 
    is.null(unlist(x)))==TRUE)] <- "/"
 
  # obtain predictions
  df_temp[, c(prediction_var):=ensemblereturn$prediction]
  df_temp[, c(paste0(prediction_var, "_nnls")):=ensemblereturn$prediction_nnls]

  df_temp <- data.table(data.frame(df_temp, ensemblereturn[grep("ind_pred", 
                names(ensemblereturn), value=T)])) 

  ## Construct agg df
  # --------------------------
  agg_df <- copy(df_temp)
  agg_df <- agg_df[, mget(c(id_var, LHS, holdout_var, prediction_var, weight_var,
    grep("ind_pred", names(agg_df), value=T)))]

  
  # Return predictions and actual labels - holdoutdf
  # ---------------------------------
  drawreturn <- list(outdf=df_temp, ensemble_weight=round(ensemblereturn$ensemble_weight,round_digit), 
                    ensemble_weight_nnls=round(ensemblereturn$ensemble_weight_nnls,round_digit), 
                    inner_param=ensemblereturn$param, agg_df=agg_df)
              
  return(drawreturn)

}

#----------------------------------------------------------------------------#
