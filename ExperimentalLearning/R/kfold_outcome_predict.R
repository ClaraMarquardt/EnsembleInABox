#----------------------------------------------------------------------------#

#' @title *Construct an ensemble model using the entire data and generate CV-based (out of sample) predictions based on this model [Interface].
#'
#' @description *
#'
#' @export
#' @import data.table
#' @param df *Datatable containing the data [data.table].
#' @param dict *List with the names of all key variables initialised at the beginning of each model construction process [list - character].
#' @param setting *List with key settings generated at the beginning of each model construction process [list - misc].
#' @param permute *Whther to construct the ensemble using the original data or a permuted version [logical].
#' @param perm_test *(Permutation) test(s) to perform [character].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @return *A list with out of sample predictions as well as hyperparameters and ensemble weights.
#' @examples

kfold_outcome_predict <- function(df, dict, setting, permute, perm_test=NULL, 
  quiet=ExpLearning.Default$quiet) {

  ## status
  cat(paste0("Starting ",ifelse(permute,"permuted","main")," ",setting$nfold,"-fold loop"),"\n")
  
  # select appropriate construction method (classic/opt)
  if (setting$construct_mode=="classic") {
    kfold_predict_temp <- kfold_predict
  } else if (setting$construct_mode=="opt") {
    kfold_predict_temp <- kfold_opt_predict
  }

  drawreturn <- kfold_predict_temp(df=df,
                               LHS=dict$assignment_var,
                               RHS=c(dict$group_var,  dict$control_var, 
                                dict$control_var_unbalanced), 
                               RHS_ext=c(dict$id_var),
                               id_var=dict$id_var,
                               control_var=dict$control_var, 
                               control_var_unbalanced=dict$control_var_unbalanced, 
  							               cluster_var=dict$cluster_var,
                               prediction_var=dict$prediction_var, 
                               holdout_var=dict$holdout_var, 
                               fold_method = setting$fold_method, 
                               rand_grid_iter=setting$rand_grid_iter, 
                               tuning_method=setting$tuning_method, 
                               losstype=setting$losstype, 
                               task=setting$task, 
                               perm_strategy=setting$perm_strategy,
                               fold_var=dict$fold_var, 
                               weight_var=dict$weight_var, 
                               predictor=setting$predictor,
                               ensemble_agg=setting$ensemble_agg,
                               execution_id=setting$execution_id, 
                               output_path=setting$output_path,
                               innerfold=setting$nfold, 
                               max_core=setting$max_core, 
                               custom_param=setting$custom_param, 
                               permutation=setting$permutation, 
                               quiet=quiet, 
                               permute=permute,
                               perm_test=perm_test) 
  
 
  return(drawreturn)

}

#----------------------------------------------------------------------------#
