#----------------------------------------------------------------------------#

#' @title *Construct an ensemble model using a holdout set and generate out of sample predictions based on this model [Interface].
#'
#' @description *.
#' 
#' @export
#' @param df *Datatable containing the data [data.table].
#' @param dict *List with the names of all key variables initialised at the beginning of each model construction process [list - character].
#' @param setting *List with key settings generated at the beginning of each model construction process [list - misc].
#' @return *A list with out of sample predictions as well as hyperparameters and ensemble weights.
#' @examples

holdout_outcome_predict <- function(df, dict, setting) {
  
  cat(paste0("\n### Starting ",setting$nfold, "-fold loop"),"\n")
  
  # select appropriate construction method (classic/opt)
  if (setting$construct_mode=="classic") {
    holdout_predict_temp <- holdout_predict
  } else if (setting$construct_mode=="opt") {
    holdout_predict_temp <- holdout_opt_predict
  }

  drawreturn <- holdout_predict_temp(df=df,
                               LHS=dict$assignment_var,
                               RHS=c(dict$group_var,  dict$control_var, 
                                dict$control_var_unbalanced), 
                               RHS_ext=c(dict$id_var),
                               id_var=dict$id_var,
  							               cluster_var=dict$cluster_var,
                               prediction_var=dict$prediction_var, 
                               holdout_var=dict$holdout_var, 
                               fold_method = setting$fold_method, 
                               rand_grid_iter=setting$rand_grid_iter, 
                               tuning_method=setting$tuning_method, 
                               losstype=setting$losstype, 
                               task=setting$task, 
                               fold_var=dict$fold_var, 
                               weight_var=dict$weight_var,
                               max_core=setting$max_core,
                               predictor=setting$predictor,
                               ensemble_agg=setting$ensemble_agg,
                               execution_id=setting$execution_id, 
                               output_path=setting$output_path,
                               innerfold=setting$nfold, 
                               custom_param=setting$custom_param, 
                               insample=setting$insample, 
                               quiet=setting$quiet) 
  

  return(drawreturn)
}

#----------------------------------------------------------------------------#

