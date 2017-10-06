#----------------------------------------------------------------------------#

#' @title *Format the model output.
#'
#' @description *.
#' 
#' @export 
#' @param model_result_raw *List of outputs returned from the holdout_(opt)_outcome_predict or kfold_(opt)_outcome_predict functions [list - misc].
#' @param setting *List with key settings generated at the beginning of each model construction process [list - misc].
#' @param dict *List with the names of all key variables initialised at the beginning of each model construction process [list - character].
#' @return *A list with the formatted model output.
#' @examples

format_output <- function(model_result_raw, setting, dict) {

      return_final <- list()
      return_final$execution_id             <- setting$execution_id

      return_final$ensemble_loss            <- model_result_raw$model_loss
      return_final$ensemble_loss_auc        <- model_result_raw$main_loss_auc

      return_final$ensemble_pval_baseline_signal             <- model_result_raw$p_baseline_signal
      return_final$ensemble_pval_95_ci_baseline_signal       <- model_result_raw$p_95_ci_baseline_signal
      return_final$ensemble_pval_90_ci_baseline_signal       <- model_result_raw$p_90_ci_baseline_signal
      return_final$ensemble_pval_baseline_balance            <- model_result_raw$p_baseline_balance
      return_final$ensemble_pval_95_ci_baseline_balance      <- model_result_raw$p_95_ci_baseline_balance
      return_final$ensemble_pval_90_ci_baseline_balance      <- model_result_raw$p_90_ci_baseline_balance
      return_final$ensemble_pval_signal_a                    <- model_result_raw$p_signal_a
      return_final$ensemble_pval_95_ci_signal_a              <- model_result_raw$p_95_ci_signal_a
      return_final$ensemble_pval_90_ci_signal_a              <- model_result_raw$p_90_ci_signal_a
      return_final$ensemble_pval_signal_b                    <- model_result_raw$p_signal_b
      return_final$ensemble_pval_95_ci_signal_b              <- model_result_raw$p_95_ci_signal_b
      return_final$ensemble_pval_90_ci_signal_b              <- model_result_raw$p_90_ci_signal_b

      if (length(model_result_raw[grep("p_ind.*$", names(model_result_raw), value=T)])>0) { 

        return_final[paste0(setting$predictor, "_pval_signal_a")] <- 
          model_result_raw[grep("p_ind.*$", names(model_result_raw), value=T)]
        return_final[paste0(setting$predictor, "_pval_95_ci_signal_a")] <- 
          model_result_raw[grep("p_95_ci_ind.*$", names(model_result_raw), value=T)]
        return_final[paste0(setting$predictor, "_pval_90_ci_signal_a")] <- 
          model_result_raw[grep("p_90_ci_ind.*$", names(model_result_raw), value=T)]

      } 

      if (length(model_result_raw[grep("ensemble_wald", names(model_result_raw), value=T)])>0) { 
        return_final$ensemble_wald       <- 
          model_result_raw[grep("ensemble_wald", names(model_result_raw), value=T)]
      } 


      return_final$ensemble_loss_nnls       <- model_result_raw$main_loss_nnls
      return_final$ensemble_loss_auc_nnls   <- model_result_raw$main_loss_auc_nnls
      return_final[paste0(setting$predictor, "_loss")] <- 
        model_result_raw[grep("^mainloss_[^_]*$", names(model_result_raw), value=T)]
      return_final[paste0(setting$predictor, "_loss_auc")] <- 
        model_result_raw[grep("^mainloss_auc_.*$", names(model_result_raw), value=T)]
      
      return_final$ensemble_perm_loss_baseline_signal        <- model_result_raw$perm_loss_baseline_signal
      return_final$ensemble_mean_perm_loss_baseline_signal   <- model_result_raw$perm_loss_mean_baseline_signal
      return_final$ensemble_perm_loss_baseline_balance       <- model_result_raw$perm_loss_baseline_balance
      return_final$ensemble_mean_perm_loss_baseline_balance  <- model_result_raw$perm_loss_mean_baseline_balance
      return_final$ensemble_perm_loss_signal_a               <- model_result_raw$perm_loss_signal_a
      return_final$ensemble_mean_perm_loss_signal_a          <- model_result_raw$perm_loss_mean_signal_a
      return_final$ensemble_perm_loss_signal_b               <- model_result_raw$perm_loss_signal_b
      return_final$ensemble_mean_perm_loss_signal_b          <- model_result_raw$perm_loss_mean_signal_b
 
      return_final$ensemble_perm_class_balance_baseline_signal        <- model_result_raw$perm_df_class_balance_baseline_signal
      return_final$ensemble_perm_class_balance_baseline_balance       <- model_result_raw$perm_df_class_balance_baseline_balance
      return_final$ensemble_perm_class_balance_signal_a               <- model_result_raw$perm_df_class_balance_signal_a
      return_final$ensemble_perm_class_balance_signal_b               <- model_result_raw$perm_df_class_balance_signal_b

      return_final$ensemble_weight          <- model_result_raw$ensemble_weight
      return_final$ensemble_weight_nnls     <- model_result_raw$ensemble_weight_nnls 
      
      if (length(model_result_raw[grep("outer_param", names(model_result_raw), value=T)])>0) {
        for (i in 1:length(setting$predictor)) {
          return_final[paste0(setting$predictor[i],"_outer_param")] <- 
            model_result_raw[grep("outer_param", names(model_result_raw), value=T)][[1]][i]
        }
      } else {
        return_final[paste0(setting$predictor,"_outer_param")] <- "/"
      }

      if (setting$construct_mode =="classic" & setting$holdout==TRUE) { 

        for (i in 1:length(setting$predictor)) {
          return_final[paste0(setting$predictor[i],"_inner_param")] <- 
            model_result_raw[grep("inner_param", names(model_result_raw), 
              value=T)]$inner_param[setting$predictor[[i]]]
          }

      } else if(setting$construct_mode =="opt" & setting$holdout==FALSE) {
        for (i in 1:length(setting$predictor)) {
          return_final[paste0(setting$predictor[i],"_inner_param")] <- 
            model_result_raw[grep("inner_param", names(model_result_raw), 
              value=T)]$inner_param[setting$predictor[[i]]]
        }
     } else {
        return_final[paste0(setting$predictor,"_inner_param")] <- "/"
      }

      return_final$obs_count_total          <- model_result_raw$obs_count_total
      return_final$obs_count_train          <- model_result_raw$obs_count_train
      return_final$obs_count_holdout        <- model_result_raw$obs_count_holdout

      return_final$pred_count_total                 <- model_result_raw$pred_count_total
      return_final$pred_count_group                 <- model_result_raw$pred_count_group
      return_final$pred_count_balanced_covariate    <- model_result_raw$pred_count_balanced_covariate
      return_final$pred_count_unbalanced_covariate  <- model_result_raw$pred_count_unbalanced_covariate

      return_final$df                       <- model_result_raw$df
      return_final$agg_df                   <- model_result_raw$agg_df
      return_final$setting                  <- setting
      return_final$dict                     <- dict
      return_final$model_predict_function   <- model_result_raw$model_predict_function
      return_final$learner_list             <- model_result_raw$learner_list

      return(return_final)

}

#----------------------------------------------------------------------------#
