
#----------------------------------------------------------------------------#

#' @title *Construct an ensemble model to test the predictability of an outcome variable based on one or more predictors [Interface].
#'
#' @description *.
#' 
#' @export
## vars & data
#' @param df *Datatable containing the data [data.table].
#' @param assignment_var *Name of outcome variable [character].
#' @param group_var *Name of predictor variable [character].
#' @param control_var *Vector of control variable names (balanced across treatment conditions ) (RHS) [character].
#' @param control_var_unbalanced *Vector of control variable names ((potentially) unabalanced across treatment conditions, i.e. random assignemnt is assumed to hold conditional on these variables) (RHS) [character].
#' @param cluster_var *Name of variable containing observation level cluster identifiers (integer) [character].
#' @param id_var *Name of variable containing unique observation level identifiers (integer) [character].
## weights
#' @param weight *Vector of observation weights [integer or numeric].
#' @param clusterweight_norm *Whether to normalise weights at the cluster level [logical - TRUE, FALSE].
## construction
#' @param missing_mode *How to handle missingness - (a) omit all missing observations (missing_omit), (b) contruct missingness indicators (and median impute the missing values) treating the missingness indicators as balanced covariates (missing_indic_balanced) or (c) contruct missingness indicators (and median impute the missing values) treating the missingness indicators as unbalanced covariates (missing_indic_unbalanced) [character].
#' @param construct_mode *Tuning structure used to construct the ensemble - (a) computationally intensive method yielding valid performance metrics (classic) or (b) computationally less intensive method yielding biased performance metrics (opt) [character].
#' @param holdout *Whether or not to construct and use a holdout set [logical - TRUE, FALSE].
#' @param nfold *Number of CV folds (used in both the inner and outer CV routine) [integer].
#' @param fold_method *Method used to generate CV folds (and the train-holdout split) [cluster_count (target cluster count), obs_count (target observation count), stratification (target stratification)].
#' @param ensemble_agg *Method used to aggregate the individual learners into an ensemble learner [ols  (linear regression on individual learners predictions in the case of a regression task vs. logit regression on individual learner predictions in the case of a classification task), nnls (weights derived through non-negative least squares)]. [character].
#' @param tuning_method *Tuning method [character].
#' @param rand_grid_iter *Number of random grid search iterations (performed if tuning_method==""rand_grid"") [integer]. 
#' @param custom_param *Custom tuning parameters supplied to the prediction function and used in place of the default values (if tuning_method==""no_tuning"") [list - misc]. 
## predictors
#' @param task *The nature of the prediction task  [regression (regression), classification (classification)].
#' @param predictor *List of algorithms included in ensemble learner [ols (linear regression), rf (random forest (classification OR regression)), avg (simple mean), elnet (elastic net), logit (logistic regression), tree (tree (classification or regression)), xgb (gradient boosted tree (classification or regression))] [character].
## permutation/tests
#' @param perm_test *(Permutation) test(s) to perform [character].
#' @param permutation *Number of permutations performed to generate the p-values [integer].
#' @param perm_strategy *Whether to permute the assignment variable by (a) exchanging variable values at the cluster level (entire_cluster) or (b) exchanging lvariable values within clusters (within_cluster) [character].
## loss
#' @param losstype *Loss function used to tune the individiual learners and the ensemble neg_mse_brier, loglik, auc, accuracy, balanced_accuracy, r2].
#' @param class_cutoff *Probability cutoff used to convert predicted probabilities into predicted classes in order to calcuate the accuracy of the predictions (if losstype==accuracy or balanced_accuracy) [numeric - 0 to 1].
## other tests
#' @param wald_test *Whether to conduct and report a series of Wald tests based on a linear model of the LHS as a function of all RHS variables (note: only applicable in the case of the holdout, optimised holdout, optimized CV approach) [logical]. 
#' @param ind_learner_p *Whether to generate and return p-values for each of the individual learners in the ensemble (only relevant if holdout==TRUE) [logical].
## misc
#' @param insample *Whether (with holdout==TRUE) the holdout  set predictions are based on the whole sample (TRUE) or only the holdoutset (FALSE) [logical - TRUE, FALSE].
#' @param round_digit *Number of digits to which results are rounded [integer].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @param save_only *Whether to build an ensemble model or to merely prepare and save the data for a later analysis [logical - TRUE, FALSE].
#' @param max_core *The maximum number of cores to be used in all parallelised loops [integer].
#' @param output_path *Directory within which all model output (e.g. tuning graphs) is stored [path].
#' @param execution_id *Unique ID generated at the beginning of each model construction process - all output is associated with this ID [character].
#' @return *List with the (final - CV or holdout) loss [main_loss], the mean (final - CV or holdout) permuted loss [perm_loss], the individual permutation losses [perm_loss_raw], the p-value [pval], the runtime in seconds [run_time], the selected tuning parameters for each of the learners [inner_param and outer_param], the out of sample auc of the final model [main_loss_auc] as well as the out of sample losses for each of the individual learners [indiv_loss]. 
#' @examples


effect_on_group <- function(df, assignment_var, group_var, 
                            control_var = NULL, 
                            control_var_unbalanced = NULL,
                            weight_var = NULL, 
                            cluster_var = NULL, id_var = NULL,
                            perm_test = ExpLearning.Default$perm_test,
                            construct_mode = ExpLearning.Default$construct_mode,
                            predictor = ExpLearning.Default$predictor,
                            holdout = ExpLearning.Default$holdout, 
                            nfold = ExpLearning.Default$nfold,
                            missing_mode = ExpLearning.Default$missing_mode,
                            holdout_prop = ExpLearning.Default$holdout_prop,
                            insample = ExpLearning.Default$insample,
                            rand_grid_iter = ExpLearning.Default$rand_grid_iter,
                            fold_method = ExpLearning.Default$fold_method,
                            permutation = ExpLearning.Default$permutation,
                            tuning_method = ExpLearning.Default$tuning_method, 
                            losstype = ExpLearning.Default$losstype,  
                            ensemble_agg = ExpLearning.Default$ensemble_agg,
                            perm_strategy = ExpLearning.Default$perm_strategy,
                            clusterweight_norm = ExpLearning.Default$clusterweight_norm, 
                            task = ExpLearning.Default$task, 
                            ind_learner_p = ExpLearning.Default$ind_learner_p, 
                            custom_param = ExpLearning.Default$custom_param,
                            class_cutoff = ExpLearning.Default$class_cutoff, 
                            round_digit = ExpLearning.Default$round_digit,
                            quiet = ExpLearning.Default$quiet,
                            wald_test = ExpLearning.Default$wald_test,
                            max_core = ExpLearning.Default$max_core,
                            save_only = FALSE, output_path = eval(parse(text=ExpLearning.Default$output_path)), 
                            execution_id = eval(parse(text=ExpLearning.Default$execution_id))) {


  # Start Code
  #----------------------------
  start_time <- Sys.time()
  cat(sprintf("\n### Launching experimental pipeline - execution id: %s  (start time: %s)\n", execution_id,
    as.character(start_time)))

  # Settings
  #----------------------------
  options(scipen=9999999)
  options(digits.secs=3)

  # Extend execution_id 
  # ---------------------------
  execution_id_ext <- gsub("-", "_", UUIDgenerate(use.time = NA))
  execution_id     <- paste0(execution_id,"_", execution_id_ext)

  # Ensure that the learners are aligned with the task
  #----------------------------
  # regression:     intercept, avg, ols,   *  , elnet, rf, svm, xgb
  # classification: intercept, avg, ols, logit, elnet, rf, svm, xgb

  if (task=="regression") {

    if ("logit" %in% c(unlist(predictor))) {

      if (quiet==0.5) cat("Regression - Logistic learner (logit) replaced with OLS learner (ols)")

      predictor <- as.list(c(setdiff(c(unlist(predictor)), c("logit")), "ols"))
    
    }

  }

  # Copy df
  #----------------------------
  df_temp <- copy(df)

  # Prepare dataframe
  #----------------------------
  cat("\n### Preparing the data\n")

  # prepare the data
  cleandata <- prepare_data(df=df_temp,assignment_var=assignment_var, group_var=group_var, 
                control_var=control_var,id_var=id_var, weight_var=weight_var, task=task, 
                cluster_var=cluster_var, control_var_unbalanced=control_var_unbalanced,
                clusterweight_norm=clusterweight_norm, quiet=quiet)

  df_temp          <- cleandata$df       # Cleaned data frame
  df_temp_omit_var <- cleandata$df_other # Data frame with other variables - merged back on at the end
  dict             <- cleandata$dict     # Dictionary with variable names for 'df'

  # store the setting
  setting  <- list(predictor = predictor, holdout = holdout, nfold = nfold,
                   holdout_prop = holdout_prop, losstype = losstype, 
                   permutation = permutation, clusterweight_norm=clusterweight_norm, 
                   fold_method =fold_method, 
                   task = task, class_cutoff = class_cutoff, round_digit = round_digit, 
                   insample = insample, tuning_method = tuning_method, 
                   rand_grid_iter = rand_grid_iter,
                   ensemble_agg = ensemble_agg, quiet = quiet, 
                   max_core = max_core, perm_strategy = perm_strategy,
                   output_path = output_path, custom_param = custom_param, 
                   execution_id = execution_id, wald_test = wald_test, 
                   missing_mode = missing_mode, perm_test = perm_test,
                   construct_mode = construct_mode, 
                   ind_learner_p = ind_learner_p, 
                   save_only = save_only)


  # Add folds/holdout set
  #----------------------------
  cat("\n### Generating folds/holdout set\n")

  df_temp <- generate_fold_holdout(df=df_temp, setting=setting, dict=dict)
    

  # Missingness (df & dict update)
  #----------------------------

  # store the original number of observations
  obs_pre_missing <- nrow(copy(df_temp))

  df_complete <- complete_data(df=df_temp, missing_mode=setting$missing_mode, 
    assignment_var=dict$assignment_var, missing_var=c(dict$group_var, dict$control_var, 
    dict$control_var_unbalanced), non_missing_var=c( 
    dict$cluster_var, dict$weight_var,dict$id_var), 
    holdout_var=dict$holdout_var, quiet=setting$quiet)

  df_temp            <- df_complete$df
  dict$miss_var      <- df_complete$miss_var

  if (setting$missing_mode=="missing_indic_balanced") {
    if (length(df_complete$miss_var)>0) {
      dict$control_var              <- c(dict$control_var ,
        paste0(df_complete$miss_var, "_MISS"))
    }
    
  } else if(setting$missing_mode=="missing_indic_unbalanced") {
    if (length(df_complete$miss_var)>0) {
      dict$control_var_unbalanced   <-  c(dict$control_var_unbalanced ,
        paste0(df_complete$miss_var, "_MISS"))
    }
  }

  # IF omit observations, i.e. missing_mode =="missing_omit" -> reconstruct folds and holdout set (forcibly)
  if (obs_pre_missing > nrow(df_temp)) {

    cat("\n### Re-Generating folds/holdout set (Due to the omission of missing observations)\n")

    df_temp <- generate_fold_holdout(df=df_temp, setting=setting, 
      dict=dict, regenerate=TRUE)

  }

  # obtain the number of observations
  obs_count_total   <- nrow(df_temp)
  obs_count_train   <- nrow(df_temp[holdout==FALSE])
  obs_count_holdout <- nrow(df_temp[holdout==TRUE])

  # obtain the total number of predictors
  pred_count_total                 <- length(c(group_var, control_var, control_var_unbalanced))
  pred_count_group                 <- length(group_var)
  pred_count_balanced_covariate    <- length(control_var) 
  pred_count_unbalanced_covariate  <- length(control_var_unbalanced)

  if (setting$quiet==0.5) {

    cat("\n\n## Prediction Task Overview\n\n")
    cat(sprintf("* obs_count_total: %d / obs_count_train: %d / obs_count_holdout: %d\n", 
      obs_count_total,obs_count_train,obs_count_holdout))

    cat(sprintf("* pred_count_total: %d / pred_count_group: %d / pred_count_balanced_covariate: %d / pred_count_unbalanced_covariate: %d\n\n", 
      pred_count_total,pred_count_group,pred_count_balanced_covariate, pred_count_unbalanced_covariate))
  }

  # Look at what you have done (before really having done anything)
  #----------------------------
  if (setting$quiet==0.5) {

    cat("\n### Data overview\n")

    print(str(df_temp))

  }

  # If only initialization, save the prepared files for analysis; otherwise, start analysis
  #----------------------------
  if(setting$save_only==TRUE) {

    cat("\n### Saving the data (for later analysis)\n")

    basedata <- list(df=df_temp,dict=dict,setting=setting, execution_id=setting$execution_id)

    if (!(dir.exists(paste0(setting$output_path, "batch")))) 
      dir.create(paste0(setting$output_path, "batch"))
    if (!(dir.exists(paste0(setting$output_path, "batch/", setting$execution_id)))) 
      dir.create(paste0(setting$output_path, "batch/", setting$execution_id))

    saveRDS(basedata, file=paste0(setting$output_path, "batch/",setting$execution_id,"/basedata.Rds"))
    saveRDS(setting$execution_id, file=paste0(setting$output_path, "batch/current.Rds"))

    return(list(runtime=as.numeric(round(Sys.time()-start_time, 2))))

   } else {

  # Wald ensemble
  #----------------------------
  #----------------------------

  if (!(setting$wald_test==FALSE)) {
        
        wald_return <- wald_ensemble(df=df_temp, 
          assignment_var=dict$assignment_var,
          group_var=dict$group_var,
          control_var=dict$control_var,
          control_var_unbalanced=dict$control_var_unbalanced,
          wald_test=setting$wald_test, 
          holdout_var=dict$holdout_var, 
          permutation=setting$permutation,
          perm_strategy=setting$perm_strategy,
          max_core=setting$max_core,
          cluster_var=dict$cluster_var,
          quiet=setting$quiet) 

   } else {

      wald_return <- list()
        
  }

  # Ensemble
  #----------------------------
  #----------------------------

  if(setting$holdout==TRUE) {

      # A: Hold-out Routine / Classical & Opt
      #----------------------------
      #----------------------------

      # Assert that using implemented permutation test
      #----------------------------
      assert("Holdout mode (classic or optimised) - Using implemented permutation tests (effect_on_group())", 
        sum(!(setting$perm_test %in% c("baseline_balance", "signal_a")))==0)

      # Obtain predictions
      #----------------------------
      holdout_return <- holdout_outcome_predict(df=df_temp,dict=dict,setting=setting)
      
      # Analyze them
      #----------------------------
      return_raw <- list.append(
        holdout_analysis(model_result_raw=copy(holdout_return),dict=dict,setting=setting), 
        ensemble_weight=paste0(c(rbind(c(unlist(setting$predictor)),  
          " ", round(holdout_return$ensemble_weight, 4), " / ")), collapse=""),
        ensemble_weight_nnls=paste0(c(rbind(c(unlist(setting$predictor)),  
          " ", round(holdout_return$ensemble_weight_nnls,4), " / ")), collapse=""), 
        outer_param=holdout_return$outer_param,
        inner_param=holdout_return$inner_param, 
        model_predict_function=holdout_return$model_predict,
        learner_list=holdout_return$learner_list, 
        agg_df=holdout_return$agg_df,
        obs_count_total=obs_count_total, 
        obs_count_train=obs_count_train, 
        obs_count_holdout=obs_count_holdout,
        pred_count_total=pred_count_total, 
        pred_count_group=pred_count_group, 
        pred_count_balanced_covariate=pred_count_balanced_covariate, 
        pred_count_unbalanced_covariate=pred_count_unbalanced_covariate)

      return_raw <- c(return_raw, wald_return)

    } else if (setting$holdout==FALSE) {

      # B: CV-only Routine / Classical & Opt
      #----------------------------
      #----------------------------

      # Assert that using implemented permutation test
      #----------------------------
      assert("CV mode (classic or optimised) - Using implemented permutation tests (effect_on_group())", 
        sum(!(setting$perm_test %in% c("baseline_balance", "baseline_signal", "signal_a", "signal_b")))==0)


      # Obtain predictions
      #----------------------------
      
      cat("\n","Pre-permutation","\n")

      kfold_return <- kfold_outcome_predict(df=df_temp,dict=dict,setting=setting, 
        permute=FALSE)
      
      perm_return <- lapply(setting$perm_test, function(x) {

        cat("\n",paste0("Permutation: ", x, "\n\n"))

        perm_return_temp <- mclapply_robust(X=1:(setting$permutation), FUN=function(y) {
          
          cat("\n",paste0("Permutation ",y," of ",(setting$permutation), " [",x, "]"), "\n\n")
          
          perm_return_df <- kfold_outcome_predict(df=df_temp,dict=dict,setting=setting, 
            permute=TRUE,quiet=ifelse(setting$quiet==0.5, 0.5, 2), perm_test=x)$outdf
          
          return(perm_return_df)

        }, max_core=setting$max_core)

        return(perm_return_temp)
      
      })

      if (setting$quiet==0.5) {
        print(kfold_return)
        print(perm_return[[1]][1:2])

      }
   
      # Analyze them
      #----------------------------
      return_raw <- list.append(kfold_analysis(model_result_raw=copy(kfold_return), 
        perm_return=perm_return,
        dict=dict,setting=setting), 
        ensemble_weight=paste0(c(rbind(c(unlist(setting$predictor)),  
        " ", kfold_return$ensemble_weight, " / ")), collapse=""),
        ensemble_weight_nnls=paste0(c(rbind(c(unlist(setting$predictor)),  
        " ", kfold_return$ensemble_weight_nnls, " / ")), collapse=""), 
        inner_param=kfold_return$inner_param, 
        agg_df=kfold_return$agg_df, 
        obs_count_total=obs_count_total, 
        obs_count_train=obs_count_train, 
        obs_count_holdout=obs_count_holdout,
        pred_count_total=pred_count_total, 
        pred_count_group=pred_count_group, 
        pred_count_balanced_covariate=pred_count_balanced_covariate, 
        pred_count_unbalanced_covariate=pred_count_unbalanced_covariate)

      return_raw <- c(return_raw, wald_return)

    } 


  # Formatting
  #----------------------------
  #----------------------------
  
  ## format 
  return_final   <- format_output(model_result_raw=return_raw, setting=setting, dict=dict)

  ## merge other var back on
  return_final$df <- df_temp_omit_var[return_final$df, on=c(dict$id_var), 
                        nomatch=NA]
  setcolorder(return_final$df, c(setdiff(names(return_final$df), 
    dict$add_var),dict$add_var))
  
  ## status
  run_time              <- as.numeric(round(Sys.time()-start_time, 2))
  return_final$run_time <- run_time

  cat(paste0("\n\n## Done in ", run_time, "\n\n"))

  ## return
  return(return_final)

  }
 
}

#----------------------------------------------------------------------------#
