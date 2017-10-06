#----------------------------------------------------------------------------#

#' @title *Generate an in-sample fitted ensembled based on in-sample tuned out of sample predictions.
#'
#' @description *.
#'
#' @export
#' @param df *Datatable containing the data [data.table].
#' @param LHS *Name of the independent variable [character].
#' @param RHS *Names of all dependent variables (may incl. group_var, control_var and id_var) [character].
#' @param cluster_var *Name of variable containing observation level cluster identifiers (integer) [character].
#' @param weight *Vector of observation weights [integer or numeric].
#' @param predictor *List of algorithms included in ensemble learner [ols (linear regression), rf (random forest (classification OR regression)), avg (simple mean), elnet (elastic net), logit (logistic regression), tree (tree (classification or regression)), xgb (gradient boosted tree (classification or regression))] [character].
#' @param innerfold *Number of CV folds used in inner CV routine [integer].
#' @param tuning_method *Tuning method [character].
#' @param rand_grid_iter *Number of random grid search iterations (performed if tuning_method==""rand_grid"") [integer]. 
#' @param fold_var *Name of variable containing observation level fold identifiers (integer) [character].
#' @param fold_method *Method used to generate CV folds (and the train-holdout split) [cluster_count (target cluster count), obs_count (target observation count), stratification (target stratification)].
#' @param task *The nature of the prediction task  [regression (regression), classification (classification)].
#' @param custom_param *Custom tuning parameters supplied to the prediction function and used in place of the default values (if tuning_method==""no_tuning"") [list - misc]. 
#' @param losstype *Loss function used to tune the individiual learners and the ensemble neg_mse_brier, loglik, auc, accuracy, balanced_accuracy, r2].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @param max_core *The maximum number of cores to be used in all parallelised loops [integer].
#' @param tune_only *Whether to generate predictions (or to return only the tunes parameters or ensemble weights) [logical].
#' @param execution_id *Unique ID generated at the beginning of each model construction process - all output is associated with this ID [character].
#' @param output_path *Directory within which all model output (e.g. tuning graphs) is stored [path].
#' @param ensemble_agg *Method used to aggregate the individual learners into an ensemble learner [ols  (linear regression on individual learners predictions in the case of a regression task vs. logit regression on individual learner predictions in the case of a classification task), nnls (weights derived through non-negative least squares)]. [character].
#' @return *A list of ensemble weights and tuning parameters selected for each of the individual learners.
#' @examples


ensemble_predict <- function(df, LHS, RHS, 
                      cluster_var=ExpLearning.Default$cluster_var, 
                      custom_param=ExpLearning.Default$custom_param,  
                      weight_var=ExpLearning.Default$weight_var, 
                      predictor=ExpLearning.Default$predictor, 
                      innerfold=ExpLearning.Default$innerfold, 
                      quiet=ExpLearning.Default$quiet, 
                      rand_grid_iter=ExpLearning.Default$rand_grid_iter, 
                      fold_var=ExpLearning.Default$fold_var, 
                      task=ExpLearning.Default$task, 
                      max_core=ExpLearning.Default$max_core, 
                      fold_method=ExpLearning.Default$fold_method, 
                      losstype=ExpLearning.Default$losstype, 
                      output_path=eval(parse(text=ExpLearning.Default$output_path)), 
                      tuning_method=ExpLearning.Default$tuning_method, 
                      ensemble_agg=ExpLearning.Default$ensemble_agg,  
                      tune_only=ExpLearning.Default$tune_only, 
                      execution_id=eval(parse(text=ExpLearning.Default$execution_id))) {
  
  # count how many algorithms being used
  numalg <- length(predictor)
  
  if( numalg > 1 ) {
    
    # sets up matrix with one row per obs and one column per algorithm
    phat             <- matrix(rep(NA, nrow(df) * length(predictor)), ncol = length(predictor))
    phat_cv_loss     <- list()
    phat_cv_loss_auc <- list()

    dv_vector        <- df[,get(LHS)]
    param_list       <- list()
    
    if(quiet<2) { 
      cat(paste0("### Calculating ensemble weights for ", paste0(unlist(predictor), collapse=", "), "\n")) 
    }
    
    for(a in 1:length(predictor)) {
      
      if(quiet<2) cat(paste0("\n\nFitting ", predictor[[a]]), "\n")

      algoreturn <- tuned_predict(df=df,LHS=LHS,RHS=RHS,cluster_var=cluster_var, custom_param=custom_param, 
                                 algorithm=predictor[[a]],
                                 weight_var = weight_var, tuning_method = tuning_method, 
                                 rand_grid_iter=rand_grid_iter,
                                 max_core=max_core, 
                                 fold_var=fold_var, fold_method=fold_method, task=task, 
                                 losstype = losstype, output_path = output_path, 
                                 innerfold=innerfold,quiet=quiet, 
                                 execution_id=execution_id)

      phat[,a]                        <- algoreturn$pred
      param_list[[predictor[[a]]]]    <- algoreturn$param
      phat_cv_loss[a]                 <- algoreturn$cv_loss
      phat_cv_loss_auc[a]             <- algoreturn$cv_loss_auc
 

    }
    
    if(quiet<2) { 
      
      cat("\n\n")
      cat(paste0("Learner-Specific OOS/CV* auc (tuned/fitted !n /predict n) : ",paste(apply(phat, 2, function(x) 
        round(loss(x,dv_vector,  losstype="auc", task=task, weight=df[, get(weight_var)]),3)), collapse=" / "), "\n"))
      cat(paste0("Learner-Specific OOS/CV* ", losstype, " (tuned/fitted !n /predict n) : ",paste(apply(phat, 2, 
        function(x) round(loss(x,dv_vector,losstype=losstype, task=task, weight=df[, get(weight_var)]),3)), collapse=" / "), "\n"))

    }

    if (quiet==0.5) {
      cat("\n\n")
      cat(paste0("Learner-Specific CV auc : ", paste(round(c(unlist(phat_cv_loss_auc)), 3), collapse=" / "), "\n"))
      cat(paste0("Learner-Specific CV ",losstype," : ", paste(round(c(unlist(phat_cv_loss)), 3), collapse=" / "), "\n"))
  
    }
       
    if(ensemble_agg %in% c("ols", "nnls")) {
        
        phat_mod           <- matrix(c(phat), ncol=ncol(phat))
        phat_mod           <- data.frame(phat_mod)
        names(phat_mod)    <- c(predictor)

        # Using OLS to fit ensemble weights
        if(task=="classification") {

          if (all(df[, get(weight_var)]==1)) {
            stack_model_ols <- glm(dv_vector ~ .-1, family="binomial", 
              data=phat_mod)
          } else {
            stack_model_ols <- glm(dv_vector ~ .-1, data=phat_mod, 
              weights=df[, get(weight_var)], family="quasibinomial")           
          }
        } else if (task=="regression") {
          stack_model_ols <- lm(dv_vector ~ .-1, 
            data=phat_mod, weights=df[, get(weight_var)])

        }

        ensemble_weight_ols <- as.vector(coef(stack_model_ols))
        ensemble_weight_ols[is.na(ensemble_weight_ols)] <- 0

        # Using NLS to fit ensemble weights (model: https://github.com/ecpolley/SuperLearner/blob/master/R/method.R)
        stack_model_nnls         <- nnls(sqrt(df[, get(weight_var)]) * as.matrix(phat_mod), 
                                      sqrt(df[, get(weight_var)]) * dv_vector)
        ensemble_weight_nnls     <- as.vector(coef(stack_model_nnls))
        ensemble_weight_nnls[is.na(ensemble_weight_nnls)] <- 0
        
        stack_model_nnls <- ensemble_weight_nnls

        if (ensemble_agg=="ols") {

            ensemble_weight <- ensemble_weight_ols
            stack_model     <- stack_model_ols

        } else if (ensemble_agg=="nnls") {

            ensemble_weight  <- ensemble_weight_nnls
            stack_model      <- stack_model_nnls

        }

      } else {

      ensemble_weight <- c(0,1)

    }
    
    if (tune_only==FALSE) {

       if (ensemble_agg=="ols") {
         prediction      <- predict(stack_model, newdata=phat_mod, type="response")
         prediction_nnls <- as.vector(as.vector(stack_model_nnls)%*%t(as.matrix(phat_mod)))
    
       } else if(ensemble_agg=="nnls") {
         prediction      <- as.vector(as.vector(stack_model)%*%t(as.matrix(phat_mod)))
         prediction_nnls <- prediction
       }
    
       if (quiet==0.5) {
         print("OOS/CV* Loss - Ensemble (tuned/fitted !n /predict n + ensemble weights IS):")
         print(loss(prediction, dv_vector, losstype="auc", weight=df[, get(weight_var)]))
         print(loss(prediction, dv_vector, losstype="neg_mse_brier",weight=df[, get(weight_var)]))
       }
       
       # individual prediction list
       ind_pred        <- lapply(data.table(phat),function(x) x)
       names(ind_pred) <- predictor
    
    } else {

    prediction      <- NULL
    prediction_nnls <- NULL
    ind_pred        <- NULL

    }
  
  } else {

    # Only one algorithm and no prediction required at this point, so return trivial weight
    ensemble_weight      <- c(1)
    ensemble_weight_nnls <- c(1)
    stack_model          <- c(1)
    stack_model_nnls     <- c(1)
    prediction      <- NULL
    prediction_nnls <- NULL
    param_list      <- NULL
    ind_pred        <- NULL

  }

   if(quiet<2) { 
    
    cat("\n\n")
    cat(paste0("Chosen weights are ",paste(round(ensemble_weight, 3),collapse = "/"), "\n"))
    cat("\n\n") 
  } 

  return(list(ensemble_weight=ensemble_weight, ensemble_weight_nnls=ensemble_weight_nnls, 
    prediction=prediction, param=param_list, ind_pred=ind_pred, prediction_nnls=prediction_nnls, 
    stack_model=list(model=stack_model, nnls=stack_model_nnls)))
}

#----------------------------------------------------------------------------#

