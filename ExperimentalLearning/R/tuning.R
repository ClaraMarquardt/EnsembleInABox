#----------------------------------------------------------------------------#

#' @title *Implement k-fold cross-validation on a subset of the data to tune the parameters of a given model (according to a specified tuning strategy).
#' 
#' @description *.
#' 
#' @export
#' @import data.table
#' @import testit
#' @param df *Datatable containing the data [data.table].
#' @param LHS *Name of the independent variable [character].
#' @param RHS *Names of all dependent variables (may incl. group_var, control_var and id_var) [character].
#' @param cluster_var *Name of variable containing observation level cluster identifiers (integer) [character].
#' @param tuning_method *Tuning method [character].
#' @param algorithm *Name of individual algorithm to be tuned [character].
#' @param fold_in *Observations beloging to folds on which to train an algorithm [integer].
#' @param fold_out *Observations belonging to fold on which to predict [integer].
#' @param rand_grid_iter *Number of random grid search iterations (performed if tuning_method==""rand_grid"") [integer]. 
#' @param weight *Vector of observation weights [integer or numeric].
#' @param innerfold *Number of CV folds used in inner CV routine [integer].
#' @param fold_method *Method used to generate CV folds (and the train-holdout split) [cluster_count (target cluster count), obs_count (target observation count), stratification (target stratification)].
#' @param task *The nature of the prediction task  [regression (regression), classification (classification)].
#' @param losstype *Loss function used to tune the individiual learners and the ensemble neg_mse_brier, loglik, auc, accuracy, balanced_accuracy, r2].
#' @param output_path *Directory within which all model output (e.g. tuning graphs) is stored [path].
#' @param fold_out_id *Fold on which to predict [integer].
#' @param execution_id *Unique ID generated at the beginning of each model construction process - all output is associated with this ID [character].
#' @param max_core *The maximum number of cores to be used in all parallelised loops [integer].
#' @param tune_only *Whether to generate predictions (or to return only the tunes parameters or ensemble weights) [logical].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @return *A list of out of sample predictions along with the optimal parameters selected.
#' @examples

tuning <- function(df, LHS, RHS, fold_in, fold_out, algorithm, fold_out_id, 
            cluster_var=ExpLearning.Default$cluster_var, 
            tune_only=ExpLearning.Default$tune_only,
            tuning_method=ExpLearning.Default$tuning_method, custom_param=ExpLearning.Default$custom_param,
            weight_var=ExpLearning.Default$weight_var, fold_method=ExpLearning.Default$fold_method, 
            task=ExpLearning.Default$task,rand_grid_iter=ExpLearning.Default$rand_grid_iter,
            max_core=ExpLearning.Default$max_core,innerfold=ExpLearning.Default$innerfold,
            losstype=ExpLearning.Default$losstype,output_path=eval(parse(text=ExpLearning.Default$output_path)), 
            quiet=ExpLearning.Default$quiet, 
            execution_id=eval(parse(text=ExpLearning.Default$execution_id))) {

  # check tuning method
  assert("valid tuning_method (tuning())", tuning_method %in% c("naive_grid", "no_tuning", "rand_grid"))

  # Inialise
  tuning_method_temp         <- copy(tuning_method)
  alg_param                  <- copy(alg(algorithm)$parameter)

  alg_param_raw              <- copy(alg(algorithm)$parameter_raw)
  alg_param_tuning           <- copy(alg(algorithm)$parameter_tuning)
  alg_param_tuning_grid      <- copy(alg(algorithm)$parameter_tuning_grid)
  alg_param_tuning_rand      <- copy(alg(algorithm)$parameter_tuning_rand)
  alg_param_tuning_add       <- copy(alg(algorithm)$parameter_add)


  # Parameter modifications
  # ------------------
   
    ## [1] Custom parameters supplied 
    if (!is.null(custom_param)) {


      ## default parameters (change default) - i.e. each of the supplied parameters has one value
      if (length(custom_param[[grep(paste0(algorithm, "_outer_param"), names(custom_param), value=T)]]) == 
        length(c(unlist(custom_param[[grep(paste0(algorithm, "_outer_param"), names(custom_param), value=T)]])))) {

      if(quiet==0.5) print("Custom parameters detected - No tuning")

        for (i in names(alg_param_raw)) {
          if (i %in% names(custom_param[[grep(paste0(algorithm, "_outer_param"), names(custom_param), value=T)]])) {
           alg_param_raw[[i]] <- custom_param[[grep(paste0(algorithm, "_outer_param"), names(custom_param), value=T)]][[i]]
          }
        }
  
        # update tuning method
        tuning_method_temp <- "no_tuning"
  
        # reformat
        alg_param <- expand_split_grid(alg_param_tuning=alg_param_raw, 
                    param_split_method="grid")
    
      ## tuning parameters (change tuning) - i.e. at least one of the supplied parameters represents a grid
      } else if (length(custom_param[[grep(paste0(algorithm, "_outer_param"), names(custom_param), value=T)]]) <
        length(c(unlist(custom_param[[grep(paste0(algorithm, "_outer_param"), names(custom_param), value=T)]])))) {
  
        if(quiet==0.5) print("Custom parameters detected - Tuning (Rand Grid OR Naive Grid)")

        for (i in names(alg_param_raw)) {
          if (i %in% names(custom_param[[grep(paste0(algorithm, "_outer_param"), names(custom_param), value=T)]])) {

            alg_param_tuning[[i]]      <- custom_param[[grep(paste0(algorithm, "_outer_param"), names(custom_param), value=T)]][[i]]

          }
        }

        # reformat (note in the case of rand grid - select subset)
        alg_param_tuning_rand       <- expand_split_grid(alg_param_tuning=alg_param_tuning, 
                                        param_split_method="grid", iter=rand_grid_iter)
        alg_param_tuning_grid       <- expand_split_grid(alg_param_tuning=alg_param_tuning, 
                                        alg_param_default=alg_param_raw,  
                                        param_split_method="non_grid")

      }
    }

  # CROSS VALIDATION (INNER FOLD)
  # ------------------

  # TUNING
  # ------------------

  if(tuning_method_temp %in% c("naive_grid", "rand_grid") & 
    length(c(unlist(alg_param_tuning)))>length(alg_param_tuning) ) { 
    
    ## further modifications
    # ------------------

    ## modify RF pmtry parameter - ensure that only test values which result in 
    ## different numbers of predictors
    if (algorithm=="rf") {
    
      param_count                   <- ceiling(alg_param_tuning$pmtry * length(RHS))

      if (length(unique(param_count)) < length(param_count)) {
      
        pmtry_keep                  <- c(unlist(tapply(alg_param_tuning$pmtry, param_count, min)))
      
        if (!(ceiling(sqrt(length(RHS))) %in% ceiling(alg_param_tuning$pmtry * length(RHS)))) {
          pmtry_keep <- c(pmtry_keep, NA)
        }

        if(quiet==0.5) ps("Updating pmtry values - %s", paste0(pmtry_keep, collapse="/"))


        alg_param_tuning$pmtry      <- pmtry_keep
        alg_param_tuning_grid       <- lapply(alg_param_tuning_grid, function(x) x[which(sapply(x,function(y) 
                                        y$pmtry %in% pmtry_keep))])
        alg_param_tuning_rand       <- alg_param_tuning_rand[which(sapply(alg_param_tuning_rand,function(y) 
                                        y$pmtry %in% pmtry_keep))]
                
      }

    }


    # ------------------
    # RAND GRID
    # ------------------

    if (tuning_method_temp=="rand_grid") {

      # rand_grid --- select parameters
      alg_param_tuning_rand     <- alg_param_tuning_rand[c(1, sample(c(2:length(alg_param_tuning_rand)), 
                                    min(rand_grid_iter-1, length(alg_param_tuning_rand)-1)))]

      # generate inner folds
      thisdf    <- gen_fold(df=df[fold_in,], nfold = innerfold, cluster_var=cluster_var,
                     fold_var="inner_fold", stratify_var=LHS, fold_method=fold_method, 
                     weight_var=weight_var, quiet = quiet)

      ## random tuning loop
      return_mult[phat, mean_loss, pick_me, pick_param] <- tuning_grid(df=thisdf, 
        LHS=LHS, RHS=RHS, algorithm=algorithm, alg_param_grid=alg_param_tuning_rand, 
        weight_var=weight_var, task=task, max_core=max_core, losstype=losstype, quiet=quiet)
    
     if (quiet==0.5) {
       cat("\n")
       print(alg_param_tuning_rand[[pick_me]])
       cat("\n")
     }

     ## IF XGB - TUNE (a) GAMMA/ (b) ETA/NUMBER OF TREES AT THE OPTIMAL SET OF PARAMETERS
     if (algorithm=="xgb") {

      ## stage 2 - tune gama
      xgb_gamma_tune       <- as.list(alg_param_tuning_rand[[pick_me]])
      xgb_gamma_tune$gamma <- alg_param_tuning_add$gamma
      xgb_gamma_tune       <- expand_split_grid(alg_param_tuning=xgb_gamma_tune, 
                                        param_split_method="grid")

      return_mult[phat_gamma, mean_loss_gamma, pick_me_gamma, 
        pick_param_gamma] <- tuning_grid(df=thisdf, 
        LHS=LHS, RHS=RHS, algorithm=algorithm, alg_param_grid=xgb_gamma_tune, 
        weight_var=weight_var, task=task, max_core=max_core, losstype=losstype, 
        quiet=quiet)

      alg_param_tuning_rand[[pick_me]]$gamma <- xgb_gamma_tune[[pick_me_gamma]]$gamma
      mean_loss[[pick_me]]                   <- mean_loss_gamma[[pick_me_gamma]]

      if (quiet==0.5) {
       cat("\n")
       print(alg_param_tuning_rand[[pick_me]])
       cat("\n")
      }

      # stage 3 - tune eta and nrounds
      xgb_cv_tune         <- as.list(alg_param_tuning_rand[[pick_me]])
      xgb_cv_tune$eta     <- alg_param_tuning_add$eta
      xgb_cv_tune         <- expand_split_grid(alg_param_tuning=xgb_cv_tune, 
                                        param_split_method="grid")  

      return_mult[mean_loss_cv, pick_me_cv, pick_me_eta, pick_me_nrounds]  <- alg(algorithm)$algorithm(df=thisdf,
          LHS=LHS,RHS=RHS, from=c(1:nrow(thisdf)),to=c(1:nrow(thisdf)), weight_var = weight_var, 
          alg_param=xgb_cv_tune, task=task, quiet=quiet,alg_mode="tree_cv",
          losstype=losstype)
      
      alg_param_tuning_rand[[pick_me]]$nrounds <- pick_me_nrounds
      alg_param_tuning_rand[[pick_me]]$eta     <- pick_me_eta
      mean_loss[[pick_me]]                     <- mean_loss_cv[[pick_me_cv]]

      if (quiet==0.5) {
       cat("\n")
       print(alg_param_tuning_rand[[pick_me]])
       cat("\n")
      }

    }

    ## final selection
    pick_param <- alg_param_tuning_rand[[pick_me]]

    ## TUNING PLOT
    if(quiet<2) {

      # generate and save plot
      agg_tuning_plot(tuned_loss=mean_loss,losstype=losstype, alg_param=alg_param_tuning_rand,
        alg_param_opt=pick_me, output_path=output_path, algorithm=algorithm, 
        fold_out_id=fold_out_id, execution_id=execution_id, tuning_method=tuning_method_temp, 
        quiet=quiet)


    }

    # ------------------
    # NAIVE GRID
    # ------------------
    } else if (tuning_method_temp=="naive_grid") {

      # generate inner folds
      thisdf    <- gen_fold(df=df[fold_in,], nfold = innerfold, cluster_var=cluster_var,
                     fold_var="inner_fold", stratify_var=LHS, fold_method=fold_method, 
                     weight_var=weight_var, quiet = quiet)

      pick_param_list <- copy(alg_param)

      # loop over individual parameters
      for(j in 1:length(alg_param_tuning)) {

        if (length(alg_param_tuning_grid[[j]])>1) {

          ## random tuning loop
          return_mult[phat, mean_loss, pick_me, pick_param] <- tuning_grid(df=thisdf, 
            LHS=LHS, RHS=RHS, algorithm=algorithm, alg_param_grid=alg_param_tuning_grid[[j]], 
            weight_var=weight_var, task=task, max_core=max_core, losstype=losstype, quiet=quiet)

          pick_param_list[[1]][[names(alg_param_tuning)[j]]] <- alg_param_tuning[[j]][pick_me]

          if (quiet==0.5) {
            cat("\n")
            print(pick_param_list)
            cat("\n")
          }

          ## TUNING PLOT
          if(quiet<2) {
     
            # generate and save plot
            agg_tuning_plot(tuned_loss=mean_loss,losstype=losstype, alg_param=alg_param_tuning_grid[[j]],
                alg_param_opt=pick_me, output_path=output_path, algorithm=algorithm, 
                tuning_method=paste0(tuning_method_temp, " - tuning: ", names(alg_param_tuning)[j]), 
                fold_out_id=fold_out_id, execution_id=execution_id, quiet=quiet)
          }

        } else {
            
          pick_param_list[[1]][[names(alg_param_tuning)[j]]] <- alg_param_tuning[[j]][1]

        }
      }
    
    ## final selection
    pick_param <- pick_param_list[[1]]

    }

  # ------------------
  # NO TUNING
  # ------------------
  } else if (tuning_method_temp=="no_tuning" | length(c(unlist(alg_param_tuning)))<=length(alg_param_tuning)) { 
    
    pick_param <- alg_param[[1]]

  }

  # PREDICTIONS (OUTER FOLD)
  # -----------------------
  if (tune_only==FALSE) {

    pred <- alg(algorithm)$algorithm(df=df, LHS=LHS, RHS=RHS, from=fold_in, to=fold_out, 
              weight_var=weight_var, alg_param=pick_param, task=task, quiet=quiet)

    pred_loss     <- loss(pred$pred,df[fold_out, get(LHS)], losstype=losstype, 
                        weight=df[fold_out, get(weight_var)], task=task)
    pred_loss_auc <- loss(pred$pred, df[fold_out, get(LHS)], losstype="auc", 
                        weight=df[fold_out, get(weight_var)], task=task)

    return(list(pred=pred$pred, param=list(pick_param), learner=pred$learner, pred_loss=pred_loss, 
      pred_loss_auc=pred_loss_auc))

 
  } else {

    return(list(pred=NULL, param=list(pick_param),learner=NULL, pred_loss=NULL, pred_loss_auc=NULL))

  }


}

#----------------------------------------------------------------------------#
