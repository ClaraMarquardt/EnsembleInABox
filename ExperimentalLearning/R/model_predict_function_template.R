#----------------------------------------------------------------------------#

#' @title *Ensemble prediction function template.
#'
#' @description *.
#'
#' @examples

model_predict_function_template <- function(...) {

    # data prep
    df_temp <- copy(df[, mget(c(RHS_arg, LHS_arg))])

    # check that complete cases 
    assert("complete df (model_predict_function_template())", 
      nrow(df_temp)==nrow(df_temp[complete.cases(df_temp)]))

    if(quiet==FALSE) ps("nrow: ", nrow(df_temp))
  
    if (length(learner) < length(learner_list_arg)) {

      if(quiet==FALSE) print("Using subset of learners")

      learner_list_arg <- learner_list_arg[c(learner)]

    }

    # matrix with one row per obs and one column per algorithm
    phat <- matrix(rep(NA, nrow(df_temp) * length(learner_list_arg)), 
     ncol = length(learner_list_arg))

    # generate learner predictions
    for(i in 1:length(learner_list_arg)) {

      if (names(learner_list_arg[i])=="intercept") {

        # intercept
        if (quiet==FALSE) print("intercept")
        phat[,i] <- learner_list_arg[[i]]
        if (quiet==FALSE) print(head(phat[,i]))

      } else if (names(learner_list_arg[i])=="lm") {

        # lm
        if (quiet==FALSE) print("lm")
        phat[,i] <- predict(learner_list_arg[[i]], newdata=df_temp)
        if (quiet==FALSE) print(head(phat[,i]))

      } else if (names(learner_list_arg[i])=="xgb")  {

        # xgb
        if (quiet==FALSE) print("xgb")

        learner_temp <- copy(learner_list_arg[[i]])
        learner_temp$params$nthread <- 1

        df_temp_xgb <- copy(df_temp)

        ## need to ensure that at least one numeric var
        if (sum(sapply(df_temp_xgb[, mget(c(RHS_arg))], class)=="integer")>0) {
          integer_var <- RHS_arg[sapply(df_temp_xgb[, mget(c(RHS_arg))], class)=="integer"]
          df_temp_xgb[, c(integer_var):=lapply(.SD, function(x) as.numeric(x)), 
            .SDcols=integer_var]
        }
        
        df_temp_xgb  <- xgb.DMatrix(data.matrix(df_temp_xgb[, mget(c(RHS_arg))]))
        phat[,i]     <- predict(learner_temp, df_temp_xgb, missing=NaN)
        if (quiet==FALSE) print(head(phat[,i]))

      } else if (names(learner_list_arg[i])=="avg") {

        # avg
        if (quiet==FALSE) print("avg")
        phat[,i] <- learner_list_arg[[i]]
        if (quiet==FALSE) print(head(phat[,i]))

      } else if (names(learner_list_arg[i])=="logit") {

        # logit
        if (quiet==FALSE) print("logit")
        phat[,i] <- c(unlist(as.data.table(predict(learner_list_arg[[i]], newdata=df_temp, 
          type="response"))[, c(1),with=F]))
        if (quiet==FALSE) print(head(phat[,i]))

      } else if(names(learner_list_arg[i])=="rf") {

        # rf
        if (quiet==FALSE) print("rf")
        pred_raw <- predict(learner_list_arg[[i]], df_temp, 
          type="response")$predictions

        if (task_arg=="classification") {
          phat[,i] <- pred_raw[,which(colnames(pred_raw)=="1")]
     
        } else { 
          
           phat[,i] <- pred_raw
        }
        
        if (quiet==FALSE) print(head(phat[,i]))


      } else if(names(learner_list_arg[i])=="regtree") {

        # regtree
        if (quiet==FALSE) print("regtree")
        phat[,i] <- predict(learner_list_arg[[i]], df_temp)
        if (quiet==FALSE) print(head(phat[,i]))

      } else if (names(learner_list_arg[i])=="elnet") {

        # elnet
        if (quiet==FALSE) print("elnet")        
        phat[,i]     <- predict(learner_list_arg[[i]][[1]], s=learner_list_arg[[i]][[2]], 
          newx=as.matrix(df_temp[,mget(RHS_arg)]), type="response", exact=FALSE)
        if (quiet==FALSE) print(head(phat[,i]))

      } else if (names(learner_list_arg[i])=="svm") {

        # svm
        if (quiet==FALSE) print("svm")        
        
        # generate predictions
        if (task_arg=="classification") {
   
          pred    <- predict(learner_list_arg[[i]],  df_temp, probability=TRUE)
          pred    <- attr(pred, "probabilities")[,which(colnames(attr(pred, "probabilities"))==1)]


        } else if (task_arg=="regression") {

           pred       <- predict(learner_list_arg[[i]],  df_temp)

        }

        phat[,i]     <- c(unlist(pred))
        if (quiet==FALSE) print(head(phat[,i]))


      }

    }

    if (length(learner_list_arg)>1) {
      
      # predict linearly according to ensemble
      phat_mod           <- matrix(c(phat), ncol=ncol(phat))
      phat_mod           <- data.frame(phat_mod)
      names(phat_mod)    <- c(predictor)
  
      if (class(stack_model_arg)[1] %in% c("lm", "glm") & length(learner_list_arg) >1) {
        phat_ensemble   <- predict(stack_model_arg, newdata=phat_mod, 
          type="response")
      } else {
        phat_ensemble   <- as.vector(as.vector(stack_model_arg)%*%t(as.matrix(phat_mod)))
      }

    } else {

      phat_ensemble <- phat[,1]

    }
    
    return(phat_ensemble)

}

#----------------------------------------------------------------------------#
