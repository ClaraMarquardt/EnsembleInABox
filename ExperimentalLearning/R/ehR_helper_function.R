#----------------------------------------------------------------------------#

#' Generate a function programatically. 
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

make_function <- function(arg, body, env = parent.frame()) {

      f <- function() {}
      formals(f) <- arg
      body(f)    <- body
      environment(f) <- env 

      return(f)

}

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' Invisible lapply.
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

inv_lapply <- function(X, FUN,...) {

  invisible(lapply(X, FUN,...))

}

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' Invisible mapply.
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

inv_mapply <- function(FUN,...) {

  invisible(mapply(FUN,...))

}

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' @title Encode categorical values in data.table. 
#'
#' @description Expand each categorical value into a full set of dummies. 
#'
#' @export
#' @import data.table
#' @param dt data.table containining the data [data.table].
#' @param var_list Names of variables which are to be encoded [character].
#' @param drop Whether to omit one category per variable (to avoid multicolinearity) [logical].
#' @return Modified data.table.
#' @examples

 one_hot_encoding <- function (dt, var_list, drop = FALSE) {

    contr.onehot = function(n, contrasts, sparse = FALSE) {
        contr.sum(n = n, contrasts = FALSE, sparse = sparse)
    }
    current.na.action <- options("na.action")
    options(contrasts = c("contr.onehot", "contr.onehot"))
    options(na.action = "na.pass")

    dt_factor <- dt[, c(var_list), with=F]
    dt_factor <- lapply(var_list, function(x) {
        
        # rename 
        dt_factor[, c(x):=lapply(.SD, function(y) paste0("_", gsub("(_*)$", "", y))), 
            .SDcols=c(x), by=1:nrow(dt_factor)]

        # cast
        dt_factor_temp <- data.frame(model.matrix(~. - 1, data = dt_factor[,
            mget(x)]))

        # drop one if only two columns
        if (ncol(dt_factor_temp)==2 & names(dt_factor_temp)[1] %like% "(0|1)$") {
            name_orig <- gsub("(0|1)$", "", names(dt_factor_temp)[1])
            dt_factor_temp <- dt_factor_temp[, 1]
            dt_factor_temp <- data.frame(dt_factor_temp)
            names(dt_factor_temp) <- name_orig
        }

        # drop one column - avoid colinearity
        if (drop == TRUE & ncol(dt_factor_temp)>1) {
            dt_factor_temp[[names(dt_factor_temp)[1]]] <- NULL
        }
        return(dt_factor_temp)
    })

    dt_factor <- data.frame(dt_factor)
    dt_factor <- as.data.table(dt_factor)

    # drop factor columns which end in NA
    na_col   <- grep("_NA$", names(dt_factor), value=T)
    dt_factor[, c(na_col):=NULL]

    dt_factor[,]
    # format
    dt_factor[, `:=`(names(dt_factor), lapply(.SD, function(x) as.integer(x)))]
    dt_non_factor <- dt[, c(setdiff(names(dt), var_list)), with=F]
    
    # check that row count aligns
    if (nrow(dt_factor) != nrow(dt_non_factor)) 
      print("warning - one hot encoding () - it appears that rows are dropped during the conversion")

    dt_temp <- data.table(data.frame(dt_non_factor, dt_factor))

    options(na.action = current.na.action)

    # return
    return(dt_temp)
}

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' Save R objects to different formats.
#' 
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

out <- function(obj, path) {

  # purpose: 
  # save .csv,.pdf,.Rds files

  if (path %like% "csv") {

    write.csv(obj, path, row.names=F)

  } else if(path %like% "pdf|jpeg|png") {

    ggsave(obj, path)

  }  else if(path %like% "Rds") {

    saveRDS(obj, path)

  } else if (path %like% "txt") {

  	write.table(obj, path)

  }

}

#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

#' Calculate a percentage. 
#' @export
#' @param num Numerator [numeric]
#' @param denom Denominator [numeric]
#' @param digit Number of digits to round to [integer]
#' @return Percentage [numeric]
#' @examples
#' TBC

perc <- function(num, denom, digit=1) {

  round((num/denom)*100, digit)

}

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' Permutation based F/t-test
#' @export
#' @param TBC
#' @return TBC
#' @examples TBC

perm_param_test <- function(df, assignment_var, group_var, permutation=ExpLearning.Default$permutation, 
                    cluster_var=ExpLearning.Default$cluster_var,
                    max_core=ExpLearning.Default$max_core, test=ExpLearning.Default$test) {

  # generate one master sur fit - if use  

  if (test=="wald_ind_independent") {

    temp_fit_sample <- systemfit(group_var, data=df)

  }

  # obtain the permutation stat (include model stat)
  perm_stat  <- mclapply_robust(X=1:(permutation+1), FUN=function(i) {

    # ensure that the actual treatment assignment is included as one of the permutations
    if(i==1) {

      perm_df <- copy(df)

    } else {

      perm_df <- perm(df, assignment_var, cluster_var=cluster_var, 
                    fold_var=fold_var, within_fold=FALSE, 
                    control_var=NULL, control_var_unbalanced=NULL, 
                    perm_strategy="entire_cluster", perm_test="baseline_signal")

    }

    if (test=="t_ind") {

      x1 <- perm_df[get(assignment_var) == 0, get(group_var)]
      x2 <- perm_df[get(assignment_var) == 1, get(group_var)]

      temp <- t.test(x1, x2)$statistic

     
    } else if (test=="t_clust") {

      temp <-  t.test.cluster(perm_df[, get(group_var)], perm_df[, get(cluster_var)], 
                  perm_df[, get(assignment_var)])[19]


    } else if (test=="wald_ind") {

      hypothesis_matrix            <- matrix(0, nrow=length(group_var), 
                                          ncol=(length(group_var)*2))
        
      for (i in 1:length(group_var)) {
            hypothesis_matrix[i,(i*2)]   <- 1
        }

      qvec                       <- rep(0,length(group_var))

      fit_sur                    <- systemfit(group_var, data=perm_df)
 
      temp                       <- linearHypothesis(fit_sur,hypothesis_matrix,qvec, 
                                        test = "F")$F[2]

    }  else if (test=="wald_clust") {

      hypothesis_matrix            <- matrix(0, nrow=length(group_var), 
                                          ncol=(length(group_var)*2))
        
      for (i in 1:length(group_var)) {
            hypothesis_matrix[i,(i*2)]   <- 1
        }

      qvec        <- rep(0,length(group_var))

      fit_sur     <- systemfit(group_var, data=perm_df)
      

      # robust cov matrix
      fit_sur_clust_robust_cov   <-  systemfit_var(group_var, perm_df, 
                                          clust="clust",
                                          cluster_var=cluster_var) 

      temp       <- linearHypothesis(systemfit(group_var, 
                     data=perm_df),hypothesis_matrix,qvec, test = "F", 
                      vcov=fit_sur_clust_robust_cov)$F[2]

    }  else if (test=="wald_ind_independent") {


     hypothesis_matrix            <- matrix(0, nrow=length(group_var), 
                                          ncol=(length(group_var)*2))
        
      for (i in 1:length(group_var)) {
            hypothesis_matrix[i,(i*2)]   <- 1
        }

      qvec                       <- rep(0,length(group_var))

      fit_sur                    <- systemfit_unrelated(formula=group_var, 
                                      df=perm_df, sample_systemfit=temp_fit_sample)
 
      temp                       <-  linearHypothesis(fit_sur,hypothesis_matrix,qvec, 
                                        test = "F")$F[2]

    }

    return(temp)


  }, max_core=max_core)

  # determine the p-values 
  model_stat <- perm_stat[[1]]
  perm_stat  <- c(unlist(perm_stat[2:length(perm_stat)]))

  if (test %like% "^t") {
    p    <- mean(perm_stat>=abs(model_stat) | perm_stat<=-abs(model_stat))
  } else if (test %like% "^wald") {
    p    <- mean(perm_stat>=model_stat)
  }

  # return the p values, permuted losses and model losses
  return(list(p=p, model_stat=model_stat, perm_stat=perm_stat))

}

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' Print(sprintf(...).
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

ps <- function(char_string, ...) {

     print(sprintf(char_string, ...))
}

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' Store multiple arguments returned from function as separate objects - function. 
#' @export
#' @return TBC
#' @examples
#' \dontrun{
#' within function_test: return(list(obj_1,obj_2, obj_3))
#' outside of function: return_mult[obj_1, obj_2, obj_3] <- function_test()
#' }

"[<-.result"  <- function(x,...,value) {

   args <- as.list(match.call())
   args <- args[-c(1:2,length(args))]
   length(value) <- length(args)
   for(i in seq(along=args)) {
     a <- args[[i]]
     if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
   }
   x
}


#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' Store multiple arguments returned from function as separate objects - structure. 
#' @export
#' @return TBC
#' @examples
#' \dontrun{
#' within function_test: return(list(obj_1,obj_2, obj_3))
#' outside of function: return_mult[obj_1, obj_2, obj_3] <- function_test()
#' }

return_mult <- structure(NA,class="result")


#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' Replace (in place) empty values ("[ ]*" or "") in data.table with another value.
#' @export
#' @param name Name of data.table [character]
#' @param replace Value with which to replace empty values  [any]
#' @return Data.table modified in place
#' @examples
#' TBC

set_missing_na <- function(dt, replace=NA, subset_col=names(dt)) {

  for (j in which(names(dt) %in% subset_col))
    set(dt, which(gsub("[ ]*", "", dt[[j]])==""), j, replace)
}

#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#

#' Replace (in place) NAs/+inf/-inf in data.table with another value.
#' @export
#' @param name Name of data.table [character]
#' @param replace Value with which to replace Nas/+inf/-inf  [any]
#' @return Data.table modified in place
#' @examples
#' TBC

set_na_zero <- function(dt, replace=0, subset_col=names(dt)) {

  for (j in which(names(dt) %in% subset_col))
    set(dt, which(is.na(dt[[j]]) | dt[[j]] %in% c(-Inf, +Inf) ), j, replace)

}

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' Replace (in place) zeros in in data.table with another value.
#' @export
#' @param name Name of data.table [character]
#' @param replace Value with which to replace 0s  [any]
#' @return Data.table modified in place
#' @examples
#' TBC

set_zero_na <- function(dt, replace=NA) {

  for (j in seq_len(ncol(dt)))
    set(dt, which(dt[[j]] %in% c(0)), j, replace)
}

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' Generate a systemfit object from a truly unrelated system of regressions (equivalent to systemfit vs. faster). 

#' @export
#' @import data.table
#' @import testit
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

systemfit_unrelated <- function(formula, df, sample_systemfit) {
  

  # ensure that sample fit object has the correct number of dimensions
  assert("appropriate sample_systemfit object - length (systemfit_unrelated())", 
    length(formula)==length(sample_systemfit$eq))

  # ensure that sample fit object has the same Xs

  # estimate each model
  temp_model <- lapply(formula, function(x) {

    temp <- lm(x, data=df)

  })

  # replace terms in sample_systemfit

  ## generate 'blank' var_cov matrix
  sample_systemfit$coefCov <- matrix(rep(0, 
      (length(temp_model)*length(coef(temp_model[[1]])))^2),
      ncol= (length(temp_model)*length(coef(temp_model[[1]]))))

  ## modify terms
  for (i in 0:(length(temp_model)-1)) {

    # key - coefficients (aggregate)
    sample_systemfit$coefficients[seq(from=((i)* 
      length(coef(temp_model[[i+1]])))+1,
      to=((i+1)*
      length(coef(temp_model[[i+1]]))))]           <- coef(temp_model[[i+1]])
  
    # key var cov (as not externally supplied)
    sample_systemfit$coefCov[seq(from=length(coef(temp_model[[i+1]]))*i+1, 
              to=length(coef(temp_model[[i+1]]))*i+(length(coef(temp_model[[i+1]])))), 
              seq(from=length(coef(temp_model[[i+1]]))*i+1, 
              to=length(coef(temp_model[[i+1]]))*i+
              (length(coef(temp_model[[i+1]]))))]  <- vcov(temp_model[[i+1]])
    
   }

  # key - degree of freedoms
  sample_systemfit$df.residual <- nrow(df)*length(temp_model)-
              length(temp_model)*length(coef(temp_model[[1]]))

  return(sample_systemfit)

}

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' Determine the significance (star format) of a p-value. 
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

sign_star <- function(p_value) {

  p_value <- as.numeric(gsub("<|>", "", p_value))

  ## *: Sign at 10%, **: Sign at 5%, *: Sign at 1%
  if (!is.na(p_value[1])) {

      sign_temp <- ifelse(p_value < 0.01, "***", ifelse(p_value <
          0.05, "**", ifelse(p_value < 0.1, "*", "    ")))
      
      return(sign_temp)

    } else {

      return("    ")
    }

}

#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
#' @title ggplot theme.
#'
#' @description ddd
#'
#' @export
#' @import ggplot2
#' @return
#' @examples

theme_basic <-  function(axis_size=0.5, title_size=8, subtitle_size=6, 
                  font_gen ="URWHelvetica", col_gen="grey50")  
  theme_bw() +
  theme(
    axis.text.x = element_text(size=rel(axis_size), colour = col_gen,
      family=font_gen),
    axis.text.y = element_text(size=rel(axis_size), colour = col_gen,
      family=font_gen), 
    axis.title.x = element_text(size=rel(axis_size), colour = col_gen,
    family=font_gen),
      axis.title.y = element_text(size=rel(axis_size), colour = col_gen,
    family=font_gen),
    plot.title = element_text(size = title_size, colour = col_gen, face = "bold",
      family=font_gen),
    plot.subtitle = element_text(size = subtitle_size, colour = col_gen, 
      face = "plain",family=font_gen),
    plot.caption = element_text(size = (subtitle_size-1), colour = col_gen, 
      face = "plain",family=font_gen)
  )

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' @title ggplot theme.
#'
#' @description \
#'
#' @export
#' @import ggplot2
#' @return
#' @examples

theme_legend_bottom <- function(title_size=0.5, text_size=0.4, tick_size=0.08,
  legend_width=0.5, legend_height=0.2, hjust_title=0.5, font_gen ="URWHelvetica", 
  col_gen  ="grey50") 

  theme(
   	legend.position="bottom", 
   	legend.key.height=unit(legend_height,"cm"),
   	legend.key.width=unit(legend_width,"cm"),
   	axis.ticks.length=unit(tick_size,"cm"),
   	legend.title=element_text(size=rel(title_size), colour=col_gen, family=font_gen, 
      hjust=hjust_title, face="plain"),
   	legend.text=element_text(size=rel(text_size), colour=col_gen, family=font_gen))
  


#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#

#' Ordered frequency table formatted as a data.table.
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

table_mod <- function(x) {

  table(x, useNA="always")[order(-table(x, useNA="always"))]
}

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' Merge list of data.tables on a given set of variables.
#' @export
#' @param x,y Names of DT [object]
#' @param by Names of variables on which to merge  [character vector]
#' @return Merged data.table
#' @examples
#' TBC

mymerge <- function(x,y, var_list=cohort_key_var) {

  merge(x,y, all = TRUE, by=var_list)

}

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' mclapply function useable across Mac/Windows. 
#' @export
#' @import parallel
#' @param  TBC
#' @return TBC
#' @examples
#' #' \dontrun{
#'  result_robust <- mclapply_robust(X=seq(1:100), FUN=function(x) x*3)
#'  result        <- mclapply(seq(1:100), function(x) x*3)
#' }

mclapply_robust <- function(max_core=8, quiet=TRUE, noparallel=FALSE, ...) {

  ## check platform
  if( Sys.info()[['sysname']] == 'Windows' | noparallel ==TRUE) {

    if (quiet==FALSE) cat(sprintf("\nwindows: %s / sequential\n", 
      as.character(Sys.info()[['sysname']] == 'Windows')))

    temp <- lapply(...)

  } else {
    if (max_core %like% "*") {
      core_count <- as.numeric(gsub("\\*", "", max_core))
    } else {
      core_count <- min(max_core, detectCores())
    }

    if (quiet==FALSE) cat(sprintf("\nwindows: %s / number of cores: %d (parallelized)\n", 
      as.character(Sys.info()[['sysname']] == 'Windows'),core_count))
    
    temp <- mclapply(..., mc.cores=core_count)
  }
  
  ## return
  return(temp)
}

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

#' Generate a varcov matrix for a system of truly unrelated equations - to be used as part of a linearHypothesis test. 
#' @export
#' @import data.table
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

systemfit_var <- function(group_var, df, clust="ind", 
  cluster_var=NULL, systemfit_obj=NULL) {

  coef_count <- length(group_var[[1]])-1

  temp_vcov  <- matrix(rep(0, 
      (length(group_var)*coef_count)^2),
      ncol= (length(group_var)*coef_count))

  for (i in seq(from=0, to=length(group_var)-1)) {

    temp      <- lm(group_var[[i+1]], data=df)
    temp_coef <- setdiff(names(coef(temp)), "(Intercept)")

    if (clust=="ind") {
      temp_cov <- vcov(temp)
    } else if (clust=="clust") {
      temp_cov <- cluster.vcov(temp, df[, get(cluster_var)])
    } else if (clust=="robust") {
      temp_cov <- hccm(temp, "hc0")
    } else if (clust=="manual") {

      res <- systemfit_obj$eq[[i+1]]$residuals
      n   <- length(res)
      X   <- cbind(c(rep(1, nrow(systemfit_obj$eq[[i+1]]$model))), 
        systemfit_obj$eq[[i+1]]$model[, c(temp_coef)])
      k   <- ncol(X)

      temp_cov <- 1/(n-k) * as.numeric(t(res)%*%res) * solve(t(X)%*%X)

    }

    temp_vcov[seq(from=length(coef(temp))*i+1, 
        to=length(coef(temp))*i+(length(coef(temp)))), 
        seq(from=length(coef(temp))*i+1, 
        to=length(coef(temp))*i+
        (length(coef(temp))))]       <- temp_cov
  
  }

  return(temp_vcov)

}


#----------------------------------------------------------------------------#

