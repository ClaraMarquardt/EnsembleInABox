#----------------------------------------------------------------------------#

#' @title *Convert a data.table into a data.table containing only complete observations (and potentially featuring missigness indicators).
#' 
#' @description *.
#' 
#' @export
#' @import data.table
#' @import testit
#' @param df *Datatable containing the data [data.table].
#' @param missing_mode *How to handle missingness - (a) omit all missing observations (missing_omit), (b) contruct missingness indicators (and median impute the missing values) treating the missingness indicators as balanced covariates (missing_indic_balanced) or (c) contruct missingness indicators (and median impute the missing values) treating the missingness indicators as unbalanced covariates (missing_indic_unbalanced) [character].
#' @param missing_var *Names of all RHS variables that need to exist for every observation in the datatable used for model construction [character].
#' @param assignment_var *Name of outcome variable [character].
#' @param non_missing_var *Names of all RHS variables that do not need need to exist for every observation in the datatable used for model construction [character].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @return *Complete data.table (df) alongside a list of variables affected by missingness (miss_var).
#' @examples

complete_data <- function(df, missing_var, non_missing_var, assignment_var,
  missing_mode=ExpLearning.Default$missing_mode, 
  holdout_var=ExpLearning.Default$holdout_var, 
  quiet=ExpLearning.Default$quiet) {
    
    # check that all non_missing_var are non_missing
    assert("non-treatment non_missing_var are incomplete (complete_data())", 
      sum(sapply(df[, mget(non_missing_var)], function(x) sum(is.na(x))))==0)

    # check if labels are complete - otherwise omit
    for (i in c(unlist(assignment_var))) {

      if (sum(is.na(df[, get(i)]))>0) {

        cat(sprintf("## Warning: Omitting %d observations - missing labels (assignment_var: %s) \n\n", 
          sum(is.na(df[, get(i)])), i))

        df <- df[!is.na(get(i))]
    
      }

    }

    # initialise
    miss_var <- c()

    # imputation/missingness indicators
    if(missing_mode %in% c("missing_indic_balanced", "missing_indic_unbalanced")) {

      var_list <- c(missing_var)

      for (var in var_list) {

        if (sum(is.na(df[, get(var)]))>0)  {

          # append to list
          miss_var <- c(miss_var, var)
          
          # missingness indicator
          df[, c(paste0(var, "_MISS")):=0]
          df[is.na(get(var)), c(paste0(var, "_MISS")):=1]

          # impute (median) (using only the training observations)
          df[is.na(get(var)),c(var):=median(df[get(holdout_var)==FALSE & 
            !is.na(get(var)), get(var)], na.rm=T)]

        }

      } 
    
    # no imputation/missingness indicators
    } else if (missing_mode %in% c("missing_omit")) {

      ps("omitting %d observations", nrow(df)-nrow(df[complete.cases(df[, mget(missing_var)])]))

      df <- df[complete.cases(df[, mget(missing_var)])]

    }


    # check that df complete
    assert("df contains incomplete observations post formatting (complete_data())", 
      sum(sapply(df[, mget(c(missing_var, non_missing_var))], 
      function(x) sum(is.na(x))))==0)


    return(list(df=df, miss_var=miss_var))
  
}

#----------------------------------------------------------------------------#
