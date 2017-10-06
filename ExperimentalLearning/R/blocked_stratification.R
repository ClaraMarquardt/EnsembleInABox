#----------------------------------------------------------------------------#

#' @title *Split a data.table into stratified cluster-level subsets.
#' 
#' @description *.
#' 
#' @export
#' @import data.table
#' @import testit
#' @param df *Datatable containing the data [data.table].
#' @param stratify_var *Name of variable on which to stratify (when generating CV folds, test vs. holdout splits)  [character].
#' @param cluster_var *Name of variable containing observation level cluster identifiers (integer) [character].
#' @param prop_vector *Number of intervals into which the stratification variable is split (if fold_method==stratification) [integer] .
#' @param stratify_cut *Number of intervals into which the stratification variable is split (if fold_method==stratification) [integer] .
#' @return *Vector of subset IDs (to be merged into original data.table).
#' @examples 

blocked_stratification <- function(df, stratify_var, cluster_var, prop_vector, stratify_cut) {

    # check that sum(prop_vector) == 1
    # ---------------------------------
    assert("'prop_vector', i.e. proportions of folds sum to 1 (blocked_stratification())", 
        sum(prop_vector)==1)
    
    # initialise
    # ---------------------------------
    df_temp <- copy(df)
    df_temp[,id_temp:=1:nrow(df_temp)]
    df_temp[,cluster_id_temp:=.GRP,by=c(cluster_var)]

    min_size <- length(prop_vector)

    # cut the stratify_var into stratify_cut intervals (ensuring that the number of observations 
    # in each interval >= number of subsets to be generated)
    # ---------------------------------
    stratify_cut <- min(stratify_cut, length(unique(df_temp[, get(stratify_var)])))
    if (stratify_cut>1) {
        df_temp[, stratify_var_cut:=cut(get(stratify_var),stratify_cut,labels=F)]
    } else {
        df_temp[, stratify_var_cut:=1]
    }
    df_temp_unique <- unique(df_temp, by=c(cluster_var))
    df_temp_unique[, stratify_var_cut_count:=.N, by=c("stratify_var_cut")]

    while(nrow(df_temp_unique[stratify_var_cut_count < min_size & stratify_var_cut > 0])>0) {
        
        df_temp_unique[stratify_var_cut_count < min_size & stratify_var_cut > 0, 
          stratify_var_cut:=stratify_var_cut-1L]
        
        df_temp_unique[, stratify_var_cut_count:=.N, by=c("stratify_var_cut")]

    }

    # sample from within each interval of stratify_var to generate all but the last subset
    # ---------------------------------
    df_temp_unique[, subset:=0]
    df_temp[, subset:=0]


    invisible(lapply(1:(length(prop_vector)), function(subset_id) {

        if (subset_id %in% seq(1,length(prop_vector),2)) temp_fun <- ceiling else temp_fun <- floor

        obs_count <- lapply(unique(df_temp_unique, by=c("stratify_var_cut"))$stratify_var_cut_count, 
                        function(x) temp_fun(x*prop_vector[subset_id]))

        obs_id_raw    <- mapply(function(cat, count) {
                            sample(df_temp_unique[stratify_var_cut==cat & subset==0]$id_temp, 
                                min(count,nrow(df_temp_unique[stratify_var_cut==cat & subset==0])))},
                            cat=unique(df_temp_unique, by=c("stratify_var_cut"))$stratify_var_cut, 
                            count=obs_count, SIMPLIFY = FALSE)

        df_temp_unique[id_temp %in% unlist(obs_id_raw), subset:=subset_id]

        df_temp[cluster_id_temp %in% df_temp_unique[subset==subset_id]$cluster_id_temp, 
            subset:=subset_id]

    }))

    # generate the last subset
    # ---------------------------------
    df_temp[subset==0, subset:=length(prop_vector)]

    # return a vector of subset IDs
    # ---------------------------------
    return(df_temp$subset)

}

#----------------------------------------------------------------------------#

    
