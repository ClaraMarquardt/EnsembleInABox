#----------------------------------------------------------------------------#

#' @title *Convert tuning parameters returned as part of the results into a list of parameters which can be passed to the next round as a custom_param argument.
#' 
#' @description *.
#' 
#' @export
#' @import data.table
#' @param model_result *List of results returned by the effect_on_group function [list-misc].
#' @return *Formatted list of tuning parameters.
#' @examples 


extract_param <- function (model_result) {

    param <- lapply(model_result, function(x) x[grep("outer_param",
        names(x), value = T)])
    for (model_id in 1:length(param)) {
        names(param[model_id]) <- paste0(gsub("outer_param\\.",
            "", names(param[model_id])), "_param")
    }

    param <- lapply(param, function(x) lapply(x, function(y) lapply(y,
        function(z) z)))

    return(param)
}

#----------------------------------------------------------------------------#

