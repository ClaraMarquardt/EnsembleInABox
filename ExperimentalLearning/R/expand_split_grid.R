#----------------------------------------------------------------------------#

#' @title *Convert a list of tuning parameters into a list of data.tables each containing one tuning parameter combination.
#' 
#' @description *.
#' 
#' @export
#' @param alg_param_tuning *Tuning parameters - tuning [list - misc].
#' @param alg_param_default *Tuning parameters - default [list - misc].
#' @param param_split_method *Method used to split and combine a parameter list into a (tuning) parameter grid [character].
#' @return *Formatted parameter tuning grid.
#' @examples

expand_split_grid <- function(alg_param_tuning, alg_param_default=NULL, iter=NULL, param_split_method) {


    # grid/joint
	if (param_split_method=="grid") {

		if (!is.null(alg_param_tuning)) {

			if (!is.null(iter)) {

				alg_param_tuning <- lapply(alg_param_tuning, function(x) {

					if (length(x) >1) {
						x <- x[c(1, sample(2:length(x), min(iter-1, length(x)-1)))]
					} 

					return(x)
				})

			}

			temp <- expand.grid(alg_param_tuning)
			temp <- split(temp, seq(nrow(temp)))

		} else {

			temp <- NULL

		}

    # indi
	} else if (param_split_method=="non_grid") {

		if (!is.null(alg_param_tuning)) {

			temp <- lapply(names(alg_param_tuning), function(x) {

				temp_list  <- list()

				temp_list_tuning  <- list(alg_param_tuning[[x]])
				temp_list_default <- alg_param_default[!names(alg_param_default)==x]

				temp_list <- c(temp_list_tuning, temp_list_default)
				names(temp_list) <- c(x, names(temp_list_default))
				temp_list <- expand.grid(temp_list)
			    temp_list <- split(temp_list, seq(nrow(temp_list)))

				return(temp_list)

			})

		} else {

			temp <- NULL

		}

	}

	# return
	return(temp)

}

#----------------------------------------------------------------------------#
