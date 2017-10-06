#----------------------------------------------------------------------------#

#' @title *Combine an algorithm with a list of tuning parameters.
#' 
#' @description *.
#' 
#' @export
#' @param algorithm *Name of individual algorithm to be tuned [character].
#' @return *List with the algorithm [algorithm] and the parameters [parameter].
#' @examples

alg <- function(algorithm) {
  list(algorithm=get(paste0(algorithm, "alg")), 
  	parameter_tuning_rand=ExpLearning.MLParameter[[paste0(algorithm, "_param_tuning_rand")]], 
  	parameter_tuning_grid=ExpLearning.MLParameter[[paste0(algorithm, "_param_tuning_grid")]], 
  	parameter_tuning=ExpLearning.MLParameter[[paste0(algorithm, "_param_tuning")]], 
  	parameter=ExpLearning.MLParameter[[paste0(algorithm, "_param")]], 
  	parameter_raw=ExpLearning.MLParameter[[paste0(algorithm, "_param_raw")]],
  	parameter_add=ExpLearning.MLParameter[[paste0(algorithm, "_param_tuning_add")]])
}

#----------------------------------------------------------------------------#
