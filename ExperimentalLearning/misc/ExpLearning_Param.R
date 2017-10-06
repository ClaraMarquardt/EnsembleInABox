#----------------------------------------------------------------------------#

# Purpose:     Update the default parameters/parameter definitions associated with 
#              the ExperimentalLearning Package 
# Author:      Clara Marquardt
# Date:        2017


#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
#                                    CONTROL                                 #
#----------------------------------------------------------------------------#

# parameters
wd_path         <- commandArgs(trailingOnly = TRUE)[1]
package_name    <- commandArgs(trailingOnly = TRUE)[2]

print(sprintf("wd_path: %s",  wd_path))
print(sprintf("package_name: %s",  package_name))

# dependencies
library("devtools")
library(roxygen2)
library(data.table)
library(ExperimentalLearning)

# paths
setwd(paste0(wd_path, "/", package_name))
param_file <- paste0(wd_path, package_name, "/doc/parameter_def_reference.csv")

#----------------------------------------------------------------------------#
#                                      CODE                                  #
#----------------------------------------------------------------------------#

# read in values & subset to ml parameters (i.e. parameters with default values)
#----------------------------
param_overview <- fread(param_file)
param_overview_sub <- param_overview[!(default_value=="")]

# initialise
#----------------------------

## default values
ExpLearning.Default        <- as.list(param_overview_sub$default_value)
names(ExpLearning.Default) <- param_overview_sub$parameter_name

### format default values
suppressWarnings(ExpLearning.Default <- lapply(ExpLearning.Default,  function(x) 
	ifelse(!is.na(as.numeric(x)), as.numeric(x), x)))
ExpLearning.Default <- lapply(ExpLearning.Default,  function(x) 
	ifelse(x %like% "^\\*\\*",eval(parse(text= paste0("ExpLearning.Default$", 
	gsub("\"\"", "\"", gsub("^\\*\\*", "", x))))) , x))
ExpLearning.Default <- lapply(ExpLearning.Default,  function(x) 
 	ifelse(x %like% "^\\*",gsub("\"\"", "\"", gsub("^\\*", "", x)) , x))
ExpLearning.Default <- lapply(ExpLearning.Default,  function(x) 
	ifelse(x %in% c("TRUE", "FALSE"),as.logical(x) , x))
ExpLearning.Default <- lapply(ExpLearning.Default,  function(x) 
	if(x %in% c("NULL")) {NULL} else {x})
 
## overview table
ExpLearning.Parameter   <-  lapply(1:nrow(param_overview_sub), 
								function(x) param_overview_sub[x])
ExpLearning.Parameter <- lapply(ExpLearning.Parameter, function(x) lapply(1:ncol(x), function(y) 
	as.character(x[, c(y), with=F])))
ExpLearning.Parameter <- lapply(1:length(ExpLearning.Parameter), function(x) 
	list(parameter_description=ExpLearning.Parameter[[x]][4][[1]],
		default_value=ExpLearning.Default[x][[1]], 
		possible_value=ExpLearning.Parameter[[x]][3][[1]], 
		global_access=as.integer(ExpLearning.Parameter[[x]][5][[1]])))
names(ExpLearning.Parameter) <- param_overview_sub$parameter_name
	

# save to use with data
#----------------------------
devtools::use_data(ExpLearning.Default, overwrite = TRUE)
devtools::use_data(ExpLearning.Parameter, overwrite = TRUE)

#----------------------------------------------------------------------------#
#                                       END                                  #
#----------------------------------------------------------------------------#

