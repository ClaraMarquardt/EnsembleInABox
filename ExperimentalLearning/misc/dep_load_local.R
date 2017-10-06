#----------------------------------------------------------------------------#

# Purpose:     Load dependencies locally 
# Author:      Clara Marquardt
# Date:        2017


#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
#                                    CONTROL                                 #
#----------------------------------------------------------------------------#

# packages
#--------------------------------------# 

package_list <- list("data.table", "survey","rlist", "plyr", "zoo","ggplot2",
	"xlsx", "mfx", "testit", "glmnet", "ranger", "rpart", "caret", "xgboost", 
	"systemfit", "parallel", "Hmisc", "multiwayvcov", "plotly", "car", "nnls")

# install/load
#--------------------------------------# 
##  ehR package
library(devtools)
if (!("ehR" %in% installed.packages())) {
	install_github("claramarquardt/ehR", dependencies = TRUE) 
}
library(ehR)

## generic packages
load_or_install(package_names=package_list, custom_lib_path=FALSE)

#----------------------------------------------------------------------------#
#                                       END                                  #
#----------------------------------------------------------------------------#

