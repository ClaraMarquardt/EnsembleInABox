#----------------------------------------------------------------------------#

# Purpose:     Update the the basic test dataframe stored in the ExperimentalLearning 
#			   package  (test_df)
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

# set.seed
set.seed(1)

# dependencies
library("devtools")
library(roxygen2)
library(data.table)

# paths
setwd(paste0(wd_path, "/", package_name))

# parameters
obs_count       <- 10000
cluster_count   <- ceiling(obs_count/100)
treatment_ratio <- 0.5

control_count_cat  <- 4
control_count_num  <- 4

control_ratio_min <- 0.1 # 10% 1, 90% 0 [% 1]
control_ratio_max <- 0.9 # 90% 1, 10% 0 [% 1]

control_mean_min <- 2
control_mean_max <- 8
control_sd_min   <- 1
control_sd_max   <- 2

#----------------------------------------------------------------------------#
#                                      CODE                                  #
#----------------------------------------------------------------------------#

# initialise
test_df <- data.table(id=1:obs_count)

# cluster
cluster <- unlist(sapply(1:cluster_count, function(x) rep(x, sample(c(rep(1,10), 100, 200, 300, 500, 
            600, 700, 800, 1000,1500, 2000),1))))[1:obs_count] 
cluster[is.na(cluster)] <- (cluster_count+1)
test_df[, ':='(cluster=cluster)]
print(prop.table(table(test_df$cluster)))

# treatment (outcome variable)
treatment_raw <- sample(c(0,1), prob=c(1-treatment_ratio, treatment_ratio), 
    size=cluster_count, replace=TRUE)
test_df[ ,treatment:=treatment_raw[cluster]]  
print(prop.table(table(test_df$treatment)))

# (sample) predicted probabilities
test_df[ , prediction:=round(runif(obs_count, min=0.001, max=0.999), 3)]  
print(summary(test_df$prediction))

# weights
test_df[, weight:=sample(c(0.8,1.2), nrow(test_df), replace = TRUE)]

# control variables (ind. of treatment)

## categorical
for (i in paste0("control_cat_", 1:control_count_cat)) {

    temp_ratio <- round(runif(n=1, min=control_ratio_min, 
                  max=control_ratio_max), 2)
    temp <- sample(c(0,1), prob=c(1-temp_ratio, temp_ratio), 
           obs_count, replace=TRUE)

    assign(i, temp)

}

## numerical
for (i in paste0("control_num_", 1:control_count_num)) {

    temp_mean <- runif(n=1, min=control_mean_min , max=control_mean_max)
    temp_sd   <- runif(n=1, min=control_sd_min , max=control_sd_max)
  
    temp <- rnorm(mean=temp_mean, sd=temp_sd, n=obs_count)

    assign(i, temp)

}

test_df[, c(paste0("control_cat_", 1:control_count_cat)):=
    lapply(c(paste0("control_cat_", 1:control_count_cat)), function(x) get(x))]
test_df[, c(paste0("control_num_", 1:control_count_num)):=
    lapply(c(paste0("control_num_", 1:control_count_num)), function(x) get(x))]


# save (include in package)
#----------------------------
devtools::use_data(test_df, overwrite = TRUE)


#----------------------------------------------------------------------------#
#                                       END                                  #
#----------------------------------------------------------------------------#



