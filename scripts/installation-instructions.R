packages <- c("tidyverse", "here", "rstudioapi", "rpart", "rpart.plot", "vip", "pdp", "ipred", "ranger", "gbm", "xgboost", 
              "rstudioapi", "gganimate", "transformr", "Ckmeans.1d.dp", "classInt", "gifski", "DiagrammeR")

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

if(sum(!(packages %in% installed.packages()[, "Package"]))) {
  stop(paste('The following required packages are not installed:\n', 
             paste(packages[which(!(packages %in% installed.packages()[, "Package"]))], collapse = ', ')));
} else {
  message("Everything is set up correctly. You are ready to go.")
}