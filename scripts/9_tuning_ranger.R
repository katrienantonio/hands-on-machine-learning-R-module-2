##############################################
#  Tuning ranger
##############################################



## --------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
mtpl <- read.table('../data/PC_data.txt',
                   header = TRUE, stringsAsFactors = TRUE) %>% as_tibble() %>% rename_all(tolower) %>% rename(expo = exp)


## --------------------------------------------------------------------------------------------------------------------------------------------------
search_grid <- expand.grid(num.trees = c(100,200),
                           mtry = c(3,6,9),
                           min.node.size = c(0.001,0.01)*nrow(mtpl),
                           error = NA)
print(search_grid)


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(ranger)
for(i in seq_len(nrow(search_grid))) {
  fit <- ranger(formula = nclaims ~ ageph + agec + bm + power + coverage + fuel + sex + fleet + use, 
                data = mtpl, 
                num.trees = search_grid$num.trees[i],
                mtry = search_grid$mtry[i],
                min.node.size = search_grid$min.node.size[i],
                replace = TRUE,
                sample.fraction = 0.75,
                verbose = FALSE,
                seed = 54321)
  search_grid$error[i] <- fit$prediction.error
}


## --------------------------------------------------------------------------------------------------------------------------------------------------
search_grid %>% dplyr::arrange(error)
