##############################################
#  From bagging to Random forest
##############################################



## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------


library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
mtpl <- read.table('../data/PC_data.txt',
                   header = TRUE, stringsAsFactors = TRUE) %>% as_tibble() %>% rename_all(tolower) %>% rename(expo = exp)

set.seed(486291) # reproducibility

bsample_1 <- mtpl %>% nrow %>% sample(replace = TRUE)
bsample_2 <- ...

mtpl_b1 <- mtpl %>% dplyr::slice(...)
mtpl_b2 <- ...

library(rpart)
fit_b1 <- rpart(formula = cbind(expo,nclaims) ~ ageph + agec + bm + power + coverage + fuel + sex + fleet + use,
                data = ...,
                method = ...,
                control = rpart.control(
                  maxdepth = 3,
                  minsplit = 2000,
                  minbucket = 1000,
                  cp = ...))
fit_b2 <- ...

library(rpart.plot)
rpart.plot(..., cex = 1.4, extra = 0)
...