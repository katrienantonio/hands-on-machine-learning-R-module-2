##############################################
#  Claim frequency and severity modeling with gbm
##############################################



## --------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
mtpl <- read.table('../data/PC_data.txt',
                   header = TRUE, stringsAsFactors = TRUE) %>% as_tibble() %>% rename_all(tolower) %>% rename(expo = exp)


## --------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(76539) # reproducibility
library(gbm)
fit <- gbm(formula = nclaims ~ ageph + agec + bm + power + coverage + fuel + sex + fleet + use + offset(log(expo)),
           data = mtpl,
           distribution = 'poisson',
           var.monotone = c(0,0,1,0,0,0,0,0,0),
           n.trees = 200,
           interaction.depth = 3,
           n.minobsinnode = 1000,
           shrinkage = 0.1,
           bag.fraction = 0.75,
           cv.folds = 0)


## --------------------------------------------------------------------------------------------------------------------------------------------------
fit %>% pretty.gbm.tree(i.tree = 1)


## --------------------------------------------------------------------------------------------------------------------------------------------------
summary(fit, plotit = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(pdp)
pred.fun <- function(object,newdata){
  mean(predict(object, newdata,
               n.trees = object$n.trees,
               type = 'response'))
} 


## --------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(48927)
pdp_ids <- mtpl %>% nrow %>% sample(size = 5000)
fit %>% 
  partial(pred.var = 'bm',
          pred.fun = pred.fun,
          train = mtpl[pdp_ids,],
          recursive= FALSE) %>% 
  autoplot()






