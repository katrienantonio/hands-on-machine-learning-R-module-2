##############################################
#  Interpretation tools
##############################################



## --------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
mtpl <- read.table('../data/PC_data.txt',
                   header = TRUE, stringsAsFactors = TRUE) %>% as_tibble() %>% rename_all(tolower) %>% rename(expo = exp)


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(rpart)
set.seed(9753) # reproducibilty
fit <- rpart(formula = cbind(expo,nclaims) ~ ageph + agec + bm + power + coverage + fuel + sex + fleet + use,
             data = mtpl,
             method = 'poisson',
             control = rpart.control(maxdepth = 20,
                                     minsplit = 2000,
                                     minbucket = 1000,
                                     cp = 0,
                                     xval = 5))
cpt <- fit$cptable
fit_srt <- prune(fit,
                 cp = cpt[which.min(cpt[,'xerror']), 'CP'])


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(vip)
vip::vi(fit_srt)


## --------------------------------------------------------------------------------------------------------------------------------------------------
vip::vip(fit_srt, scale = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(pdp)
# Need to define this helper function for Poisson
pred.fun <- function(object,newdata){
  mean(predict(object, newdata))
} 


## --------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(48927)
pdp_ids <- mtpl %>% nrow %>% sample(size = 5000)


## --------------------------------------------------------------------------------------------------------------------------------------------------
fit_srt %>% 
  pdp::partial(pred.var = 'ageph',
               pred.fun = pred.fun,
               train = mtpl[pdp_ids,]) %>% 
  autoplot()


## --------------------------------------------------------------------------------------------------------------------------------------------------
fit_srt %>% 
  pdp::partial(pred.var = c('ageph','power'),
               pred.fun = pred.fun,
               train = mtpl[pdp_ids,]) %>% 
  autoplot()


## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------





