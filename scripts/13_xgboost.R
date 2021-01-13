##############################################
#  XGBoost
##############################################



## --------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
mtpl <- read.table('../data/PC_data.txt',
                   header = TRUE, stringsAsFactors = TRUE) %>% as_tibble() %>% rename_all(tolower) %>% rename(expo = exp)


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(xgboost)
mtpl_xgb <- xgb.DMatrix(data = mtpl %>% 
                          select(ageph,power,bm,agec,coverage,fuel,sex,fleet,use) %>%
                          data.matrix,
                        info = list(
                          'label' = mtpl$nclaims,
                          'base_margin' = log(mtpl$expo)))
mtpl_xgb


## --------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(86493) # reproducibility
fit <- xgboost(data = mtpl_xgb,
               nrounds = 200,
               early_stopping_rounds = 20,
               verbose = FALSE,
               params = list(
                 booster = 'gbtree',
                 objective  = 'count:poisson',
                 eval_metric = 'poisson-nloglik',
                 eta = 0.1, nthread = 1,
                 subsample = 0.75, colsample_bynode = 0.5,
                 max_depth = 3, min_child_weight = 1000,
                 gamma = 0, lambda = 1, alpha = 1))


## --------------------------------------------------------------------------------------------------------------------------------------------------
xgb.plot.tree(feature_names = colnames(mtpl_xgb),
              model = fit,
              trees = 0)


## --------------------------------------------------------------------------------------------------------------------------------------------------
xgb.plot.multi.trees(model = fit,
                     feature_names = colnames(mtpl_xgb))


## --------------------------------------------------------------------------------------------------------------------------------------------------
xgb.ggplot.importance(importance_matrix = xgb.importance(feature_names = colnames(mtpl_xgb),
                                                         model = fit))


## --------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(86493) # reproducibility
xval <- xgb.cv(data = mtpl_xgb,
               nrounds = 200,
               early_stopping_rounds = 20,
               verbose = FALSE,
               nfold = 5,
               stratified = TRUE,
               params = list(booster = 'gbtree',
                             objective  = 'count:poisson',
                             eval_metric = 'poisson-nloglik',
                             eta = 0.1, nthread = 1,
                             subsample = 0.75, colsample_bynode = 0.5,
                             max_depth = 3, min_child_weight = 1000,
                             gamma = 0, lambda = 1, alpha = 1))


## --------------------------------------------------------------------------------------------------------------------------------------------------
xval$evaluation_log


## --------------------------------------------------------------------------------------------------------------------------------------------------
xval_log <- xval$evaluation_log
xval_log <- as.data.frame(rbind(as.matrix(xval_log[,c(1,2,3)]),as.matrix(xval_log[,c(1,4,5)])))
names(xval_log) <- c('iteration','poisson_nloglik','std')
xval_log$loss <- c(rep('train',nrow(xval_log)/2),rep('test',nrow(xval_log)/2))


## --------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(xval_log, aes(x=iteration, y=poisson_nloglik, colour=loss, linetype = loss)) + geom_line(size = 1.3)


## --------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(xval_log[c(150:200,350:400),], aes(x=iteration, y=poisson_nloglik, colour=loss, linetype = loss)) + geom_line(size = 1.5)


## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------












