##############################################
#  Tuning gbm
##############################################



## --------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
set.seed(54321) # reproducibility
dfc <- tibble::tibble(
  x1 = rep(seq(0.1,10,by = 0.1), times = 100),
  x2 = rep(seq(0.1,10,by = 0.1), each = 100),
  y = as.factor(
    pmin(1,
         pmax(0,
              round(
                1*(x1+2*x2<8) + 1*(3*x1+x2>30) + 
                  rnorm(10000,sd = 0.5))
         )
    )
  )
)


## --------------------------------------------------------------------------------------------------------------------------------------------------
dfc <- dfc %>% dplyr::mutate(y_recode = as.integer(as.character(y)))


## --------------------------------------------------------------------------------------------------------------------------------------------------
ctrl_grid <- expand.grid(depth = c(1,3,5),
                         shrinkage = c(0.01,0.1,1))
results <- vector('list', length = nrow(ctrl_grid))


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(gbm)
for(i in seq_len(nrow(ctrl_grid))) {
  fit <- gbm(y_recode ~ x1 + x2,
             data = dfc,
             distribution = 'bernoulli',
             n.trees = 100,
             interaction.depth = ctrl_grid$depth[i],
             shrinkage = ctrl_grid$shrinkage[i])
  
  results[[i]] <- dfc %>% mutate(
    depth = factor(paste('depth =',ctrl_grid$depth[i]), ordered =TRUE),
    shrinkage = factor(paste('shrinkage =',ctrl_grid$shrinkage[i]), ordered = TRUE),
    pred_prob = predict(fit, n.trees = fit$n.trees, type = 'response'), #<<
    pred_clas = factor(1*(predict(fit, n.trees = fit$n.trees, type = 'response') >= 0.5))) #<<
}


## --------------------------------------------------------------------------------------------------------------------------------------------------
results <- do.call(rbind, results)


## --------------------------------------------------------------------------------------------------------------------------------------------------
results %>% ggplot(aes(x = x1, y = x2)) +
  geom_point(aes(color = pred_prob)) +
  facet_grid(depth ~ shrinkage)


## --------------------------------------------------------------------------------------------------------------------------------------------------
results %>% ggplot(aes(x = x1, y = x2)) +
  geom_point(aes(color = pred_clas)) +
  facet_grid(depth ~ shrinkage)


## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------


my_grid <- expand.grid(...)
my_grid <- my_grid %>% dplyr::mutate(oob_improv = NA)

for(i in seq_len(nrow(my_grid))) {
  fit <- gbm(y_recode ~ x1 + x2,
             data = dfc,
             distribution = 'bernoulli',
             n.trees = ...,
             interaction.depth = ...,
             shrinkage = ...,
             ...)
  my_grid$oob_improv[i] <- sum(fit$oobag.improve)
}






