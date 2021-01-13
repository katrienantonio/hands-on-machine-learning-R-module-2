##############################################
#  Pruning via cross-validation
##############################################



## --------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
set.seed(54321) # reproducibility
dfr <- tibble::tibble(
  x = seq(0, 2 * pi, length.out = 500),
  m = 2 * sin(x),
  y = m + rnorm(length(x), sd = 1))

plot_pred_reg <- function(dt, preds){
  dt %>% mutate(pred = preds) %>% ggplot(aes(x = x)) +
    geom_point(aes(y = y), alpha = 0.3) +
    geom_line(aes(y = m), colour = 'darkgreen', size = 1.5) +
    geom_line(aes(y = pred), colour = 'darkred', size = 1.5) + theme_bw()
}


## --------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(87654) # reproducibility
library(rpart)
fit <- rpart(formula = y ~ x,
             data = dfr,
             method = 'anova',
             control = rpart.control(maxdepth = 20,
                                     minsplit = 10,
                                     minbucket = 10,
                                     cp = 0,
                                     xval = 5))


## --------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_reg(dt = dfr, predict(fit, dfr))


## --------------------------------------------------------------------------------------------------------------------------------------------------
plotcp(fit)


## --------------------------------------------------------------------------------------------------------------------------------------------------
cpt <- fit$cptable
cpt


## --------------------------------------------------------------------------------------------------------------------------------------------------
min_xerr <- which.min(cpt[,'xerror'])
fit_1 <- prune(fit, cp = cpt[min_xerr, 'CP'])
plot_pred_reg(dt = dfr, preds = predict(fit_1, dfr))


## --------------------------------------------------------------------------------------------------------------------------------------------------
se_rule <- min(which(cpt[, 'xerror'] < (cpt[min_xerr, 'xerror'] + cpt[min_xerr, 'xstd'])))
fit_2 <- prune(fit, cp = cpt[se_rule, 'CP'])
plot_pred_reg(dt = dfr, preds = predict(fit_2, dfr))


## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------

set.seed(83625493)
dfr2 <- tibble::tibble(
  x = seq(0, 2 * pi, length.out = 500),
  m = 2 * sin(x),
  y = m + rnorm(length(x), sd = 1))




