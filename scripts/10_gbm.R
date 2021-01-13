##############################################
#  (Stochastic) Gradient Boosting Machines
##############################################



## --------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
set.seed(54321) # reproducibility
dfr <- tibble::tibble(
  x = seq(0, 2 * pi, length.out = 500),
  m = 2 * sin(x),
  y = m + rnorm(length(x), sd = 1))


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(gbm)
fit <- gbm(formula = y ~ x,
           data = dfr,
           distribution = 'gaussian',
           n.trees = 10,
           interaction.depth = 1,
           shrinkage = 1) 


## --------------------------------------------------------------------------------------------------------------------------------------------------
pred <- predict(fit,
                n.trees = fit$n.trees,
                type = 'response')


## --------------------------------------------------------------------------------------------------------------------------------------------------
preds <- do.call(rbind, lapply(0:fit$n.trees,
                               function(i) dfr %>% mutate(iter = i,
                                                          pred = predict(fit, n.trees = i, type = 'response'))))

library(gganimate)
library(transformr)
preds %>% ggplot(aes(x = x)) +
  geom_point(aes(y = y), alpha = 0.3) +
  geom_line(aes(y = m), colour = 'darkgreen', size = 1.5) +
  geom_line(aes(y = pred), colour = 'darkred', size = 1.5) + 
  transition_states(iter, transition_length = 0.1, state_length = 0.5) + labs(title = "Iteration: {closest_state}") + theme_bw()


## --------------------------------------------------------------------------------------------------------------------------------------------------
fit <- gbm(formula = y ~ x,
           data = dfr,
           distribution = 'gaussian',
           n.trees = 10,
           interaction.depth = 1,
           shrinkage = 0.1) 


## --------------------------------------------------------------------------------------------------------------------------------------------------
preds <- do.call(rbind, lapply(0:fit$n.trees,
                               function(i) dfr %>% mutate(iter = i,
                                                          pred = predict(fit, n.trees = i, type = 'response'))))

preds %>% ggplot(aes(x = x)) +
  geom_point(aes(y = y), alpha = 0.3) +
  geom_line(aes(y = m), colour = 'darkgreen', size = 1.5) +
  geom_line(aes(y = pred), colour = 'darkred', size = 1.5) + 
  transition_states(iter, transition_length = 0.1, state_length = 0.5) + labs(title = "Iteration: {closest_state}") + theme_bw()


## --------------------------------------------------------------------------------------------------------------------------------------------------
fit <- gbm(formula = y ~ x,
           data = dfr,
           distribution = 'gaussian',
           n.trees = 10,
           interaction.depth = 3,
           shrinkage = 0.1) 


## --------------------------------------------------------------------------------------------------------------------------------------------------
preds <- do.call(rbind, lapply(0:fit$n.trees,
                               function(i) dfr %>% mutate(iter = i,
                                                          pred = predict(fit, n.trees = i, type = 'response'))))

preds %>% ggplot(aes(x = x)) +
  geom_point(aes(y = y), alpha = 0.3) +
  geom_line(aes(y = m), colour = 'darkgreen', size = 1.5) +
  geom_line(aes(y = pred), colour = 'darkred', size = 1.5) + 
  transition_states(iter, transition_length = 0.1, state_length = 0.5) + labs(title = "Iteration: {closest_state}") + theme_bw()



## --------------------------------------------------------------------------------------------------------------------------------------------------
fit <- gbm(formula = y ~ x,
           data = dfr,
           distribution = 'gaussian',
           n.trees = 10,
           interaction.depth = 3,
           shrinkage = 1) 


## --------------------------------------------------------------------------------------------------------------------------------------------------
preds <- do.call(rbind, lapply(0:fit$n.trees,
                               function(i) dfr %>% mutate(iter = i,
                                                          pred = predict(fit, n.trees = i, type = 'response'))))

preds %>% ggplot(aes(x = x)) +
  geom_point(aes(y = y), alpha = 0.3) +
  geom_line(aes(y = m), colour = 'darkgreen', size = 1.5) +
  geom_line(aes(y = pred), colour = 'darkred', size = 1.5) + 
  transition_states(iter, transition_length = 0.1, state_length = 0.5) + labs(title = "Iteration: {closest_state}") + theme_bw()


## --------------------------------------------------------------------------------------------------------------------------------------------------
fit <- gbm(formula = y ~ x,
           data = dfr,
           distribution = 'gaussian',
           n.trees = 300,
           interaction.depth = 3,
           shrinkage = 0.01) 


## --------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_reg <- function(dt, preds){
  dt %>% mutate(pred = preds) %>% ggplot(aes(x = x)) +
    geom_point(aes(y = y), alpha = 0.3) +
    geom_line(aes(y = m), colour = 'darkgreen', size = 1.5) +
    geom_line(aes(y = pred), colour = 'darkred', size = 1.5) + theme_bw()
}

plot_pred_reg(dt = dfr, preds = predict(fit, n.trees = fit$n.trees, type = 'response'))


## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------









