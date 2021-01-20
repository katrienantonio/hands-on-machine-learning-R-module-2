##############################################
#  Bagging introduction
##############################################



## --------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
set.seed(54321) # reproducibility
dfr <- tibble::tibble(
  x = seq(0, 2 * pi, length.out = 500),
  m = 2 * sin(x),
  y = m + rnorm(length(x), sd = 1)) 


## --------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(45678) # reproducibility

bsample_1 <- dfr %>% nrow %>% sample(replace = TRUE)
bsample_2 <- dfr %>% nrow %>% sample(replace = TRUE)

dfr_b1 <- dfr %>% dplyr::slice(bsample_1)
dfr_b2 <- dfr %>% dplyr::slice(bsample_2)


## --------------------------------------------------------------------------------------------------------------------------------------------------
dfr_b1 %>% dplyr::arrange(x) %>% head()
dfr_b2 %>% dplyr::arrange(x) %>% head()


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(rpart)
fit_b1 <- rpart(formula = y ~ x,
                data = dfr_b1,
                method = 'anova',
                control = rpart.control(maxdepth = 30,
                                        minsplit = 20,
                                        minbucket = 3,
                                        cp = 0.01))


## --------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_reg <- function(dt, preds){
  dt %>% mutate(pred = preds) %>% ggplot(aes(x = x)) +
    geom_point(aes(y = y), alpha = 0.3) +
    geom_line(aes(y = m), colour = 'darkgreen', size = 1.5) +
    geom_line(aes(y = pred), colour = 'darkred', size = 1.5) + theme_bw()
}

plot_pred_reg(dt = dfr, preds = predict(fit_b1, dfr))


## --------------------------------------------------------------------------------------------------------------------------------------------------
fit_b2 <- rpart(formula = y ~ x,
                data = dfr_b2,
                method = 'anova',
                control = rpart.control(maxdepth = 30,
                                        minsplit = 20,
                                        minbucket = 3,
                                        cp = 0.01))


## --------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_reg(dt = dfr, preds = predict(fit_b2, dfr))


## --------------------------------------------------------------------------------------------------------------------------------------------------
pred_b1 <- fit_b1 %>% predict(dfr)
pred_b2 <- fit_b2 %>% predict(dfr)

pred <- rowMeans(cbind(pred_b1,
                       pred_b2))


## --------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_reg(dt = dfr, preds = pred)


## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------







## --------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(12345)
N <- 100000 ; x <- 1:N
mean(x %in% sample(N, replace = TRUE))


## --------------------------------------------------------------------------------------------------------------------------------------------------
mean(x %in% sample(N, size = 0.75*N, replace = TRUE))


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(ipred)
set.seed(83946) # reproducibility
fit <- ipred::bagging(formula = y ~ x,
                      data = dfr,
                      nbagg = 200,
                      ns = nrow(dfr),
                      coob = TRUE,
                      control = rpart.control(maxdepth = 30,
                                              minsplit = 20,
                                              minbucket = 3,
                                              cp = 0.01))


## --------------------------------------------------------------------------------------------------------------------------------------------------
pred <- predict(fit, dfr)


## --------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_reg(dt = dfr, preds = predict(fit, dfr))


## --------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(98765) # reproducibility
nbags <- 10*(1:20)
oob <- rep(0, length(nbags))
for(i in 1:length(nbags)){
  fit <- ipred::bagging(formula = y ~ x,
                        data = dfr,
                        nbagg = nbags[i],
                        ns = nrow(dfr),
                        coob = TRUE,
                        control = rpart.control(maxdepth = 30,
                                                minsplit = 20,
                                                minbucket = 3,
                                                cp = 0.01))
  oob[i] <- fit$err
}


## --------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data.frame('B' = nbags, 'RMSE' = oob), aes(x = B, y = RMSE)) + geom_line()


## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------




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


plot_pred_class <- function(dt, preds){
  dt %>% mutate(pred = preds) %>% ggplot(aes(x = x1, y = x2)) +
    geom_point(aes(color = pred))
}

# See ?ipred::predict.classbagg for prediction arguments
preds <- predict(fit, dfc, type = 'class', aggregation = 'majority')


