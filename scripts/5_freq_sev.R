##############################################
#  Claim frequency and severity modeling
##############################################



## --------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
mtpl <- read.table('../data/PC_data.txt',
                   header = TRUE, stringsAsFactors = TRUE) %>% as_tibble() %>% rename_all(tolower) %>% rename(expo = exp)


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(rpart)
fit <- rpart(formula = cbind(expo,nclaims) ~ ageph + agec + bm + power + coverage + fuel + sex + fleet + use,
             data = mtpl,
             method = 'poisson',
             control = rpart.control(maxdepth = 3,
                                     cp = 0))
print(fit)


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(rpart.plot)
rpart.plot(fit, cex = 1.5)


## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------



mtpl %>% 
  dplyr::filter(... & ...) %>% 
  dplyr::summarise(claim_freq = ... / ...)


## --------------------------------------------------------------------------------------------------------------------------------------------------
k <- 1

alpha <- 1/k^2

mu <- mtpl %>% with(sum(nclaims)/sum(expo))

beta <- alpha/mu

mtpl %>% 
  dplyr::filter(bm < 2, ageph >= 56) %>% 
  dplyr::summarise(prediction = (alpha + sum(nclaims))/(beta + sum(expo))) %>% as.data.frame()


## --------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = cbind(expo,nclaims) ~ ageph + agec + bm + power + coverage + fuel + sex + fleet + use,
             data = mtpl,
             method = 'poisson',
             control = rpart.control(maxdepth = 3,
                                     cp = 0),
             parms = list(shrink = 10^-5))
print(fit)


## --------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = cbind(expo,nclaims) ~ ageph + agec + bm + power + coverage + fuel + sex + fleet + use,
             data = mtpl,
             method = 'poisson',
             control = rpart.control(maxdepth = 3,
                                     cp = 0),
             parms = list(shrink = 10^5))
print(fit)


## --------------------------------------------------------------------------------------------------------------------------------------------------
mtpl %>% 
  dplyr::filter(bm < 2, ageph >= 56) %>% 
  dplyr::summarise(claim_freq = sum(nclaims)/sum(expo)) %>% as.data.frame()


## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------







