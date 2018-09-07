# 1_test_mixed_model.R (lyra)
# test whether a mixed model fits the data well (this is without an editor-in-chief effect)
# Sep 2018
library(tidyverse)
library(broom)
library(lme4) # for mixed model

## Journal/Gender data from Holman et al
# get data on journals and gender for individual papers
load('For.Sample.Size.RData') # from 0_extract_data.R
selected = filter(selected, year >= 2002) %>% # just data from 2002, ignore small amount before then
  mutate(datec = (as.numeric(date) - as.numeric(as.Date('2010-06-01')))/(365.25*5), # centre at 2010 (mean & median in sample), scale to per 5 year increase
         datec2 = datec*datec) # 
# run the Poisson regression models (takes a while, used nAGQ=0 to save time)
# three models of increasing complexity
# intercept only
model0 = glmer(females ~ 1 + (1|journal), data=selected, family=poisson, offset=log(n), nAGQ=0)
# intercept plus slope
model1 = glmer(females ~ datec + (1 + datec|journal), data=selected, family=poisson, offset=log(n), nAGQ=0)
# intercept plus slope plus quadratic slope
model2 = glmer(females ~ datec + datec2 + (1 + datec + datec2|journal), data=selected, family=poisson, offset=log(n), nAGQ=0)
# compare AIC
AIC(model0, model1, model2)

# save results for use in protocol
save(model0, model1, model2, file='Initial.mixed.model.results.RData')
