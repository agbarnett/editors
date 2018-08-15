# 1_sample_size.R (lyra)
# estimate the number of journals needed in order to detect a change in probability of female authors
# July 2018
library(tidyverse)
library(broom)
library(lme4) # for mixed model

## Journal/Gender data from Holman et al
# get data on journals and gender for individual papers
load('For.Sample.Size.RData') # from 0_extract_data.R
selected = filter(selected, year >= 2002) # just data from 2002, ignore small amount before then

#### key simulation parameters ###
# what sample size of journals:
n.journals = c(100)
# expected change (increase in probability of female authors for change in editor-in-chief):
expected.change = c(0, 0.01, 0.02)
# proportion of editors-in-chief changing per year:
prop.change.year = c(0.15)
# number of simulations per combination:
n.sim = 100


#### Main simulation loop ####
to.select = unique(selected$journal) # vector of possible journals
sim.res = NULL
for (pr in prop.change.year){ # loop through editor-in-chief change
  for (jo in n.journals){ # loop through number of journals
    for (ec in expected.change){ # loop through expected change
      for (s in 1:n.sim){
        # randomly select journals 
        jselected = sample(to.select, replace=F, size=jo) 
        journals = filter(selected, journal %in% jselected) %>% 
          group_by(journal) %>% 
          summarise(miny = min(year), maxy = max(year))
        # make data frame that covers years; could not work out how to do this in tidy!
        jframe = NULL 
        for (k in 1:nrow(journals)){
          tframe = data.frame(journal = journals$journal[k], year = seq(journals$miny[k], journals$maxy[k]))
          jframe = rbind(jframe, tframe)
        }
        # ... now add random change in editor-in-chief
        # NOTE: this code allows a change in the journals first available year. This is wrong, but is hopefully unimportant
        jframe = jframe %>% 
          group_by(journal) %>% 
          mutate(change = rbinom(n=n(), size=1, prob=pr),
                 month = sample(1:12, size=n(), replace=T), # random month of change (even where there's no change) ...
                 change.date = as.Date(paste(year, '-', month, '-01', sep='')),  # ... make into date, assuming 1st of month (not used)
            changes = cumsum(change)) %>% # cumulative sum of changes within journal
            select(-change, -month) 
        jframe$journal = as.character(jframe$journal)
        # merge new change data with journal data
        for.model = inner_join(selected, jframe, by=c('journal','year'))
        # centre and scale date, also make squared version
        for.model = for.model %>% mutate(
                  datec = (as.numeric(date) - as.numeric(as.Date('2010-06-01')))/(365.25*5), # centre at 2010 (mean & median in sample), scale to per 5 year increase
                  datec2 = datec*datec) # 
        # increase female author numbers after editor change; assume linear increase with each editor change
        if(ec > 0){
          index = for.model$changes > 0 # only where change
          for.model$females[index] = for.model$females[index] + rbinom(n=sum(index), size=1, prob=ec*for.model$changes[index]) # increase women
          index = for.model$females > for.model$n # increase denominator where all authors were female
          for.model$n[index] = for.model$n[index] + 1
        }
        ## tried two alternative regression models
        amodel = 'poisson'
        if(amodel=='nonlinear'){
        # non-linear model - convergence is poor
        for.model$p = for.model$females / for.model$n
        nmodel = nlmer(p ~ SSmicmen(datec, Vm, K) ~ changes + (Vm + K |journal) , 
                       start = c(Vm=0.5, K=0.5), data=for.model, nAGQ=0)
        res = tidy(nmodel) %>% 
          filter(group == 'fixed') %>%
          select(-group) %>%
          mutate(sim = s, prop.change.year=pr, expected.change=ec, n.journals=jo)
        }
        if(amodel=='poisson'){
          # run the Poisson regression model with a random intercept and non-linear time-slope per journal (takes a while, used nAGQ=0 to save time)
          model = glmer(females ~ datec + datec2 + changes + (1 + datec + datec2|journal), data=for.model, family=poisson, offset=log(n), nAGQ=0)
          # extract and store the fixed effects
          res = tidy(model) %>% 
            filter(group == 'fixed') %>%
            select(-group) %>%
            mutate(sim = s, prop.change.year=pr, expected.change=ec, n.journals=jo)
        }
        sim.res = rbind(sim.res, res)
        remove(model) # tidy up
        if(s%%20 == 0){cat("Up to simulation", s,' for expected change ', ec, ', journal numbers ', jo, ' and editor-change ', pr, '\n', sep='')}
      } # end of simulation loop
      # ongoing save of results
      ifile = paste('SampleSize.', ec, '.', jo, '.', pr,'.RData', sep='')
      save(sim.res, file=ifile)
      sim.res = NULL # start afresh
    } # end of expected change loop
  } # end of journal loop
} # end of proportion change loop

# aside: check fitted values from one model
if(mcheck == T){
  for.model$fit = fitted(model)
  to.plot = dplyr::sample_n(for.model, size=200)
  test.plot = ggplot(data=to.plot, aes(x=year, y=fitted, col=factor(journal)))+
    geom_line()+
    theme(legend.position = 'none')
  test.plot
}
