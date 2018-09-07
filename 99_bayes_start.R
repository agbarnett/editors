# Bayes model for author data
# Sep 2018
library(RColorBrewer)
library(R2WinBUGS)
library(dplyr)
library(ggplot2)

# get the data
load('For.Sample.Size.RData') # from 0_extract_data.R
to.select = unique(selected$journal) # vector of possible journals

## make a smaller data set
date.ref = as.Date('2000-01-01') # date reference same as Holman
ystan = 1 # number of years to standardise by 
# randomly select journals 
n.journals = 20
jselected = sample(to.select, replace=F, size=n.journals) 
# selection to match web site https://lukeholman.github.io/genderGap/
#jselected = c('Am Nat','An Acad Bras Cienc','BMC Res Notes','Chaos','J R Soc Interface','J Vis Exp',
#              'Nanoscale Res Lett','Nat Commun','Nature','PLOS ONE','Prev Sci',
#              'Naturwissenschaften','Proc Natl Acad Sci USA',
#              'Science Am','Science','ScientificWorldJournal','Int J Nurs Stud',
#              'Nurs Res','Am J Nurs','Clin Nurs Res')
for.model = dplyr::filter(selected, journal %in% jselected) %>% 
  filter(year >= 2000) %>%
  mutate(t = as.numeric((date - date.ref)/(365.25*ystan))) %>% # years since date ref, scaled to per 5 years
  mutate(journaln = as.numeric(as.factor(journal)))

model.file = 'bugs.gender.model.mvn.txt'
bugs = file(model.file, 'w')
cat('model{
    for (i in 1:N){
      females[i] ~ dpois(p[i])
      log(p[i]) <- offset[i] + time[i] # 
      time[i] <- (0.5*parms[journal[i],1]*t[i]) - (2*exp(0.5*parms[journal[i],1]*t[i])) - parms[journal[i],2]
    }
    for (k in 1:N.journal){
      parms[k,1:2] ~ dmnorm(mu.parms[1:2], Omega[1:2, 1:2])
    }
    mu.parms[1] ~ dnorm(0, 0.01)
    mu.parms[2] ~ dnorm(0, 0.01)
    Omega[1:2, 1:2] ~ dwish(R[1:2,1:2], 2)
}\n', file=bugs)
close(bugs)

# random intercept and slope model
model.file = 'bugs.gender.model.linear.txt'
bugs = file(model.file, 'w')
cat('model{
    for (i in 1:N){
      females[i] ~ dpois(p[i])
      log(p[i]) <-  offset[i] + parms[journal[i],1] + (parms[journal[i],2]*t[i])
    }
    for (k in 1:N.journal){
      parms[k,1:2] ~ dmnorm(mu.parms[1:2], Omega[1:2, 1:2])
    }
    mu.parms[1] ~ dnorm(0, 0.01)
    mu.parms[2] ~ dnorm(0, 0.01)
    Omega[1:2, 1:2] ~ dwish(R[1:2,1:2], 2)
}\n', file=bugs)
close(bugs)


# prepare the data
bdata = with(for.model, list(N = nrow(for.model),
                             journal = journaln,
                             N.journal = max(journaln),
                             females = females,
                             R = R,
                             offset = log(n), # log link
                             t = t))
# intial values
R = matrix(data=c(1,0,0,1), nrow=2)
inits = list(list(mu.parms=c(0,0), Omega=R))

# run BUGS
parms = c('parms', 'mu.parms')
bugs.results =  bugs(data=bdata, inits=inits, parameters=parms, model.file=model.file,
                     n.chains=1, n.burnin=10000, n.thin=3, n.iter=30000, debug=T,
                     bugs.directory="C:/Program Files/WinBUGS14")

## plot results by journal
n.journals = bdata$N.journal
journal.names = select(for.model, journaln, journal) %>%
  distinct() %>%
  mutate(t = seq(0, 10, length.out=n.journals)) %>%
  tidyr::expand(journaln, t)
#results.frame = data.frame(journaln=1:n.journals, r = bugs.results$median$r, c = bugs.results$median$c)
#results.frame = data.frame(journaln=1:n.journals, r = bugs.results$median$parms[,1], c = bugs.results$median$parms[,2])
results.frame = data.frame(journaln=1:n.journals, intercept = bugs.results$median$parms[,1], slope = bugs.results$median$parms[,2])
results.frame = inner_join(journal.names, results.frame, by='journaln')
# now plot the curves
labels = unique(for.model$journal)
#results.plot =  mutate(results.frame, p = exp(0.5*r*t) / (2*exp(0.5*r*t)) + c))
results.plot =  mutate(results.frame, p = exp(intercept + slope*t))
ggplot(results.plot, aes(x=t, y=p, col=factor(journaln)))+
  geom_line(size=1.1)+
  scale_color_manual(NULL, values = colorRampPalette(brewer.pal(9,"Set1"))(n.journals), labels=labels)+
  theme_bw()+
  xlab('Years since 2000')
