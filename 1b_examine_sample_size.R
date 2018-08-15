# 1b_examine_sample_size.R (lyra)
## Summarise simulations from 1_sample_size.R
# July 2018
library(tidyverse)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# get the data (split into different runs)
to.load = dir(pattern='^Sam')
all.res = NULL
for (f in to.load){
  load(f)
  all.res = rbind(all.res, sim.res)
}

# power and confidence intervals
z = qnorm(0.975)
ptable = filter(all.res, term=='changes') %>%
  group_by(term, expected.change, prop.change.year, n.journals) %>%
  summarise(count=n(),
            mean = mean(estimate),
            below = sum(p.value < 0.05),
            power = below/count,
            lower = power - z*sqrt(power*(1-power)/count),
            upper = power + z*sqrt(power*(1-power)/count))
ptable$n.journalsc = factor(paste('Journals=', ptable$n.journals)) # nicer facet label
ptable$expected.change = ptable$expected.change * 100 # change to % for better labels
# plot power
power.plot = ggplot(data=ptable, aes(x=expected.change, y=power, ymin=lower, ymax=upper, col=factor(prop.change.year)))+
  geom_line(lwd=1.2)+
  scale_color_manual('Proportion of\neditors-in-chief\nchanging per year', values=cbPalette[2:4])+
  geom_point()+
  geom_errorbar(width=0, lwd=1.2)+
  theme_bw()+
  scale_x_continuous(breaks=c(0,0.01,0.02))+
  xlab('Increase in the percent of female authors')+
  ylab('Power to find difference, %')+
  facet_wrap(~n.journalsc)+
  theme(panel.grid.minor = element_blank())

# histograms of mean effect
hplot = ggplot(data=all.res, aes(x=statistic, fill=factor(prop.change.year)))+
  geom_vline(lty=2, col='grey', xintercept = 0)+
  geom_histogram()+
  facet_grid(term ~ expected.change, scales='free')+
  theme_bw()

