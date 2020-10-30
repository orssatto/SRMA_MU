
#### Motor unit discharge rates and aging review
# Meta analysis
# Author: DN Borg
# Date: 04 September 2020

library(dplyr)
library(janitor)
library(naniar)
library(meta)
library(metafor)
library(dmetar)

setwd("~/Dropbox/Research projects/Project - Motor unit and aging review")



#### Meta analysis
####
####
d = read.csv("Excel table DRs pooled 151020.csv") %>% clean_names()
vis_miss(d)

# Remove Kamen (2004)
d <- d %>% filter(!author_date == "Kamen et al. (2004)")

# Making sure only unique studies
d %>% group_by(author_date) %>%
  summarise(count = n())

# Number of unique studies
length(unique(d$author_date))

# Studies per subgroup
table(d$subgroup)

# Look at names
sort(names(d))

# Meta analysis
# Details: https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/random.html
meta_fit <- metacont(n.e = nc,
                     mean.e = mc,
                     sd.e = sc,
                     n.c = ne,
                     mean.c = me,
                     sd.c = se,
                     data = d,
                     studlab = paste(author_date),
                     comb.fixed = F,
                     comb.random = T,
                     prediction = T,
                     sm = "SMD",
                     method.smd = "Hedges",
                     method.tau = "SJ")
meta_fit
summary(meta_fit)

forest(meta_fit, 
       sortvar = TE,
       xlim = c(-4,4),
       xlab ="Standardised mean difference",
       rightlabs = c("g","95% CI","Weight"),
       leftlabs = c("Author (year)", "N","Mean","SD","N","Mean","SD"),
       lab.e = "Old",
       lab.c = "Young",
       pooled.totals = T,
       smlab = "",
       text.random = "Overall effect",
       print.tau2 = T,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict = "black",
       print.I2.ci = F,
       digits.sd = 1,
       digits.mean = 1,
       mlab = "",
       ilab.xpos = 8,
       ilab.pos = 8,
       showweights = T)

# Subgroup analyses
sub_group_fit <- subgroup.analysis.mixed.effects(x = meta_fit, subgroups = d$subgroup)
summary(sub_group_fit)

#png(file = 'forestplot-subgroups.png', width = 8, height = 9.5, res = 600, units = "in") 
forest(x = sub_group_fit,
       xlim = c(-3,3),
       sortvar = meta_fit$TE,
       col.predict = "black",
       showweights = T,
       xlab = "Lower rates in older adults               Higher rates in older adults")
#dev.off()




#### Robustness check
####
####
influence_check <- InfluenceAnalysis(x = meta_fit, random = T)
summary(influence_check)
plot(influence_check, "influence")
plot(influence_check, "baujat")

## !!! Add sensitivity analysis


#### Publication bias
####
####
# Funnel plot
funnel(meta_fit, xlab = "Hedges' g", studlab = F)
#png(file = 'funnel-plot.png', width = 9, height = 7, res = 300, units = "in") 
funnel(meta_fit, 
       xlab = "Hedges' g", 
       contour = c(.95,.975,.99),
       col.contour = c("darkblue","blue","lightblue")) +
  legend(1.4, 0, c("p < .05", "p < .025", "p < .01"), bty = "n",
         fill = c("darkblue","blue","lightblue"))
#dev.off()

# Funnel plot symetry test
eggers.test(x = meta_fit)
trimfill(meta_fit) # Trim and fill
meta_fit$TE.fixed # Meta-estimate with all studies

# P-curve analysis
p_curve <- pcurve(meta_fit)
summary(p_curve)
#png(file = 'p-curve.png', width = 9, height = 7, res = 300, units = "in") 
pcurve(meta_fit)
#dev.off()




#### GOSH analysis
####
####
# Looking whether there are subgroups within the upper body studies
# Upper body examined due to the large degree of heterogenity observed
# Results: 3 studies largely contribute to the between study heterogenity

dsub <- d %>% filter(subgroup == "(3) Upper-body muscles")
meta_fit <- metacont(n.e = nc,
                     mean.e = mc,
                     sd.e = sc,
                     n.c = ne,
                     mean.c = me,
                     sd.c = se,
                     data = dsub,
                     studlab = paste(author_date),
                     comb.fixed = F,
                     comb.random = T,
                     prediction = T,
                     sm = "SMD",
                     method.smd = "Hedges",
                     method.tau = "SJ")
meta_fit
summary(meta_fit)
forest(meta_fit, sortvar = TE, xlim = c(-4,4))

# GOSH plot
fit <- rma(yi = meta_fit$TE, 
             sei = meta_fit$seTE,
             method = meta_fit$method.tau,
             test = "knha")
gosh_fit <- gosh(fit)

png(file = 'GOSH-plot.png', width = 9, height = 7, res = 300, units = "in") 
plot(gosh_fit, alpha= 0.5, col = "blue")
dev.off()

gosh.diagnostics(gosh_fit)

meta_fit <- metacont(n.e = nc,
                     mean.e = mc,
                     sd.e = sc,
                     n.c = ne,
                     mean.c = me,
                     sd.c = se,
                     data = dsub,
                     studlab = paste(author_date),
                     comb.fixed = F,
                     comb.random = T,
                     prediction = T,
                     sm = "SMD",
                     method.smd = "Hedges",
                     method.tau = "SJ",
                     exclude = c(1, 6, 11))

summary(meta_fit)
forest(meta_fit, sortvar = TE, xlim = c(-4,4))












