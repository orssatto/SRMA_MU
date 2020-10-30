
#### Motor unit discharge rates and aging review
# Meta regression analysis
# Author: DN Borg
# Date: 04 September 2020

library(dplyr)
library(janitor)
library(naniar)
library(meta)
library(metafor)
library(dmetar)

setwd("~/Dropbox/Research projects/Project - Motor unit and aging review")


#### Meta regression analysis
####
####
d = read.csv("Excel table DRs all data 290920.csv") %>% clean_names()
vis_miss(d)

# Remove Kamen (2004)
d <- d %>% filter(!author_date == "Kamen 2004")
vis_miss(d)

# Look at unique studies
d %>% group_by(author_date) %>%
  summarise(count = n()) %>% View()

# Studies per subgroup
table(d$subgroup)

# Look at names
sort(names(d))

# Convert intensity onto 0-100 scale
d$x_mvc <- d$x_mvc*100


# Meta-reg upper only
dsub <- d %>% filter(subgroup == "Upper")

meta_upper <- metacont(n.e = nc,
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
meta_upper
summary(meta_upper)

meta_upper <- metareg(meta_upper, x_mvc)
summary(meta_upper)

# Meta-reg lower flexor studies only
dsub <- d %>% filter(subgroup == "Lower_flexor")

meta_lower_flexor <- metacont(n.e = nc,
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
meta_lower_flexor
summary(meta_lower_flexor)


meta_lower_flexor <- metareg(meta_lower_flexor, x_mvc)
summary(meta_lower_flexor)



# Meta-reg lower extensor studies only
dsub <- d %>% filter(subgroup == "Lower_extensor")

meta_lower_extensor <- metacont(n.e = nc,
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
meta_lower_extensor
summary(meta_lower_extensor)


meta_lower_extensor <- metareg(meta_lower_extensor, x_mvc)
summary(meta_lower_extensor)

# Combine the above in a panel plot
png(file = 'metareg-three-panel.png', width = 6, height = 12, res = 600, units = "in") 
par(mfrow=c(3,1), oma=c(1,2,2,1))
bubble(meta_lower_extensor,
       xlab = "MVC intensity (%)",
       ylab = "Standardised mean difference",
       col.line = "red",
       xlim = c(0,100),
       studlab = F,
       bg = "white",
       max.cex = 4,
       cex.lab=1.5, cex.axis=1.5, cex.sub=1.5)
title(main = "(A) Lower-body extensor muscles", adj = 0, cex.main=1.75)
bubble(meta_lower_flexor,
       xlab = "MVC intensity (%)",
       ylab = "Standardised mean difference",
       col.line = "red",
       xlim = c(0,100),
       ylim = c(-2,0),
       studlab = F,
       bg = "white",
       max.cex = 4,
       cex.lab=1.5, cex.axis=1.5, cex.sub=1.5)
title(main = "(B) Lower-body flexor muscles", adj = 0, cex.main=1.75)
bubble(meta_upper,
       xlab = "MVC intensity (%)",
       ylab = "Standardised mean difference",
       col.line = "red",
       xlim = c(0,100),
       studlab = F,
       bg = "white",
       max.cex = 4,
       cex.lab=1.5, cex.axis=1.5, cex.sub=1.5)
title(main = "(C) Upper-body muscles", adj = 0, cex.main=1.75)
dev.off() 


# Meta reg - all studies fit in a single model
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

TE = as.data.frame(meta_fit$TE)
seTE = as.data.frame(meta_fit$seTE)
study = as.data.frame(meta_fit$studlab)

df <- cbind(TE,seTE,study)
df_x_variables <- d %>% select(subgroup, x_mvc)
dat <- cbind(df,df_x_variables)

fit <- rma(yi = meta_fit$TE,
           sei = meta_fit$seTE,
           data = dat,
           method = "ML",
           mods = ~ x_mvc*subgroup,
           test = "knha")
fit
