
#### Motor unit discharge rates and aging review
# Meta analysis
# Author: DN Borg
# Date: 04 September 2020

# Packages
library(dplyr)
library(janitor)
library(naniar)
library(meta)
library(metafor)
library(dmetar)
library(metaviz)
library(ggplot2)
library(tidyr)

#### Meta analysis
d <- read.csv("Excel table DRs all data 261021.csv") %>%
  clean_names()

vis_miss(d)

# Remove Kamen (2004)
d <- d %>% filter(!author_date == "Kamen et al. (2004)")

# Study occurrences
d %>% group_by(author_date) %>%
  summarise(count = n()) %>%
  ggplot() + geom_histogram(aes(x = count), colour = 'white') +
  theme_minimal()

# Number of unique studies
length(unique(d$author_date))

# Studies per subgroup
table(d$muscle)

# Look at names
sort(names(d))

# Regroup muscles
df = d %>%
  mutate(
    muscle_regroup = as.factor(recode_factor(muscle,
                                   'Biceps femoris' = 'Hamstrings',
                                   'Vastus lateralis' = 'Quadriceps',
                                   'Vastus medialis' = 'Quadriceps',
                                   'SS Group' = 'Hamstrings',
                                   'Medial gastrocnemius' = 'Gastrocnemius',
                                   'Lateral gastrocnemius' = 'Gastrocnemius',
                                   'sup. trap.' = 'Trapezius',
                                   'Biceps Brachii' = 'Biceps brachii',
                                   'Tibialis Anterior' = 'Tibialis anterior',
                                   'Triceps Brachii' = 'Triceps brachii',
                                   'ADM' = 'Abductor digiti minimi')) 
  )


table(df$muscle_regroup)

# Calculate the effect size and se
meta_fit = metacont(n.e = ne,
                   mean.e = me,
                   sd.e = se,
                   n.c = nc,
                   mean.c = mc,
                   sd.c = sc,
                   studlab = paste(author_date),
                   comb.fixed = F,
                   comb.random = T,
                   prediction = T,
                   sm = "SMD",
                   method.smd = "Hedges",
                   method.tau = "SJ",
                   data = df)

meta_fit

# Pull effect sizes
dsub <- cbind(df,
              meta_fit$TE,
              meta_fit$seTE,
              meta_fit$w.random) %>%
  clean_names() %>%
  drop_na(meta_fit_se_te) %>%
  mutate(
    intensity = x_mvc*100,
    author_date = as.factor(author_date),
    id = row.names(.)
    )


# Multilevel model no mods
two_level_model_no_mod = rma.mv(yi = meta_fit_te, 
                         V = meta_fit_se_te,
                         slab = paste(author_date),
                         data = dsub,
                         random = ~ 1 | author_date, 
                         test = "t", 
                         method = "REML") # mods = ~ muscle_regroup + intensity

two_level_model_no_mod
confint(two_level_model_no_mod)


# Multilevel model with mods
two_level_model = rma.mv(yi = meta_fit_te, 
                     V = meta_fit_se_te,
                     slab = paste(author_date),
                     data = dsub,
                     random = ~ 1 | author_date, 
                     test = "t", 
                     method = "REML",
                    mods = ~ muscle_regroup + intensity) # mods = ~ muscle_regroup + intensity

two_level_model

two_level_model = rma.mv(yi = meta_fit_te, 
                         V = meta_fit_se_te,
                         slab = paste(author_date),
                         data = dsub,
                         random = ~ 1 | author_date, 
                         test = "t", 
                         method = "REML",
                         mods = ~ muscle_regroup) # mods = ~ muscle_regroup + intensity

two_level_model

png(file = 'forestplot-multilevel.png', width = 8, height = 10, res = 600, units = "in") 
viz_forest(x = two_level_model,
           study_labels = paste(dsub$author_date, dsub$intensity),
           group = dsub$muscle_regroup,
           x_limit = c(-3,8),
           annotate_CI = T,
           text_size = 2.5,
           variant = 'classic')
dev.off()



#### Publication bias
two_level_model_no_mod = rma.mv(yi = meta_fit_te, 
                                V = meta_fit_se_te,
                                slab = paste(author_date),
                                data = dsub,
                                random = ~ 1 | author_date, 
                                test = "t", 
                                method = "REML") # mods = ~ muscle_regroup + intensity

two_level_model_no_mod
funnel(two_level_model_no_mod, xlab = "Hedges' g")

dsub_no_dalton = dsub %>% filter(!author_date == 'Dalton 2010')

two_level_model_no_mod_no_dalton = rma.mv(yi = meta_fit_te, 
                                V = meta_fit_se_te,
                                slab = paste(author_date),
                                data = dsub_no_dalton,
                                random = ~ 1 | author_date, 
                                test = "t", 
                                method = "REML") # mods = ~ muscle_regroup + intensity

two_level_model_no_mod_no_dalton
confint(two_level_model_no_mod_no_dalton)
funnel(two_level_model_no_mod_no_dalton, xlab = "Hedges' g")

# Panel funnel plots
png(file = 'funnel-plot-figure.png', width = 8, height = 4.25, res = 600, units = "in") 
par(mfrow=c(1,2))
funnel(two_level_model_no_mod, xlab = "SMD", ylab = "Standard error"); title(main = "A", adj = 0, line = 1)
funnel(two_level_model_no_mod_no_dalton, xlab = "SMD",ylab = "Standard error"); title(main = "B", adj = 0, line = 1)
dev.off()



#### Lower body figure
dsub_lower <- dsub %>% filter(muscle_regroup %in% c('Hamstrings','Quadriceps','Gastrocnemius','Soleus','Tibialis anterior'))

# Multilevel model
two_level_model = rma.mv(yi = meta_fit_te, 
                         V = meta_fit_se_te,
                         slab = paste(author_date),
                         data = dsub_lower,
                         random = ~ 1 | author_date, 
                         test = "t", 
                         method = "REML",
                         mods = ~ muscle_regroup + intensity) # mods = ~ muscle_regroup + intensity


png(file = 'figure-lower-body.png', width = 7, height = 8, res = 600, units = "in") 
viz_forest(x = two_level_model,
           study_labels = paste(dsub_lower$author_date, dsub_lower$intensity,'%'),
           group = dsub_lower$muscle_regroup,
           x_limit = c(-3.25,3.25),
           annotate_CI = T,
           text_size = 3,
           xlab = 'SMD', 
           variant = 'classic')
dev.off()






#### Upper body figure
dsub_upper <- dsub %>% filter(muscle_regroup %in% c('First dorsal interosseous','Biceps brachii','Triceps brachii','Abductor digiti minimi',
                                                    'Extensor digitorum','Trapezius','Anconeus'))

# Multilevel model
two_level_model = rma.mv(yi = meta_fit_te, 
                         V = meta_fit_se_te,
                         slab = paste(author_date),
                         data = dsub_upper,
                         random = ~ 1 | author_date, 
                         test = "t", 
                         method = "REML",
                         mods = ~ muscle_regroup + intensity) # mods = ~ muscle_regroup + intensity


png(file = 'figure-upper-body.png', width = 7, height = 8, res = 600, units = "in") 
viz_forest(x = two_level_model,
           study_labels = paste(dsub_upper$author_date, dsub_upper$intensity, '%'),
           group = dsub_upper$muscle_regroup,
           x_limit = c(-3,13),
           annotate_CI = T,
           text_size = 3,
           xlab = 'SMD',
           variant = 'classic')
dev.off()




#### Sensitivity analysis
# Remove study with smallest effect
smallest = min(dsub$meta_fit_te)
dsub_small_removed = dsub %>% filter(meta_fit_te > smallest)
two_level_model_no_mod = rma.mv(yi = meta_fit_te, 
                                V = meta_fit_se_te,
                                slab = paste(author_date),
                                data = dsub_small_removed,
                                random = ~ 1 | author_date, 
                                test = "t", 
                                method = "REML") # mods = ~ muscle_regroup + intensity

two_level_model_no_mod
confint(two_level_model_no_mod)


# Remove study with largest effect
largest = max(dsub$meta_fit_te)
dsub_largest_removed = dsub %>% filter(meta_fit_te < largest)
two_level_model_no_mod = rma.mv(yi = meta_fit_te, 
                                V = meta_fit_se_te,
                                slab = paste(author_date),
                                data = dsub_largest_removed,
                                random = ~ 1 | author_date, 
                                test = "t", 
                                method = "REML") # mods = ~ muscle_regroup + intensity

two_level_model_no_mod
confint(two_level_model_no_mod)
