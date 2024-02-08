################################################################################-
#
# SWELL trial Vainre et al 2024 BMJ Mental Health
# Affective Learning Task
# Timepoints: T012
# Written by: Maris Vainre, 2023
#
################################################################################-

#This code analyses the affective learning task at all timepoints. 
#The pre-specified main outcome was accuracy

################################################################################-
# Set-up ----
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(miceadds)
library(mice)
library(rstatix)
library(broom.mixed)

options(scipen = 999)

source(here::here("./Code/Analysis/SWELL_GlobalParameters.Formulae.R"))


SWELL_MI_LT_rds <- readRDS(here::here("./Data/MultipleImputation/Output/SWELL_forMI_T012_notcompleted.rds"))
SWELL_LT_unimp <- read.csv(here::here("./Data/Tasks/3.processed-data/JATOS_LearningTask_eLTcomp.csv"))

# Assign names to arms when ready
arm1 <- Arm1.l 
arm2 <- Arm2.l 

################################################################################-
# Wrangle data ----

# the data were imputed in one format but for the statistical testing, the data 
# need to be transfromed, so that the condition variable (neu vs neg) 
# can be included in the model as a variable. This means 3 steps:
#  1. converting the mids object into a data.frame, 
#  2. doing the transformation and then 
#  3. converting it back to mids




## Full dataset ----

# 1. convert the mids to a data frame
SWELL_MI_LT <- mice::complete(SWELL_MI_LT_rds, action = "long", include = TRUE)

# 2. transform data
SWELL_MI_LT_long <- list()

for(i in 0:max(SWELL_MI_LT$.imp)) {
  SWELL_MI_LT_long[[i+1]] <- SWELL_MI_LT %>%
    subset(.imp == i)  %>%
    tidyr::pivot_longer(cols = c(LT.accurancy.cummean_neg_T0:LT.accurancy.cummean_neu_T1),
                        values_to = "acc_cummean") |>
    dplyr::mutate(condition = stringr::str_split(name, "_", simplify = TRUE)[,2],
                  time = stringr::str_split(name, "_", simplify = TRUE)[,3])  %>%
    dplyr::select(-c(name))  %>%
    #tidyr::pivot_wider(id_cols = c(.imp:employer_anon.factor, condition),
    #                   names_from = time,
    #                   values_from = acc_cummean) %>%
    dplyr::mutate(arm = dplyr::case_when(arm == 1 ~ arm1,
                                         arm == 2 ~ arm2,
                                         TRUE ~ "Unknown"), #this should not happen but is here for safeguarding
                  arm = as.factor(arm),
                  arm = factor(arm, levels = c("Light exercise", "Mindfulness")),
                  .id = 1:nrow(.),
                  condition = as.factor(condition),
                  condition = factor(condition, levels = c("neu", "neg")),
                  employer_2fact = dplyr::case_when(employer_anon.factor == "Local Authority 1" ~ "Local Authority",
                                                    employer_anon.factor == "Local Authority 2" ~ "Local Authority",
                                                    employer_anon.factor == "Local Authority 3" ~ "Local Authority",
                                                    TRUE ~ "Other"),
                  employer_2fact = as.factor(employer_2fact))
}

# 3. convert back to mids
SWELL_MI_LT_longmids <- mice::as.mids(do.call(rbind, SWELL_MI_LT_long))



## Post-int and baseline dataset ----
## this is for easier data analysis later on.

# 2. transform data T1
SWELL_MI_LT_long_T1 <- list()

  for(i in 0:max(SWELL_MI_LT$.imp)) {
    SWELL_MI_LT_long_T1[[i+1]] <- SWELL_MI_LT %>%
       subset(.imp == i)  %>%
       tidyr::pivot_longer(cols = c(LT.accurancy.cummean_neg_T0:LT.accurancy.cummean_neu_T1),
                           values_to = "acc_cummean") |>
       dplyr::mutate(condition = stringr::str_split(name, "_", simplify = TRUE)[,2],
                     time = stringr::str_split(name, "_", simplify = TRUE)[,3])  %>%
       dplyr::filter(time != "T2") %>%
       #dplyr::select(-c(name))  %>%
       #tidyr::pivot_wider(id_cols = c(.imp:employer_anon.factor, condition),
       #                   names_from = time,
       #                   values_from = acc_cummean) %>%
       dplyr::mutate(arm = case_when(arm == 1 ~ arm1,
                                     arm == 2 ~ arm2,
                                     TRUE ~ "Unknown"), #this should not happen but is here for safeguarding
                     arm = as.factor(arm),
                     arm = factor(arm, levels = c(Arm2.l, Arm1.l)),
                     .id = 1:nrow(.),
                     condition = as.factor(condition),
                     condition = factor(condition, levels = c("neu", "neg")),
                     time = factor(time, levels = c("T0", "T1")),
                     employer_2fact = case_when(employer_anon.factor == "Local Authority 1" ~ "Local Authority",
                                                employer_anon.factor == "Local Authority 2" ~ "Local Authority",
                                                employer_anon.factor == "Local Authority 3" ~ "Local Authority",
                                                TRUE ~ "Other"),
                     employer_2fact = as.factor(employer_2fact))
  }

# 3. convert back to mids T1
SWELL_MI_LT_longmids_T1 <- mice::as.mids(do.call(rbind, SWELL_MI_LT_long_T1))



## Follow-up and baseline dataset ----

# 2. transform data
SWELL_MI_LT_long_T2 <- list()

for(i in 0:max(SWELL_MI_LT$.imp)) {
  SWELL_MI_LT_long_T2[[i+1]] <- SWELL_MI_LT %>%
    subset(.imp == i)  %>%
    tidyr::pivot_longer(cols = c(LT.accurancy.cummean_neg_T0:LT.accurancy.cummean_neu_T1),
                        values_to = "acc_cummean") |>
    dplyr::mutate(condition = stringr::str_split(name, "_", simplify = TRUE)[,2],
                  time = stringr::str_split(name, "_", simplify = TRUE)[,3])  %>%
    dplyr::filter(time != "T1") %>%
    dplyr::select(-c(name))  %>%
    #tidyr::pivot_wider(id_cols = c(.imp:employer_anon.factor, condition),
    #                   names_from = time,
    #                   values_from = acc_cummean) %>%
    dplyr::mutate(arm = case_when(arm == 1 ~ arm1,
                                  arm == 2 ~ arm2,
                                  TRUE ~ "Unknown"), #this should not happen but is here for safeguarding
                  arm = as.factor(arm),
                  arm = factor(arm, levels = c(Arm2.l, Arm1.l)),
                  .id = 1:nrow(.),
                  condition = as.factor(condition),
                  condition = factor(condition, levels = c("neu", "neg")),
                  time = factor(time, levels = c("T0", "T2")),
                  employer_2fact = case_when(employer_anon.factor == "Local Authority 1" ~ "Local Authority",
                                             employer_anon.factor == "Local Authority 2" ~ "Local Authority",
                                             employer_anon.factor == "Local Authority 3" ~ "Local Authority",
                                             TRUE ~ "Other"),
                  employer_2fact = as.factor(employer_2fact))
}

# 3. convert back to mids
SWELL_MI_LT_longmids_T2 <- mice::as.mids(do.call(rbind, SWELL_MI_LT_long_T2))


#################################################################################-
## Test assumptions ----
#
## The SAP pre-specified a three-way ANOVA. However, ANOVA needs certain assumptions to be met.
## Let's test if they are. 
## Following this: https://cran.r-project.org/web/packages/afex/vignettes/assumptions_of_ANOVAs.html
## These are:
##
#
### Assumption 1: Observations are independent and identically distributed - OK ----
## this cannot be tested but given it was an RCT and the conditions within this task were counter-balanced,
## this assumption is met
#
#
#
### Assumption 2: Heterogeneity of Variances - OK ----
#library(plyr)
#library(afex)
#library(performance)
#
#SWELL_MI_LT_long_df <- plyr::ldply(SWELL_MI_LT_long,  data.frame) %>%
#  dplyr::mutate(arm = as.factor(arm)) 
#
##base::plot(SWELL_MI_LT_long_df$acc_cummean, SWELL_MI_LT_long_df$arm)
#
#
#SWELL_MI_LT_unimp <- SWELL_MI_LT_long_df |>
#  dplyr::filter(.imp == 0)
#
#o0 <- afex::aov_ez(id = "record_id",
#                   dv = "acc_cummean",
#                   data = SWELL_MI_LT_long_df, #SWELL_MI_LT_unimp, #
#                   within = c("condition", "time"),
#                   between = c("arm"), 
#                   na.rm = TRUE,
#                   fun_aggregate = mean)
#
#performance::check_homogeneity(o0) #OK: There is not clear evidence for different variances across groups (Levene's Test, p = 0.488)
#
### Assumption 3: Specificity - VIOLATED ----
#
#performance::check_sphericity(o0) 
#
##Warning: Sphericity violated for: 
##  - time (p = 0.001)
##- arm:time (p = 0.001)
##- condition:time (p = 0.001)
##- arm:condition:time (p = 0.001).
#
#
### Assumption 4: Normalcy of Residuals - VIOLATED ----
#
#is_norm0 <- performance::check_normality(o0)
#
#plot(is_norm0)
#
#plot(is_norm0, type = "qq")
#
#plot(is_norm0, type = "qq", detrend = TRUE)
#
#t0 <- afex::aov_ez(id = "record_id",
#                   dv = "acc_cummean",
#                   data = SWELL_MI_LT_long_df, #SWELL_MI_LT_unimp, #
#                   transformation = "log",
#                   within = c("condition", "time"),
#                   between = c("arm"), 
#                   na.rm = TRUE,
#                   fun_aggregate = mean)
#
#is_normt0 <- performance::check_normality(t0)
#plot(is_normt0, type = "qq", detrend = TRUE)



################################################################################-
# Statistical testing ----
################################################################################-

#Since ANOVA assumptions are violated, we should run robust ANOVA as WRS2's bwtrim. 

#However, this cannot be pooled as a mice object.
#LT_T01 <- with(SWELL_MI_LT_longmids,
#               exp = WRS2::bwtrim(T1 ~ arm * condition * T0 * employer_anon.factor, id = record_id))

# Mair & Wilcox (2020) suggest mixed effect models using lme4 is a good alternative to bwtim (10.3758/s13428-019-01246-w)
# and Schielzeth et al (2020: 10.1111/2041-210X.13434) suggest that linear mixed-effects models are robust to violations of 
# distributional assumptions.



################################################################################-
## Accuracy ----

### Time 1: Post-intervention ----
outcome.of.interest <- "aLT"
submeasure <- "acc"
timepoint.of.interest <- "T1"
baseline <- "T0"


# Christina suggested this model:
# lmer( dependent.variable ~ condition * arm + time + (time | individual), data=XX)

# Modelling several clustering structures makes the model really complex and 
# the more complex the more data you need and the less likely that you can replicate it. 
# Also, a 'nesting structure' such as condition, you can add it easily as a binary predictor 
# without making the model much more complicated. Usually, a random intercept is added for something 
# like individual and maybe if you have a nesting structure or several (more than two) trials or school classes.
# And finally modelling several or nested random intercepts makes interpretation of results more difficult 
# (e.g., the estimate of a certain predictor is related to a changes in the dependent variable for a person nested 
# in condition - and you cannot directly measure the effect of condition which I think you may be interested in).


library(lme4)


aLT_T01 <- with(SWELL_MI_LT_longmids_T1,
               exp = lme4::lmer(acc_cummean ~ arm * condition + time + (1 | record_id))) # the 1|record_id should take into account that all of the variables before are nested within record ID.

pool <- pool(aLT_T01, dfcom = 241-4)

aLT_T01_summary <- summary(pool(aLT_T01, dfcom = 241-4), 
                            conf.int = TRUE, 
                            conf.level = 0.95)

aLT_T01_summary 

#             term      estimate   std.error   statistic       df
#1     (Intercept)  0.5552277988 0.003663784 151.5449007 195.6407
#2            arm1 -0.0007719388 0.003680193  -0.2097550 193.1495
#3      condition1  0.0012527182 0.002542769   0.4926590 153.2489
#4           time1  0.0086140383 0.002549332   3.3789387 152.0373
#5 arm1:condition1  0.0008673300 0.002560109   0.3387863 150.1694
#p.value
#1 0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008827157
#2 0.8340799788637474065922106092330068349838256835937500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
#3 0.6229586699010476458227003604406490921974182128906250000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
#4 0.0009245227330142867430001762940605658513959497213363647460937500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
#5 0.7352439817261062771081014943774789571762084960937500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
#         2.5 %      97.5 %
#1  0.548002217 0.562453381
#2 -0.008030464 0.006486586
#3 -0.003770687 0.006276124
#4  0.003577347 0.013650729
#5 -0.004191158 0.005925818

d <- 0.3387863*sqrt(1/119+1/122)
d <- 3.3789387*sqrt(1/241+1/241)


saveRDS(aLT_T01_summary, here::here("./Data/Tasks/4.output/aLT_T01_summary.rda"))

model_summary <- aLT_T01_summary

Effsizes_t1 <- data.frame(measure = outcome.of.interest, 
                         submeasure = submeasure,
                         d.bwgroups = model_summary$statistic[5]*adj, 
                         p.value = model_summary$p.value[5]) 

names(Effsizes_t1)[names(Effsizes_t1) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t1)[names(Effsizes_t1) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)



### Time 2: Follow-up ----
outcome.of.interest <- "aLT"
submeasure <- "acc"
timepoint.of.interest <- "T2"
baseline <- "T0"


aLT_T02 <- with(SWELL_MI_LT_longmids_T2,
                exp = lme4::lmer(acc_cummean ~ arm * condition + time + (1 | record_id))) # the 1|record_id should take into account that all of the variables before are nested within record ID.

pool <- pool(aLT_T02, dfcom = 241-4)

aLT_T02_summary <- summary(pool(aLT_T02, dfcom = 241-4), 
                           conf.int = TRUE, 
                           conf.level = 0.95)

aLT_T02_summary 
#            term      estimate   std.error    statistic       df
#1     (Intercept)  0.5630053423 0.003942747 142.79518868 173.8041
#2            arm1  0.0026364107 0.003821900   0.68981672 190.5483
#3      condition1 -0.0012814281 0.002813712  -0.45542259 137.5341
#4           time1  0.0164340094 0.002879277   5.70768640 128.6326
#5 arm1:condition1  0.0002229377 0.002605780   0.08555507 172.9148
#p.value
#1 0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004270911
#2 0.49114870704869850737139813645626418292522430419921875000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
#3 0.64952341720584549911166050151223316788673400878906250000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
#4 0.00000007511164826354336767916947126266791201487649232149124145507812500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
#5 0.93191909799855854323880066658603027462959289550781250000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
#2.5 %      97.5 %
#  1  0.555223514 0.570787170
#2 -0.004902256 0.010175078
#3 -0.006845158 0.004282302
#4  0.010737136 0.022130883
#5 -0.004920294 0.005366170


d <- 0.08555507*sqrt(1/119+1/122)
d <- 5.70768640 *sqrt(1/241+1/241)


saveRDS(aLT_T02_summary, here::here("./Data/Tasks/4.output/aLT_T02_summary.rda"))


model_summary <- aLT_T02_summary

Effsizes_t2 <- data.frame(measure = outcome.of.interest, 
                         submeasure = submeasure,
                         d.bwgroups = model_summary$statistic[5]*adj, 
                         p.value = model_summary$p.value[5]) 

names(Effsizes_t2)[names(Effsizes_t2) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t2)[names(Effsizes_t2) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)

Effsizes <- dplyr::left_join(Effsizes_t1, Effsizes_t2, by = c("measure", "submeasure"))


################################################################################-

write.csv(Effsizes, here::here("./Data/RedCap/outputs/SWELL_effectsizes_tasks.csv"), row.names = FALSE)

rm(list = ls())
