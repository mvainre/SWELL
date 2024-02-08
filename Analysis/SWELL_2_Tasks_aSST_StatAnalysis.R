################################################################################-
#
# SWELL trial Vainre et al 2024 BMJ Mental Health
# Affective Stop-Signal Task analysis
# Timepoints: T012
# Written by Maris Vainre, 2023
#
################################################################################-



################################################################################-
# Set-up ----
################################################################################-

library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(miceadds)
library(mice)
library(rstatix)
library(afex)
#library(WRS2)
library(broom.mixed)
library(psycho)

options(scipen = 999)

source(here::here("./Code/Analysis/SWELL_GlobalParameters.Formulae.R"))

SWELL_MI_aSST_long <- read.csv(here::here("./Data/Tasks/3.processed-data/SWELL_aStopSignalTask_imp_long.csv"))
SWELL_MI_aSST_longmids_T1 <- readRDS(here::here("./Data/Tasks/3.processed-data/SWELL_MI_aSST_longmids_T1.rds"))
SWELL_MI_aSST_longmids_T2 <- readRDS(here::here("./Data/Tasks/3.processed-data/SWELL_MI_aSST_longmids_T2.rds"))

################################################################################-
# Test assumptions ----
################################################################################-

# The SAP pre-specified a three-way ANOVA. However, ANOVA needs certain assumptions to be met.
# Let's test if they are. 
# Following this: https://cran.r-project.org/web/packages/afex/vignettes/assumptions_of_ANOVAs.html

# These are:


## Assumption 1: Observations are independent and identically distributed - OK ----
# this cannot be tested but given it was an RCT and the conditions within this task were counter-balanced,
# this assumption is met



## Assumption 2: Heterogeneity of Variances - OK ----
#library(plyr)
#library(afex)
#library(performance)
#
#SWELL_MI_aSST_long_df <- SWELL_MI_aSST_long |>
#  dplyr::mutate(arm = as.factor(arm)) 
#
#SWELL_MI_aSST_unimp <- SWELL_MI_aSST_long_df |>
#  dplyr::filter(.imp == 0)
#
#o0 <- afex::aov_ez(id = "record_id",
#                   dv = "SSRT",
#                   data = SWELL_MI_aSST_long_df, #SWELL_MI_aSST_unimp, #
#                   within = c("condition", "time"),
#                   between = c("arm"), 
#                   na.rm = TRUE,
#                   fun_aggregate = mean)
#
#performance::check_homogeneity(o0) #OK: There is not clear evidence for different variances across groups (Levene's Test, p = 0.519).
#
### Assumption 3: Sphericity - VIOLATED ----
#
#performance::check_sphericity(o0) 
#
##Warning: Sphericity violated for: 
##- time (p < .001)
##- arm:time (p < .001)
##- condition:time (p < .001)
##- arm:condition:time (p < .001).
#
#
## Assumption 4: Normality of Residuals - VIOLATED ----

#is_norm0 <- performance::check_normality(o0)
#
#plot(is_norm0)
#
#plot(is_norm0, type = "qq")
#
#plot(is_norm0, type = "qq", detrend = TRUE)
#
#t0 <- afex::aov_ez(id = "record_id",
#                   dv = "SSRT",
#                   data = SWELL_MI_aSST_long_df, #SWELL_MI_aSST_unimp, #
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
#LT_T01 <- with(SWELL_MI_aSST_longmids,
#               exp = WRS2::bwtrim(T1 ~ arm * condition * T0 * employer_anon.factor, id = record_id))

# Mair & Wilcox (2020) suggest mixed effect models using lme4 is a good alternative to bwtim (10.3758/s13428-019-01246-w)
# and Schielzeth et al (2020: 10.1111/2041-210X.13434) suggest that linear mixed-effects models are robust to violations of 
# distributional assumptions.




################################################################################-
## SSRT ----

### Post-intervention ----
outcome.of.interest <- "aSST"
submeasure <- "SSRT"
timepoint.of.interest <- "T1"
baseline <- "T0"

library(lme4)
aSST_T01 <- with(SWELL_MI_aSST_longmids_T1,
                 exp = lme4::lmer(SSRT ~ arm * condition + time + (1 | record_id)))

# the 1|record_id should take into account that all of the var before are nested within record ID.


model_summary <- summary(pool(aSST_T01, dfcom = 241-4),
                            conf.int = TRUE, 
                            conf.level = 0.95)

model_summary 


#             term    estimate std.error  statistic       df
#1     (Intercept) 262.1576889  2.920154 89.7752951 162.7408
#2            arm1   4.1752668  2.887765  1.4458473 168.2857
#3      condition1  -1.1777386  2.017408 -0.5837881 143.6257
#4           time1  -7.1887619  2.126035 -3.3813002 123.3763
#5 arm1:condition1  -0.8237328  2.039716 -0.4038468 139.0486
#p.value      2.5 %
#1 0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001532458 256.391412
#2 0.15007979257376211079666461500892182812094688415527343750000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000  -1.525645
#3 0.56027860489304015523259749897988513112068176269531250000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000  -5.165385
#4 0.00096676803042308517757014163507278681208845227956771850585937500000000000000000000000000000000000000000000000000000000000000000000000000000000000 -11.396990
#5 0.68694579481714201918407525226939469575881958007812500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000  -4.856601
#97.5 %
#1 267.923966
#2   9.876179
#3   2.809907
#4  -2.980534
#5   3.209136

# Interpretation:
# the light exercise group had slightly lower SSRT compared to mindfulness group although the difference is not statistically significant. 
# In the neutral condition, the SSRT was slighhtly higher than in the negative condition (opposite to what we predicted), but not statistically significant.
# There was a time effect, in T1 the SSRT was lower (p < 0.00081).
# There was no interaction effect: in the negative condition SSRT were slightly longer in the LE group.

d <- -0.4038468*sqrt(1/119+1/122)
d <- -3.3813002*sqrt(1/241+1/241)


saveRDS(model_summary, here::here("./Data/Tasks/4.output/aSST_T01_summary.rda"))


Effsizes_t1 <- read.csv(here::here("./Data/RedCap/outputs/SWELL_effectsizes_tasks.csv"))

Effsizes_t2 <- data.frame(measure = outcome.of.interest, 
                          submeasure = submeasure,
                          d.bwgroups = model_summary$statistic[5]*adj*-1, 
                          p.value = model_summary$p.value[5]) 

names(Effsizes_t2)[names(Effsizes_t2) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t2)[names(Effsizes_t2) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)


### Follow-up ----
outcome.of.interest <- "aSST"
submeasure <- "SSRT"
timepoint.of.interest <- "T2"
baseline <- "T0"


aSST_T02 <- with(SWELL_MI_aSST_longmids_T2,
               exp = lme4::lmer(SSRT ~ arm * condition + time + (1 | record_id)))

# the 1|record_id should take into account that all of the var ebfore are nested within record ID.

#pool <- pool(aSST_T02, dfcom = 241-4)


model_summary  <- summary(pool(aSST_T02, dfcom = 241-4),
                            conf.int = TRUE, 
                            conf.level = 0.95)

model_summary 


#             term   estimate std.error  statistic       df
#1     (Intercept) 261.441274  2.988801 87.4736281 166.4126
#2            arm1   4.268687  2.969219  1.4376466 169.7292
#3      condition1  -1.990002  2.143300 -0.9284755 133.7046
#4           time1  -7.904014  2.239455 -3.5294367 118.0174
#5 arm1:condition1  -1.156556  2.170030 -0.5329677 129.0098
#p.value      2.5 %
#1 0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004804685 255.540418
#2 0.152375433999589970923693726945202797651290893554687500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000  -1.592667
#3 0.354834111598991586156159883103100582957267761230468750000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000  -6.229161
#4 0.000594622497062780312922225522953567633521743118762969970703125000000000000000000000000000000000000000000000000000000000000000000000000000000000000 -12.338738
#5 0.594972807780057344473334524082019925117492675781250000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000  -5.450011
#97.5 %
#1 267.342129
#2  10.130042
#3   2.249158
#4  -3.469291
#5   3.136899
#

d <- -0.5329677*sqrt(1/119+1/122)
d <- -3.5294367*sqrt(1/241+1/241)

saveRDS(model_summary , here::here("./Data/Tasks/4.output/aSST_T02_summary.rda"))


Effsizes_t3 <- data.frame(measure = outcome.of.interest, 
                          submeasure = submeasure,
                          d.bwgroups = model_summary$statistic[5]*adj*-1, 
                          p.value = model_summary$p.value[5]) 

names(Effsizes_t3)[names(Effsizes_t3) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t3)[names(Effsizes_t3) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)


Effsizes_t4 <- dplyr::left_join(Effsizes_t2, Effsizes_t3)


Effsizes_tasks <- dplyr::bind_rows(Effsizes_t1, Effsizes_t4)




################################################################################-
# Export----
################################################################################-
write.csv(Effsizes_tasks, here::here("./Data/RedCap/outputs/SWELL_effectsizes_tasks.csv"), row.names = FALSE)

rm(list = ls())
