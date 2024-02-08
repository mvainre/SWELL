################################################################################-
#
# SWELL trial Vainre et al 2024 BMJ Mental Health
# Main outcome analysis: WRFQ
# Timepoints: T01
# Written by: Maris Vainre, 2023
#
################################################################################-

# This analyses the pre-specified main outcome of the feasibility trial:
# Work Role Functioning Questionnaires (WRFQ) Total score at post-intervention (T1)

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
library(broom.mixed)
library(car)

options(scipen = 999)
options(contrasts = c("contr.helmert", "contr.poly"))

SWELL_MI_rds <- readRDS(here::here("./Data/MultipleImputation/Output/SWELL_MI_Q.T012.rds"))

source(here::here("./Code/Analysis/SWELL_GlobalParameters.Formulae.R"))

outcome.of.interest <- "WRFQ"


################################################################################-
# Test assumptions ----
################################################################################-

# The SAP pre-specified a three-way ANOVA. However, ANOVA needs certain assumptions to be met.
# Let's test if they are. 
# Following this: https://cran.r-project.org/web/packages/afex/vignettes/assumptions_of_ANOVAs.html



#################################################################################-
## Assumption 1: Observations are independent and identically distributed - OK ----

# this cannot be tested but given it was an RCT and the conditions within this task were counter-balanced,
# this assumption is met



#################################################################################-
## Assumption 2: Heterogeneity of Variances - OK ----

#library(plyr)
#library(afex)
#library(performance)
#
#SWELL_MI_unimp <- SWELL_MI_rds  |>
#  dplyr::filter(.imp == 0) |>
#  dplyr::filter(measure == outcome.of.interest)
#
#o0 <- afex::aov_ez(id = "record_id",
#                   dv = "value",
#                   data = SWELL_MI_unimp,
#                   within = c("time"),
#                   between = c("arm"), 
#                   na.rm = TRUE,
#                   fun_aggregate = mean)
#
#performance::check_homogeneity(o0) 
##OK: There is not clear evidence for different variances across groups (Levene's Test, p = 0.190).
#
#
#
#################################################################################-
## Assumption 3: Specificity - VIOLATED ----
#
#performance::check_sphericity(o0) 
#
##OK: Data seems to be spherical (p > 0.137).
#
#

#################################################################################-
## Assumption 4: Normalcy of Residuals - VIOLATED ----
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
#                   dv = "value",
#                   data = SWELL_MI_unimp, #SWELL_MI_LT_unimp, #
#                   transformation = "log",
#                   within = c("time"),
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
## Regression ----

#The results are the same as in SPSS

#relevel takes the control group as the reference group
WRFQ_T01 <- with(SWELL_MI_rds,
                 exp = stats::lm(scale(WRFQ_T1) ~ WRFQ_T0 + as.factor(employer_2fact) + relevel(arm, ref = 2))) 


pool <- pool(WRFQ_T01, dfcom = 241-4)

WRFQ_T01_summary <- summary(pool(WRFQ_T01, dfcom = 241-4), 
                            conf.int = TRUE, 
                            conf.level = 0.95)

WRFQ_T01_summary
#                        term     estimate   std.error  statistic       df   p.value        2.5 %     97.5 %
#1                (Intercept) -0.817220755 0.657456236 -1.2430040 48.96664 0.2197854 -2.138451295 0.50400979
#2                    WRFQ_T0  0.009691688 0.008788189  1.1028084 46.79175 0.2757502 -0.007989944 0.02737332
#3 as.factor(employer_2fact)1 -0.129639565 0.129856459 -0.9983297 70.75104 0.3215217 -0.388581781 0.12930265
#4     relevel(arm, ref = 2)1  0.061415101 0.126215072  0.4865909 44.11090 0.6289573 -0.192936600 0.31576680

# pre-post effect size
d <- 1.1028084*sqrt(1/241+1/241)

library(mitml)
testEstimates(as.mitml.result(WRFQ_T01))

est <- testEstimates(as.mitml.result(WRFQ_T01), extra.pars = TRUE)
est

confint(est, level = 0.95)




################################################################################-
# Export ----
################################################################################-

saveRDS(WRFQ_T01_summary, here::here("./Data/Tasks/4.output/WRFQ_T01_summary.rda"))

Effsizes <- data.frame(measure = outcome.of.interest, 
                       d.bwgroups_T1 = WRFQ_T01_summary$statistic[4]*adj, 
                       p.value_T1 = WRFQ_T01_summary$p.value[4])


write.csv(Effsizes, here::here("./Data/RedCap/outputs/SWELL_effectsizes_temp.csv"), row.names = FALSE)

