################################################################################-
#
# SWELL trial Vainre et al 2024 BMJ Mental Health
# Daily monitoring data
# Timepoints: daily during the intervention
# Written by: Maris Vainre, 2023
#
################################################################################-

#Mixed model repeated measures analysis will be performed using the daily 
#monitoring of work performance to study changes between arms during the 
#intervention. The analysis will be done according to the intention-to-treat 
#principle.
#Group allocation will be the fixed effect. 
#The sum of the scores to the 5 items of the daily monitoring questionnaire 
#will be nested within participants who are in turn nested within employer.


################################################################################-
# Set-up ----
################################################################################-

library(dplyr)
library(tidyr)
library(here)
library(lme4)
library(miceadds)
library(mice)
library(rstatix)
library(broom.mixed)

options(scipen = 999)
options(contrasts = c("contr.helmert", "contr.poly"))
#options("contrasts")

SWELL_MIDM_rds <- readRDS(here::here("./Data/MultipleImputation/Output/SWELL_MI_DM_notcompleted.rds"))

source(here::here("./Code/Analysis/SWELL_GlobalParameters.Formulae.R"))



## Relevel factors for arm ----
# 1. convert the mids to a data frame
SWELL_MIDM <- mice::complete(SWELL_MIDM_rds, action = "long", include = TRUE)


# 2. transform data T1
SWELL_MI_DM <- list()

for(i in 0:max(SWELL_MIDM$.imp)) {
  SWELL_MI_DM[[i+1]] <- SWELL_MIDM %>%
    subset(.imp == i)  %>%
    dplyr::mutate(arm = factor(arm, level = c(2, 1)), # the first one will be taken as the reference factor
                  employer_bin = dplyr::case_when(employer_anon.factor == "Local Authority 1" ~ 1,
                                                  employer_anon.factor == "Local Authority 2" ~ 1,
                                                  employer_anon.factor == "Local Authority 3" ~ 1,
                                                  TRUE ~ 2),
                  employer_bin = factor(employer_bin, level = c(1, 2))) 
                  }

# 3. convert back to mids T1
SWELL_MI_DM <- mice::as.mids(do.call(rbind, SWELL_MI_DM))




################################################################################-
# Analyse ----
################################################################################-



################################################################################-
## Mixed effects ---- 

### As prespecified -----
WRFQ_DM <- with(SWELL_MI_DM,
                exp = lme4::lmer(wrfq_score ~ 1 + day + arm + employer_bin + (1 +  employer_bin | record_id),
                                 REML = FALSE))

# This produces a singularity warning which could be to do with the employer factor variable. 
#Warning messages:
#  1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                    Model failed to converge with max|grad| = 0.0974441 (tol = 0.002, component 1)
#                  2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                    Model failed to converge with max|grad| = 0.150848 (tol = 0.002, component 1)
#                                  3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                                    Model failed to converge with max|grad| = 0.0214681 (tol = 0.002, component 1)

WRFQ_DM_pool <- pool(WRFQ_DM, dfcom = 241-4)
WRFQ_DM_pool 
#Class: mipo    m = 100 
#term   m   estimate
#1   (Intercept) 100 78.1180417
#2           day 100  0.2036040
#3          arm1 100 -0.2056977
#4 employer_bin1 100 -0.9982821
#ubar           b           t
#1 1.3498897931 0.854865045 2.213303489
#2 0.0003501917 0.002179912 0.002551903
#3 0.6559683788 0.199221420 0.857182013
#4 1.2761079056 0.408232730 1.688422963
#dfcom        df       riv    lambda
#1   237 117.46021 0.6396179 0.3901018
#2   237  25.95727 6.2871610 0.8627723
#3   237 163.48941 0.3067429 0.2347385
#4   237 160.46225 0.3231036 0.2442013
#fmi
#1 0.4002279
#2 0.8722503
#3 0.2439314
#4 0.2534487
WRFQ_DM_summary <- summary(WRFQ_DM_pool, 
                           conf.int = TRUE, 
                           conf.level = 0.95)
WRFQ_DM_summary
#term   estimate  std.error
#1   (Intercept) 78.1180417 1.48771754
#2           day  0.2036040 0.05051636
#3          arm1 -0.2056977 0.92584125
#4 employer_bin1 -0.9982821 1.29939331
#statistic        df
#1 52.5086513 117.46021
#2  4.0304568  25.95727
#3 -0.2221739 163.48941
#4 -0.7682678 160.46225
#p.value
#1 0.00000000000000000000000000000000000000000000000000000000000000000000000000000000002075372
#2 0.00043285729251287018756050306578231356979813426733016967773437500000000000000000000000000
#3 0.82445582484788415555954088631551712751388549804687500000000000000000000000000000000000000
#4 0.44345724483574056051793377264402806758880615234375000000000000000000000000000000000000000
#2.5 %     97.5 %
#  1 75.17181568 81.0642677
#2  0.09975783  0.3074502
#3 -2.03384573  1.6224503
#4 -3.56439963  1.5678354

#### leaving employer out for a more simpler model (no warning produced, results are similar)

#Evan McCormick says this one makes sense and I should take out employer as random effects 
#if there's singularity error as I won't estimate anything.

WRFQ_DMwoemp <- with(SWELL_MI_DM,
                     exp = lme4::lmer(wrfq_score ~ 1 + arm + day + employer_bin + (1 | record_id),
                                 REML = FALSE))

WRFQ_DMwoemp_pool <- pool(WRFQ_DMwoemp, dfcom = 241-4)
WRFQ_DMwoemp_pool 

#Class: mipo    m = 100 
#term   m   estimate
#1   (Intercept) 100 78.1171138
#2          arm1 100 -0.1500651
#3           day 100  0.2036040
#4 employer_bin1 100 -0.9986480
#ubar           b           t
#1 1.2014506697 0.854854866 2.064854085
#2 0.6613143175 0.203268344 0.866615345
#3 0.0003501917 0.002179912 0.002551903
#4 1.1276675195 0.408316759 1.540067446
#dfcom        df       riv    lambda
#1   237 110.14851 0.7186341 0.4181426
#2   237 162.79624 0.3104439 0.2368998
#3   237  25.95728 6.2871604 0.8627723
#4   237 153.01697 0.3657106 0.2677804
#fmi
#1 0.4284274
#2 0.2461051
#3 0.8722503
#4 0.2771668
WRFQ_DMwoemp_summary <- summary(WRFQ_DMwoemp_pool, 
                           conf.int = TRUE, 
                           conf.level = 0.95)
WRFQ_DMwoemp_summary
#           term   estimate  std.error  statistic      df
#1   (Intercept) 78.1171138 1.43696002 54.3627610 110.14851
#2          arm1 -0.1500651 0.93092177 -0.1612006 162.79624
#3           day  0.2036040 0.05051636  4.0304568 25.95728
#4 employer_bin1 -0.9986480 1.24099454 -0.8047159 153.01697

#p.value
#1 0.000000000000000000000000000000000000000000000000000000000000000000000000000000002148222
#2 0.872135310814193598361043768818490207195281982421875000000000000000000000000000000000000
#3 0.000432857276319181978607719685214760829694569110870361328125000000000000000000000000000
#4 0.422232204851522463506796611909521743655204772949218750000000000000000000000000000000000
#2.5 %     97.5 %
#  1 75.26943907 80.9647886
#2 -1.98830337  1.6881731
#3  0.09975783  0.3074502
#4 -3.45034264  1.4530466

saveRDS(WRFQ_DM_summary, here::here("./Data/Tasks/4.output/WRFQ_DM_summary.rda"))