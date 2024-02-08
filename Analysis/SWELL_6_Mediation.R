################################################################################-
#
# SWELL trial Vainre et al 2024 BMJ Mental Health
# Mediation analysis
# Written by: Maris Vainre, 2023
#
################################################################################-

# Mediation analyses follows per-protocol principle. 
# To establish a clear time sequence, the mediation analysis will include 
# Stop-Signal Task’s stop-trials’ reaction time at post-intervention (T1) as
# mediator and WRFQ total score at follow-up (T2).

################################################################################-
# Set-up ----
################################################################################-

library(here)
library(dplyr)
library(tidyr)
library(miceadds)
library(mediation)

seed = 21022023
set.seed(seed)

options(scipen = 999)


source(here::here("./Code/Analysis/SWELL_GlobalParameters.Formulae.R"))

SWELL_MI_rds_orig <- readRDS(here::here("./Data/MultipleImputation/Output/SWELL_MI_Q.T012.rds"))




################################################################################-
# Prepare dataset for analysis ----
################################################################################-

# select variables that go into the mediation analysis.
# also, exclude participants who did not start the course, as registered in 
# the protocol.

# 1. convert the mids to a data frame
SWELL_MI <- mice::complete(SWELL_MI_rds_orig, action = "long", include = TRUE) 


# 2. transform data
SWELL_MI_t <- list()

for(i in 0:max(SWELL_MI$.imp)) {
  SWELL_MI_t[[i+1]] <- SWELL_MI |>
    subset(.imp == i)  |>
    dplyr::select(.imp:arm, startedCourse, WRFQ_T2, SSRTint.mean_neg_T1) |>
    dplyr::filter(startedCourse == "yes")
}

SWELL_MI_rds <- mice::as.mids(do.call(rbind, SWELL_MI_t))




################################################################################-
# Mediation through a series of regressions ----
################################################################################-

################################################################################-
# Total effect ----

total.effect <- with(SWELL_MI_rds,
                     exp = stats::lm(scale(WRFQ_T2) ~ arm))

total.effect_summary <- summary(pool(total.effect))

total.effect_summary
#         term    estimate std.error  statistic       df   p.value
#1 (Intercept)  0.03697798 0.1503626  0.2459254 64.96946 0.8065152
#2       armLE -0.07539199 0.2727496 -0.2764147 40.35952 0.7836402

saveRDS(total.effect_summary, here::here("./Data/RedCap/outputs/Mediation/total.effect_summary.rds"))




################################################################################-
# IV to mediator effect ----

iv2med.effect <- with(SWELL_MI_rds,
                  exp = stats::lm(SSRTint.mean_neg_T1 ~ arm)) 

iv2med.effect_summary <- summary(pool(iv2med.effect))

iv2med.effect_summary
#
#         term   estimate std.error statistic       df   p.value
#1 (Intercept) 260.917049  6.525547 39.983937 103.5923 0.0000000
#2       armLE  -9.738875  8.635627 -1.127756 127.8569 0.2615351
#

saveRDS(iv2med.effect_summary, here::here("./Data/RedCap/outputs/Mediation/iv2med.effect_summary.rds"))




################################################################################-
# Mediator to DV effect ----

med2dv.effect <- with(SWELL_MI_rds,
                      exp = stats::lm(WRFQ_T2 ~ arm + SSRTint.mean_neg_T1)) 

med2dv.effect_summary <- summary(pool(med2dv.effect))
#
med2dv.effect_summary
#term                     estimate   std.error  statistic       df       p.value
#1         (Intercept) 75.75109697 13.59960928  5.5700936 41.92216 0.00000165447
#2               armLE -1.52220863  5.41785323 -0.2809616 39.50183 0.78020630056
#3 SSRTint.mean_neg_T1  0.01365239  0.05160071  0.2645776 39.89436 0.79269700601
#

saveRDS(med2dv.effect_summary, here::here("./Data/RedCap/outputs/Mediation/med2dv.effect_summary.rds"))





################################################################################-
# Mediation using the mediate package ----
################################################################################-

# The use of the package was pre-specified. It was not clear how the package 
# deals with multiply imputed datasets. Here's the output.

################################################################################-
## Set up the variables ----

# Exclude the original (not imputed) dataset
SWELL_MI_list <- SWELL_MI_t[-1]

# the datasets have to be in a list to work for the mediations function
names(SWELL_MI_list) <- paste("imp", seq_along(SWELL_MI_list), sep = "");SWELL_MI_list


mediators <- c("SSRTint.mean_neg_T1")
outcome <- c("WRFQ_T2")

# the mediation function requires the treatment variable to be specified for each 
# data frame in the list. It is always "arm" in the current analysis. We imputed 
# 100 datasets.
treatment <- c(rep("arm", times = 100))


################################################################################-
## Mediation model ----

mediation.statinf <- mediations(datasets = SWELL_MI_list, 
                                treatment = treatment, 
                                mediators = mediators, 
                                outcome = outcome, 
                                covariates = NULL, 
                                interaction=FALSE, 
                                boot = FALSE,
                                conf.level=.95, sims=10000)

pooled.mediation.statinf <- amelidiate(mediation.statinf)

mediation_summary <- summary(pooled.mediation.statinf)
mediation_summary
#ACME = average causal mediation effects
#ADE = average direct effects

#Causal Mediation Analysis 
#
#Quasi-Bayesian Confidence Intervals
#
#               Estimate % CI Lower % CI Upper p-value
#ACME              0.613     -0.284       1.89       1
#ADE               2.232     -2.961       7.63       1
#Total Effect      2.845     -2.401       8.27       1
#Prop. Mediated    0.345     -1.749       2.30       1
#
#Sample Size Used: 210 
#
#
#Simulations: 10000 

# ACME based on MI lm is -9.738875*0.01365239 = -0.1329589 
# ADE is -1.52220863



#Causal Mediation Analysis 
#
#Quasi-Bayesian Confidence Intervals
#
#Estimate % CI Lower % CI Upper p-value
#ACME              0.612     -0.266       1.86       1
#ADE               2.160     -3.165       7.48       1
#Total Effect      2.773     -2.583       8.17       1
#Prop. Mediated    0.652     -1.757       2.34       1
#
#Sample Size Used: 210 
#
#
#Simulations: 10000 

saveRDS(mediation_summary, file = here::here("./Data/RedCap/outputs/Mediation/SWELL_mediation.10000sims.rda"))

plot(mediation_summary)




################################################################################-
# Mediation using unimputed data and mediation package ----
################################################################################-

# The use of the package was pre-specified.Cannot handle differences in observations
# between models, so had to only include complete observations (n = 43)

################################################################################-
## Set up the variables ----

# unimputed dataset only

# 1. convert the mids to a data frame and select necessary variables
SWELL_unimp.med <- mice::complete(SWELL_MI_rds, action = "long", include = TRUE) |>
  dplyr::select(.imp:arm, startedCourse, WRFQ_T2, SSRTint.mean_neg_T1) |>
  
  #only unimputed data
  dplyr::filter(.imp == 0) |>
  tidyr::drop_na()


Ns <- SWELL_unimp.med |>
  dplyr::group_by(arm) |>
  dplyr::summarise(n = n())



################################################################################-
## Total effect ----

total.effect_unimp <- lm(scale(WRFQ_T2) ~ arm, data = SWELL_unimp.med)

total.effect_unimp_summary <- summary(total.effect_unimp)

total.effect_unimp_summary

#Call:
#  lm(formula = scale(WRFQ_T2) ~ arm, data = SWELL_unimp.med)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.5931 -0.1900  0.1837  0.5539  1.4426 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept) -0.002824   0.202423  -0.014    0.989
#armLE        0.006747   0.312866   0.022    0.983
#
#Residual standard error: 1.012 on 41 degrees of freedom
#Multiple R-squared:  1.134e-05,	Adjusted R-squared:  -0.02438 
#F-statistic: 0.0004651 on 1 and 41 DF,  p-value: 0.9829


################################################################################-
# IV to mediator effect ----

iv2med.effect_unimp <- lm(SSRTint.mean_neg_T1 ~ arm, data = SWELL_unimp.med) 

iv2med.effect_unimp_summary <- summary(iv2med.effect_unimp)

iv2med.effect_unimp_summary

#Call:
#  lm(formula = SSRTint.mean_neg_T1 ~ arm, data = SWELL_unimp.med)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-91.642 -29.577   2.321  16.818 195.984 
#
#Coefficients:
#  Estimate Std. Error t value            Pr(>|t|)    
#(Intercept)  256.876      9.619  26.705 <0.0000000000000002 ***
#  armLE        -12.421     14.867  -0.835               0.408    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 48.1 on 41 degrees of freedom
#Multiple R-squared:  0.01674,	Adjusted R-squared:  -0.007244 
#F-statistic: 0.6979 on 1 and 41 DF,  p-value: 0.4083


################################################################################-
# Mediator to DV effect ----

med2dv.effect_unimp <- stats::lm(WRFQ_T2 ~ arm + SSRTint.mean_neg_T1, data = SWELL_unimp.med) 

med2dv.effect_unimp_summary <- summary(med2dv.effect_unimp)
#
med2dv.effect_unimp_summary


#Call:
#  stats::lm(formula = WRFQ_T2 ~ arm + SSRTint.mean_neg_T1, data = SWELL_unimp.med)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-42.324  -3.295   2.421   6.720  19.705 
#
#Coefficients:
#  Estimate Std. Error t value    Pr(>|t|)    
#(Intercept)         67.32761   10.72896   6.275 0.000000194 ***
#  armLE                0.79202    3.89928   0.203       0.840    
#SSRTint.mean_neg_T1  0.05697    0.04062   1.403       0.168    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 12.51 on 40 degrees of freedom
#Multiple R-squared:  0.0469,	Adjusted R-squared:  -0.0007573 
#F-statistic: 0.9841 on 2 and 40 DF,  p-value: 0.3826

##
#

mediation_unimp <- mediation::mediate(iv2med.effect_unimp, med2dv.effect_unimp, sims=1000, 
                                      treat="arm", mediator="SSRTint.mean_neg_T1")
summary(mediation_unimp)
plot(mediation_unimp)

# model not stable!! ie every run has different results due to bootstrapping

#Causal Mediation Analysis
#ACME = average causal mediation effects
#ADE = average direct effects
#
#Quasi-Bayesian Confidence Intervals
#
#Estimate 95% CI Lower 95% CI Upper p-value
#ACME            -0.6979      -3.6146         1.23    0.51
#ADE              0.9704      -6.6043         8.48    0.81
#Total Effect     0.2725      -7.4207         7.59    0.96
#Prop. Mediated   0.0229      -3.2275         4.00    0.91
#
#Sample Size Used: 43 
#
#
#Simulations: 1000 






################################################################################-
# Mediation using unimputed data and PROCESS script ----
################################################################################-

# The use of this method was not pre-specified. Gives similar results to mediation
# package. The output cannot be saved nor does it seem to create an object.
# This means it's difficult to report using RMarkdown.

################################################################################-
## Set up the variables ----

# unimputed dataset only

# 1. convert the mids to a data frame and select necessary variables
SWELL_unimp.med <- mice::complete(SWELL_MI_rds, action = "long", include = TRUE) |>
  dplyr::select(.imp:arm, startedCourse, WRFQ_T2, SSRTint.mean_neg_T1) |>
  
  #only unimputed data
  dplyr::filter(.imp == 0) |>
  dplyr::mutate(arm = as.numeric(arm))


source(here::here("./Code/Analysis/Mediation/Dontshare/process.R"))




process.med <- process(data = SWELL_unimp.med, 
                       y = "WRFQ_T2", 
                       x = "arm",
                       m = "SSRTint.mean_neg_T1",
                       total = 1, 
                       normal = 1, 
                       model = 4, 
                       seed = seed)

#saveRDS(process.med, here::here("./Data/RedCap/outputs/Mediation/process.med.rds"))

a             <- -12.4206
a_p           <- 0.4083
b             <- 0.0570
b_p           <- 0.1684
dir.eff       <- 0.7920
dir.eff_p     <- 0.8401
indir.eff     <- -0.7077
indir.eff_p   <- 0.5405 
total.eff     <- 0.0844
total.eff_p   <- 0.9829

unimp.mediation <- data.frame(a, a_p, b, b_p, dir.eff, dir.eff_p, indir.eff, indir.eff_p, total.eff, total.eff_p)

write.csv(unimp.mediation, here::here("./Data/RedCap/outputs/Mediation/unimp.mediation.csv"),
          row.names = FALSE)


#
#Model : 4                  
#Y : WRFQ_T2            
#X : arm                
#M : SSRTint.mean_neg_T1
#
#Sample size: 43
#
#Custom seed: 21022023
#
#
#*********************************************************************** 
#  Outcome Variable: SSRTint.mean_neg_T1
#
#Model Summary: 
#  R      R-sq       MSE         F       df1       df2         p
#0.1294    0.0167 2313.1580    0.6979    1.0000   41.0000    0.4083
#
#Model: 
#             coeff        se         t         p      LLCI      ULCI
#constant  269.2962   22.3297   12.0600    0.0000  224.2000  314.3924
#arm       -12.4206   14.8672   -0.8354    0.4083  -42.4460   17.6048
#
#*********************************************************************** 
#  Outcome Variable: WRFQ_T2
#
#Model Summary: 
#  R      R-sq       MSE         F       df1       df2         p
#0.2166    0.0469  156.4522    0.9841    2.0000   40.0000    0.3826
#
#Model: 
#                        coeff        se         t         p      LLCI      ULCI
#constant              66.5356   12.3838    5.3728    0.0000   41.5067   91.5644
#arm                    0.7920    3.8993    0.2031    0.8401   -7.0888    8.6729
#SSRTint.mean_neg_T1    0.0570    0.0406    1.4028    0.1684   -0.0251    0.1391
#
#************************ TOTAL EFFECT MODEL *************************** 
#  Outcome Variable: WRFQ_T2
#
#Model Summary: 
#  R      R-sq       MSE         F       df1       df2         p
#0.0034    0.0000  160.1450    0.0005    1.0000   41.0000    0.9829
#
#Model: 
#             coeff        se         t         p      LLCI      ULCI
#constant   81.8786    5.8754   13.9359    0.0000   70.0129   93.7443
#arm         0.0844    3.9119    0.0216    0.9829   -7.8159    7.9846
#
#*********************************************************************** 
#  Bootstrapping progress:
#  |>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>| 100%
#
#************ TOTAL, DIRECT, AND INDIRECT EFFECTS OF X ON Y ************
#  
#  Total effect of X on Y:
#  effect        se         t         p      LLCI      ULCI
#0.0844    3.9119    0.0216    0.9829   -7.8159    7.9846
#
#Direct effect of X on Y:
#effect        se         t         p      LLCI      ULCI
#0.7920    3.8993    0.2031    0.8401   -7.0888    8.6729
#
#Indirect effect(s) of X on Y:
#                       Effect    BootSE  BootLLCI  BootULCI
#SSRTint.mean_neg_T1   -0.7077    0.9349   -2.9203    0.8961
#
#Normal theory test for indirect effect(s):
#                       Effect        se         Z         p
#SSRTint.mean_neg_T1   -0.7077    1.1561   -0.6121    0.5405

#
#******************** ANALYSIS NOTES AND ERRORS ************************ 
#  
#  Level of confidence for all confidence intervals in output: 95
#
#Number of bootstraps for percentile bootstrap confidence intervals: 5000
#
#NOTE: Some cases with missing data were deleted. The number of deleted cases was: 167