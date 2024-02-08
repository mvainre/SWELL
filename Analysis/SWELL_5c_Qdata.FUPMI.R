################################################################################-
#
# SWELL trial Vainre et al 2024 BMJ Mental Health
# Secondary questionnaire outcome data analysis
# Timepoints: T02
# Written by: Maris Vainre, 2023
#
################################################################################-

# This script runs statistical analyses for  several outcomes.
# Each output will be saved
# p-values and effect sizes will be extracted to a separate data frame


################################################################################-
# Set-up ----
################################################################################-


library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(miceadds)
library(mice)
library(mitools)
library(rstatix)
library(broom.mixed)
library(car)
library(effectsize)

options(scipen = 999)
options(contrasts = c("contr.helmert", "contr.poly"))
#options("contrasts")

SWELL_MI_rds <- readRDS(here::here("./Data/MultipleImputation/Output/SWELL_MI_Q.T012.rds"))
Effsizes_T1 <- read.csv(here::here("./Data/RedCap/outputs/SWELL_effectsizes_temp.csv"))

source(here::here("./Code/Analysis/SWELL_GlobalParameters.Formulae.R"))
source(here::here("./Code/Write-up/SWELL_Results.0.Formatting.R"))

#Define the colour scheme for the graphs
cols_Report <- cols_Report |>
  dplyr::filter(col_name == "SWELL") #or: "MRC"




################################################################################-
# WRFQ ----
################################################################################-



################################################################################-
## Test assumptions ----

# The SAP pre-specified a three-way ANOVA. However, ANOVA needs certain assumptions to be met.
# Let's test if they are. 
# Following this: https://cran.r-project.org/web/packages/afex/vignettes/assumptions_of_ANOVAs.html

# These are:



### Assumption 1: Observations are independent and identically distributed - OK ----
# this cannot be tested but given it was an RCT and the conditions within this task were counter-balanced,
# this assumption is met




#### Assumption 2: Heterogeneity of Variances - VIOLATED ----
#library(plyr)
#library(afex)
#library(performance)
#
#SWELL_MI_unimp <- mice::complete(SWELL_MI_rds) 
#
#o0 <- afex::aov_ez(id = "record_id",
#                   dv = paste0(outcome.of.interest, "_T1"),
#                   data = SWELL_MI_unimp,
#                   #within = paste0(outcome.of.interest, "_T0"),
#                   between = c("arm"), 
#                   na.rm = TRUE,
#                   fun_aggregate = mean)
#
#performance::check_homogeneity(o0) 
##Warning: Variances differ between groups (Levene's Test, p = 0.015).
#
#
#
#
#### Assumption 3: Specificity - VIOLATED ----
#
#performance::check_sphericity(o0) 
#
##OK: Data seems to be spherical (p > 0.137).
#
#
#### Assumption 4: Normalcy of Residuals - VIOLATED ----
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
#
#
#
#


################################################################################-
## Statistical testing ----

#Since ANOVA assumptions are violated, we should run robust ANOVA as WRS2's bwtrim. 

#However, this cannot be pooled as a mice object.
#LT_T01 <- with(SWELL_MI_LT_longmids,
#               exp = WRS2::bwtrim(T1 ~ arm * condition * T0 * employer_anon.factor, id = record_id))

# Mair & Wilcox (2020) suggest mixed effect models using lme4 is a good alternative to bwtim (10.3758/s13428-019-01246-w)
# and Schielzeth et al (2020: 10.1111/2041-210X.13434) suggest that linear mixed-effects models are robust to violations of 
# distributional assumptions.
#


## Regression ----

outcome.of.interest <- "WRFQ"
timepoint.of.interest <- "T2"
baseline <- "T0"

model <- with(SWELL_MI_rds,
              exp = stats::lm(scale(WRFQ_T2) ~ WRFQ_T0 + as.factor(employer_2fact) + arm)) 

model_summary <- summary(pool(model))

model_summary

#                        term    estimate   std.error  statistic       df   p.value
#1                (Intercept) -1.03221273 0.618626287 -1.6685562 54.60884 0.1009298
#2                    WRFQ_T0  0.01282327 0.008088856  1.5853002 54.40820 0.1186938
#3 as.factor(employer_2fact)1 -0.09283181 0.134880245 -0.6882535 64.80443 0.4937496
#4                       arm1 -0.01645188 0.138802017 -0.1185277 36.95940 0.9062914

d <- 1.5853002*sqrt(1/241+1/241)

################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/WRFQ_T02_summary.rda"))


#Create a T2 effect sizes data frame which will be later merged with T1 data
Effsizes_T2 <- data.frame(measure = outcome.of.interest, 
                       d.bwgroups = model_summary$statistic[4]*adj*-1, 
                       p.value = model_summary$p.value[4]) 

names(Effsizes_T2)[names(Effsizes_T2) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_T2)[names(Effsizes_T2) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)




################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/SWEMWBS_T02_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary$statistic[4]*adj*-1, 
                         p.value = model_summary$p.value[4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)


Effsizes_T2 <- dplyr::bind_rows(Effsizes_T2, Effsizes_t)




################################################################################-
# PSS ----
################################################################################-



################################################################################-
## T1 ----

### Regression ----

outcome.of.interest <- "PSS"
timepoint.of.interest <- "T1"
baseline <- "T0"

model <- with(SWELL_MI_rds,
              exp = stats::lm(scale(PSS_T1) ~ PSS_T0 + as.factor(employer_2fact) + arm)) 

model_summary <- summary(pool(model))

model_summary

#                        term    estimate  std.error  statistic       df           p.value
#1                (Intercept) -1.40676684 0.24775550 -5.6780447 130.2622 0.000000084577918
#2                     PSS_T0  0.07040760 0.01115442  6.3120827 125.8786 0.000000004304358
#3 as.factor(employer_2fact)1  0.06505163 0.08729015  0.7452346 154.9360 0.457258948593903
#4                       arm1  0.02589612 0.07498366  0.3453569 111.0479 0.730479436484328

#pre-post
d <- 6.31208274*sqrt(1/241+1/241)


################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/PSS_T01_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary$statistic[4]*adj*1, 
                         p.value = model_summary$p.value[4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)

Effsizes_T1 <- dplyr::bind_rows(Effsizes_T1, Effsizes_t)



################################################################################-
## T2 ----

### Regression ----

outcome.of.interest <- "PSS"
timepoint.of.interest <- "T2"
baseline <- "T0"

model <- with(SWELL_MI_rds,
              exp = stats::lm(scale(PSS_T2) ~ PSS_T0 + as.factor(employer_2fact) + arm)) 

model_summary <- summary(pool(model))

model_summary

#                        term     estimate  std.error  statistic        df       p.value
#1                (Intercept) -1.117705766 0.29306382 -3.8138647  92.94458 0.00024603655
#2                     PSS_T0  0.056617700 0.01298614  4.3598568  93.85910 0.00003331071
#3 as.factor(employer_2fact)1  0.073527784 0.09868167  0.7451007 122.51625 0.45763783007
#4                       arm1  0.009210864 0.08785842  0.1048376  82.82306 0.91675816564

#pre-post
d <- 4.3598568*sqrt(1/241+1/241)

################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/PSS_T02_summary.rda"))

#between groups
Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary$statistic[4]*adj*-1, 
                         p.value = model_summary$p.value[4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)


Effsizes_T2 <- dplyr::bind_rows(Effsizes_T2, Effsizes_t)




################################################################################-
# GAD7 ----
################################################################################-



################################################################################-
## T1 ----

### Regression ----

outcome.of.interest <- "GAD7"
timepoint.of.interest <- "T1"
baseline <- "T0"

model <- with(SWELL_MI_rds,
              exp = stats::lm(scale(GAD7_T1) ~ GAD7_T0 + as.factor(employer_2fact) + arm)) 

model_summary <- summary(pool(model))

model_summary

#                        term    estimate  std.error  statistic       df          p.value
#1                (Intercept) -0.4314307590 0.14734619 -2.928007636 145.0394 0.00396258022
#2                    GAD7_T0  0.0725811305 0.01664199  4.361325651 134.2503 0.00002554659
#3 as.factor(employer_2fact)1  0.0484643989 0.09708201  0.499210894 139.2121 0.61841881334
#4                       arm1 -0.0007261184 0.07656379 -0.009483835 123.4266 0.99244842023

#pre-post
d <- 4.361325651*sqrt(1/241+1/241)

################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/GAD7_T01_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary$statistic[4]*adj*1, 
                         p.value = model_summary$p.value[4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)

Effsizes_T1 <- dplyr::bind_rows(Effsizes_T1, Effsizes_t)



################################################################################-
## T2 ----

### Regression ----

outcome.of.interest <- "GAD7"
timepoint.of.interest <- "T2"
baseline <- "T0"

model <- with(SWELL_MI_rds,
              exp = stats::lm(scale(GAD7_T2) ~ GAD7_T0 + as.factor(employer_2fact) + arm)) 

model_summary <- summary(pool(model))

model_summary

#                      term     estimate  std.error   statistic        df     p.value
#1                (Intercept) -0.30863514 0.17433483 -1.7703585  95.32889 0.079864273
#2                    GAD7_T0  0.05721771 0.02027896  2.8215306  83.21147 0.005975481
#3 as.factor(employer_2fact)1  0.08805081 0.10878605  0.8093943 105.82661 0.420104475
#4                       arm1 -0.04751508 0.08591041 -0.5530771  94.68960 0.581514072

#pre-post
d <- 2.8215306*sqrt(1/241+1/241)

################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/GAD7_T02_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary$statistic[4]*adj*1, 
                         p.value = model_summary$p.value[4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)


Effsizes_T2 <- dplyr::bind_rows(Effsizes_T2, Effsizes_t)




################################################################################-
# PHQ9 ----
################################################################################-



################################################################################-
## T1 ----

### Regression ----

outcome.of.interest <- "PHQ9"
timepoint.of.interest <- "T1"
baseline <- "T0"

model <- with(SWELL_MI_rds,
              exp = stats::lm(scale(PHQ9_T1) ~ PHQ9_T0 + as.factor(employer_2fact) + arm)) 

model_summary <- summary(pool(model))

model_summary

#                       term    estimate  std.error  statistic       df          p.value
#1                (Intercept) -0.64777817 0.16136023 -4.0144846 144.4625 0.0000953540940
#2                    PHQ9_T0  0.08070959 0.01501394  5.3756447 134.5226 0.0000003273006
#3 as.factor(employer_2fact)1  0.08546438 0.09061567  0.9431523 152.4941 0.3470948182472
#4                       arm1  0.01824491 0.07942449  0.2297139 104.1397 0.8187646745351

#pre-post
d <- 5.3756447*sqrt(1/241+1/241)

################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/PHQ9_T01_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary$statistic[4]*adj*1, 
                         p.value = model_summary$p.value[4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)

Effsizes_T1 <- dplyr::bind_rows(Effsizes_T1, Effsizes_t)



################################################################################-
## T2 ----

### Regression ----

outcome.of.interest <- "PHQ9"
timepoint.of.interest <- "T2"
baseline <- "T0"

model <- with(SWELL_MI_rds,
              exp = stats::lm(scale(PHQ9_T2) ~ PHQ9_T0 + as.factor(employer_2fact) + arm)) 

model_summary <- summary(pool(model))

model_summary

#                      term     estimate  std.error   statistic        df     p.value
#1                (Intercept) -0.40815238 0.19765048 -2.0650209  95.40215 0.041633811
#2                    PHQ9_T0  0.05078104 0.01880916  2.6998033  85.06440 0.008369209
#3 as.factor(employer_2fact)1  0.05439322 0.10811952  0.5030842 107.03330 0.615938747
#4                       arm1 -0.06818342 0.09289763 -0.7339629  80.45361 0.465104955

#pre-post
d <- 2.6998033*sqrt(1/241+1/241)


################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/PHQ9_T02_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary$statistic[4]*adj*1, 
                         p.value = model_summary$p.value[4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)


Effsizes_T2 <- dplyr::bind_rows(Effsizes_T2, Effsizes_t)




################################################################################-
# Decentering ----
################################################################################-



################################################################################-
## T1 ----

### Regression ----

outcome.of.interest <- "decentr"
timepoint.of.interest <- "T1"
baseline <- "T0"

model <- with(SWELL_MI_rds,
              exp = stats::lm(scale(decentr_T1) ~ decentr_T0 + as.factor(employer_2fact) + arm)) 

model_summary <- summary(pool(model))

model_summary

#                        term    estimate  std.error  statistic       df          p.value
#1                (Intercept) -1.79732394 0.277783491 -6.4702331 128.9476 0.0000000018580009
#2                 decentr_T0  0.06231238 0.009211728  6.7644622 125.8745 0.0000000004508665
#3 as.factor(employer_2fact)1  0.02744252 0.089303817  0.3072939 135.0374 0.7590931036044037
#4                       arm1 -0.14038283 0.075344001 -1.8632250 102.9546 0.0652808880906663

#pre-post
d <- 6.7644622*sqrt(1/241+1/241)


################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/decentr_T01_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary$statistic[4]*adj*-1, 
                         p.value = model_summary$p.value[4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)

Effsizes_T1 <- dplyr::bind_rows(Effsizes_T1, Effsizes_t)



################################################################################-
## T2 ----

### Regression ----

outcome.of.interest <- "decentr"
timepoint.of.interest <- "T2"
baseline <- "T0"

model <- with(SWELL_MI_rds,
              exp = stats::lm(scale(decentr_T2) ~ decentr_T0 + as.factor(employer_2fact) + arm)) 

model_summary <- summary(pool(model))

model_summary

#                      term     estimate  std.error   statistic        df     p.value
#1                (Intercept) -1.40794767 0.33434986 -4.2110013  90.57140 0.00005987458
#2                 decentr_T0  0.04803339 0.01139171  4.2165212  83.15502 0.00006277053
#3 as.factor(employer_2fact)1 -0.01285731 0.09937595 -0.1293805 115.80156 0.89728103615
#4                       arm1 -0.15161370 0.08950689 -1.6938774  76.95184 0.09433281807

#pre-post
d <- 4.2165212*sqrt(1/241+1/241)

################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/decentr_T02_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary$statistic[4]*adj*-1, 
                         p.value = model_summary$p.value[4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)


Effsizes_T2 <- dplyr::bind_rows(Effsizes_T2, Effsizes_t)




################################################################################-
# MAAS ----
################################################################################-



################################################################################-
## T1 ----

### Regression ----

outcome.of.interest <- "MAAS"
timepoint.of.interest <- "T1"
baseline <- "T0"

model <- with(SWELL_MI_rds,
              exp = stats::lm(scale(MAAS_T1) ~ MAAS_T0 + as.factor(employer_2fact) + arm)) 

model_summary <- summary(pool(model))

model_summary

#                        term    estimate  std.error  statistic       df          p.value
#1                (Intercept) -1.67328613 0.31577013 -5.2990639 112.2890 0.0000005872816
#2                    MAAS_T0  0.44756521 0.08794447  5.0891799 105.0946 0.0000015835446
#3 as.factor(employer_2fact)1 -0.11467127 0.08990109 -1.2755269 153.3947 0.2040510706236
#4                       arm1  0.01434478 0.07812907  0.1836036 105.5494 0.8546766547798

#pre-post
d <- 5.0891799*sqrt(1/241+1/241)

################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/MAAS_T01_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary$statistic[4]*adj*1, 
                         p.value = model_summary$p.value[4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)

Effsizes_T1 <- dplyr::bind_rows(Effsizes_T1, Effsizes_t)



################################################################################-
## T2 ----

### Regression ----

outcome.of.interest <- "MAAS"
timepoint.of.interest <- "T2"
baseline <- "T0"

model <- with(SWELL_MI_rds,
              exp = stats::lm(scale(MAAS_T2) ~ MAAS_T0 + as.factor(employer_2fact) + arm)) 

model_summary <- summary(pool(model))

model_summary

#                      term     estimate  std.error   statistic        df     p.value
#1                (Intercept) -1.38840760 0.33677950 -4.1226012 102.63058 0.00007615498
#2                    MAAS_T0  0.38077106 0.09478230  4.0173224  93.80170 0.00011867876
#3 as.factor(employer_2fact)1 -0.04245457 0.10660237 -0.3982517 103.34667 0.69126620686
#4                       arm1 -0.01482839 0.08942266 -0.1658237  81.45485 0.86870661587
#

#pre-post
d <- 4.0173224*sqrt(1/241+1/241)

################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/MAAS_T02_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary$statistic[4]*adj*-1, 
                         p.value = model_summary$p.value[4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)


Effsizes_T2 <- dplyr::bind_rows(Effsizes_T2, Effsizes_t)




################################################################################-
# Job importance ----
################################################################################-



################################################################################-
## T1 ----

### Regression ----

outcome.of.interest <- "jobImp"
timepoint.of.interest <- "T1"
baseline <- "T0"

model <- with(SWELL_MI_rds,
              exp = stats::lm(scale(jobImp_T1) ~ jobImp_T0 + as.factor(employer_2fact) + arm)) 

model_summary <- summary(pool(model))

model_summary

#                        term    estimate  std.error  statistic       df          p.value
#1                (Intercept) -1.9960477 0.21088577 -9.465066 117.7726 0.0000000000000004440892
#2                  jobImp_T0  0.8131120 0.08414939  9.662719 108.9325 0.0000000000000002220446
#3 as.factor(employer_2fact)1 -0.1301489 0.08362942 -1.556258 115.3915 0.1223852950818529183863
#4                       arm1  0.0706970 0.06395283  1.105455 117.0861 0.2712286248710655822691



################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/jobImp_T01_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary$statistic[4]*adj*-1, 
                         p.value = model_summary$p.value[4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)

Effsizes_T1 <- dplyr::bind_rows(Effsizes_T1, Effsizes_t)



################################################################################-
## T2 ----

### Regression ----

outcome.of.interest <- "jobImp"
timepoint.of.interest <- "T2"
baseline <- "T0"

model <- with(SWELL_MI_rds,
              exp = stats::lm(scale(jobImp_T2) ~ jobImp_T0 + as.factor(employer_2fact) + arm)) 

model_summary <- summary(pool(model))

model_summary

#                      term     estimate  std.error   statistic        df     p.value
#1                (Intercept) -1.53073028 0.28756269 -5.3231185  72.15433 0.0000011022958
#2                  jobImp_T0  0.64735208 0.11414872  5.6711285  68.59214 0.0000003096702
#3 as.factor(employer_2fact)1 -0.01244482 0.09691798 -0.1284057 104.87986 0.8980738053715
#4                       arm1  0.05005776 0.08603396  0.5818373  74.04487 0.562443603978



################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/jobImp_T02_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary$statistic[4]*adj*-1, 
                         p.value = model_summary$p.value[4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)


Effsizes_T2 <- dplyr::bind_rows(Effsizes_T2, Effsizes_t)




################################################################################-
# Overtime ----
################################################################################-

#This did not get imputed so using the unimputed data
#source(here::here("./Code/Analysis/Questionnaires/SWELL_3c_Qdescriptives.R"))


SWELL_demographics_ITT <- read.csv(here::here("./Data/Redcap/cleaned-data/SWELL_demographics_ITT.csv")) |>
  dplyr::select(c(record_id, employer_2fact))

SWELL_unimp_ot <- read.csv(here::here("./Data/Redcap/processed-data/SWELL_T012.outcomes_ITT.csv")) |>
  dplyr::select(c(record_id, arm, dplyr::starts_with("wh.diff_"))) |>
  dplyr::left_join(SWELL_demographics_ITT)



################################################################################-
## T1 ----

### Regression ----

outcome.of.interest <- "wh.diff"
timepoint.of.interest <- "T1"
baseline <- "T0"

model <- stats::lm(wh.diff_T1 ~ wh.diff_T0 + as.factor(employer_2fact) + arm,
                   data = SWELL_unimp_ot) 

model_summary <- summary(model)

model_summary

#Coefficients:
#  Estimate Std. Error t value            Pr(>|t|)    
#(Intercept)                 0.70149    0.30406   2.307              0.0224 *  
#  wh.diff_T0                  0.80987    0.03374  24.003 <0.0000000000000002 ***
#  as.factor(employer_2fact)1  0.17135    0.26411   0.649              0.5175    
#arm1                       -0.35584    0.20962  -1.698              0.0917 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2.584 on 148 degrees of freedom
#(89 observations deleted due to missingness)
#Multiple R-squared:  0.7983,	Adjusted R-squared:  0.7942 
#F-statistic: 195.2 on 3 and 148 DF,  p-value: < 0.00000000000000022

################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/wh.diff_T01_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary[["coefficients"]][4,3]*adj*-1, 
                         p.value = model_summary[["coefficients"]][4,4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)

Effsizes_T1 <- dplyr::bind_rows(Effsizes_T1, Effsizes_t)



################################################################################-
## T2 ----

### Regression ----


outcome.of.interest <- "wh.diff"
timepoint.of.interest <- "T2"
baseline <- "T0"

model <- stats::lm(wh.diff_T2 ~ wh.diff_T0 + as.factor(employer_2fact) + arm,
                   data = SWELL_unimp_ot) 

model_summary <- summary(model)

model_summary


#Residuals:
#  Min       1Q   Median       3Q      Max 
#-11.6501  -1.1247  -0.3435   1.6787  12.3996 
#
#Coefficients:
#  Estimate Std. Error t value            Pr(>|t|)    
#(Intercept)                 0.27751    0.41010   0.677               0.500    
#wh.diff_T0                  0.73889    0.04772  15.484 <0.0000000000000002 ***
#  as.factor(employer_2fact)1 -0.08284    0.35529  -0.233               0.816    
#arm1                       -0.01684    0.27924  -0.060               0.952    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 3.141 on 123 degrees of freedom
#(114 observations deleted due to missingness)
#Multiple R-squared:  0.6657,	Adjusted R-squared:  0.6576 
#F-statistic: 81.66 on 3 and 123 DF,  p-value: < 0.00000000000000022

################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/wh.diff_T01_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary[["coefficients"]][4,3]*adj*1, 
                         p.value = model_summary[["coefficients"]][4,4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)


Effsizes_T2 <- dplyr::bind_rows(Effsizes_T2, Effsizes_t)






################################################################################-
# Exercise ----
################################################################################-

#This did not get imputed so using the unimputed data
#source(here::here("./Code/Analysis/Questionnaires/SWELL_3c_Qdescriptives.R"))


SWELL_demographics_ITT <- read.csv(here::here("./Data/Redcap/cleaned-data/SWELL_demographics_ITT.csv")) |>
  dplyr::select(c(record_id, employer_2fact))

SWELL_unimp_ex <- read.csv(here::here("./Data/Redcap/processed-data/SWELL_T012.outcomes_ITT.csv")) |>
  dplyr::select(c(record_id, arm, dplyr::starts_with("exercise."))) |>
  dplyr::left_join(SWELL_demographics_ITT)



################################################################################-
## T1 ----

### Regression ----

outcome.of.interest <- "exercise.total"
timepoint.of.interest <- "T1"
baseline <- "T0"

model <- stats::lm(exercise.total_T1 ~ exercise.total_T0 + as.factor(employer_2fact) + arm,
                   data = SWELL_unimp_ex) 

model_summary <- summary(model)

model_summary

#Call:
#  stats::lm(formula = exercise.total_T1 ~ exercise.total_T0 + as.factor(employer_2fact) + 
#              arm, data = SWELL_unimp_ex)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-524.88 -122.32  -34.28  119.50  849.00 
#
#Coefficients:
#  Estimate Std. Error t value             Pr(>|t|)    
#(Intercept)                135.80909   27.67356   4.908           0.00000245 ***
#  exercise.total_T0            0.79179    0.05316  14.893 < 0.0000000000000002 ***
#  as.factor(employer_2fact)1  11.03158   20.04557   0.550                0.583    
#arm1                         8.21397   15.85162   0.518                0.605    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 193.4 on 145 degrees of freedom
#(92 observations deleted due to missingness)
#Multiple R-squared:  0.6069,	Adjusted R-squared:  0.5988 
#F-statistic: 74.63 on 3 and 145 DF,  p-value: < 0.00000000000000022

################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/wh.diff_T01_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary[["coefficients"]][4,3]*adj*1, 
                         p.value = model_summary[["coefficients"]][4,4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)

Effsizes_T1 <- dplyr::bind_rows(Effsizes_T1, Effsizes_t)



################################################################################-
## T2 ----

### Regression ----


outcome.of.interest <- "exercise.total"
timepoint.of.interest <- "T2"
baseline <- "T0"

model <- stats::lm(exercise.total_T2 ~ exercise.total_T0 + as.factor(employer_2fact) + arm,
                   data = SWELL_unimp_ex) 

model_summary <- summary(model)

model_summary


#Call:
#  stats::lm(formula = exercise.total_T2 ~ exercise.total_T0 + as.factor(employer_2fact) + 
#              arm, data = SWELL_unimp_ex)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-669.80  -95.76  -14.64   99.38  605.97 
#
#Coefficients:
#  Estimate Std. Error t value             Pr(>|t|)    
#(Intercept)                195.36216   30.96516   6.309        0.00000000508 ***
#  exercise.total_T0            0.65519    0.06548  10.005 < 0.0000000000000002 ***
#  as.factor(employer_2fact)1  42.08848   22.59893   1.862                0.065 .  
#arm1                        -1.35453   17.96606  -0.075                0.940    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 198.3 on 118 degrees of freedom
#(119 observations deleted due to missingness)
#Multiple R-squared:  0.4684,	Adjusted R-squared:  0.4549 
#F-statistic: 34.66 on 3 and 118 DF,  p-value: 0.0000000000000003866

################################################################################-
## Export ----

saveRDS(model_summary, here::here("./Data/Tasks/4.output/wh.diff_T01_summary.rda"))

Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = model_summary[["coefficients"]][4,3]*adj*1, 
                         p.value = model_summary[["coefficients"]][4,4]) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)


Effsizes_T2 <- dplyr::bind_rows(Effsizes_T2, Effsizes_t)




################################################################################-
# Health problems ----
################################################################################-


################################################################################-
## T1 ----

outcome.of.interest <- "Health.status"
timepoint.of.interest <- "T1"
submeasure = "genhealth.problems"

#This did not get imputed so using the unimputed data
#source(here::here("./Code/Analysis/Questionnaires/SWELL_3c_Qdescriptives.R"))

SWELL_demographics_ITT <- read.csv(here::here("./Data/Redcap/cleaned-data/SWELL_demographics_ITT.csv")) |>
  dplyr::select(c(record_id, employer_2fact))

SWELL_unimp_hp <- read.csv(here::here("./Data/Redcap/processed-data/SWELL_T012.outcomes_ITT.csv")) |>
  dplyr::select(c(record_id, arm, dplyr::starts_with("genhealth.problems_"))) |>
  dplyr::left_join(SWELL_demographics_ITT) |>
  dplyr::mutate(employer_2fact = dplyr::case_when(employer_2fact == "Other" ~ 0,
                                                  employer_2fact == "Local Authority" ~ 1,
                                                  TRUE ~ NA_real_),
                genhealth.problems_T0 = dplyr::case_when(genhealth.problems_T0 == "No problems" ~ 0,
                                                         genhealth.problems_T0 == "Some problems" ~ 1,
                                                         TRUE ~ NA_real_),
                genhealth.problems_T1 = dplyr::case_when(genhealth.problems_T1 == "No problems" ~ 0,
                                                         genhealth.problems_T1 == "Some problems" ~ 1,
                                                         TRUE ~ NA_real_))



### GLM ----
model <-  stats::glm(genhealth.problems_T1 ~ genhealth.problems_T0 + employer_2fact + arm,
                     data = SWELL_unimp_hp,
                     family = "binomial")

model_summary <- summary(model)
#model_summary

#Call:
#  stats::glm(formula = genhealth.problems_T1 ~ genhealth.problems_T0 + 
#               employer_2fact + arm, family = "binomial", data = SWELL_unimp_hp)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.3896  -0.6868  -0.4910  -0.3502   2.3758  
#
#Coefficients:
#                      Estimate Std. Error z value  Pr(>|z|)    
#(Intercept)            -2.3955     0.6145  -3.898 0.0000969 ***
#genhealth.problems_T0   1.8103     0.5281   3.428  0.000609 ***
#employer_2fact          0.7059     0.6268   1.126  0.260099    
#arm1                   -0.3653     0.2292  -1.594  0.110887    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 146.00  on 146  degrees of freedom
#Residual deviance: 132.02  on 143  degrees of freedom
#(94 observations deleted due to missingness)
#AIC: 140.02
#
#Number of Fisher Scoring iterations: 4



### Export ----
orci <- as.data.frame(exp(cbind(OR = coef(model), confint(model))))

# create a df to export it
hp_summary1 <- data.frame(outcome = outcome.of.interest,
                          timepoint = timepoint.of.interest,
                          p = 0.110887,
                          or = orci$OR[4],
                          ci95.lo = orci$`2.5 %`[4],
                          ci95.hi = orci$`97.5 %`[4]) |>
  tidyr::pivot_wider(names_from = timepoint, values_from = c(p:ci95.hi))


# calculate effect sizes and merge with other effect sizes
Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = effectsize::oddsratio_to_d(orci$OR[4])*-1, 
                         or.bwgroups = orci$OR[4],
                         ci95.lo = orci$`2.5 %`[4],
                         ci95.hi = orci$`97.5 %`[4],
                         p.value = 0.110887,
                         submeasure = submeasure) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "or.bwgroups"] <- paste0("or.bwgroups_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "ci95.lo"] <- paste0("ci95.lo_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "ci95.hi"] <- paste0("ci95.hi_", timepoint.of.interest)


Effsizes_T1 <- dplyr::bind_rows(Effsizes_T1, Effsizes_t)


################################################################################-
## T2 ----

outcome.of.interest <- "Health.status"
timepoint.of.interest <- "T2"
submeasure = "genhealth.problems"


SWELL_demographics_ITT <- read.csv(here::here("./Data/Redcap/cleaned-data/SWELL_demographics_ITT.csv")) |>
  dplyr::select(c(record_id, employer_2fact))

SWELL_unimp_hp <- read.csv(here::here("./Data/Redcap/processed-data/SWELL_T012.outcomes_ITT.csv")) |>
  dplyr::select(c(record_id, arm, dplyr::starts_with("genhealth.problems_"))) |>
  dplyr::left_join(SWELL_demographics_ITT) |>
  dplyr::mutate(employer_2fact = dplyr::case_when(employer_2fact == "Other" ~ 0,
                                                  employer_2fact == "Local Authority" ~ 1,
                                                  TRUE ~ NA_real_),
                genhealth.problems_T0 = dplyr::case_when(genhealth.problems_T0 == "No problems" ~ 0,
                                                         genhealth.problems_T0 == "Some problems" ~ 1,
                                                         TRUE ~ NA_real_),
                genhealth.problems_T2 = dplyr::case_when(genhealth.problems_T2 == "No problems" ~ 0,
                                                         genhealth.problems_T2 == "Some problems" ~ 1,
                                                         TRUE ~ NA_real_))



### GLM ----
model <-  stats::glm(genhealth.problems_T2 ~ genhealth.problems_T0 + employer_2fact + arm,
                     data = SWELL_unimp_hp,
                     family = "binomial")

model_summary <- summary(model)
model_summary

#Call:
#  stats::glm(formula = genhealth.problems_T2 ~ genhealth.problems_T0 + 
#               employer_2fact + arm, family = "binomial", data = SWELL_unimp_hp)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.3544  -0.6551  -0.5490  -0.3749   2.3197  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)            -2.4269     0.6408  -3.787 0.000152 ***
#genhealth.problems_T0   1.8367     0.5153   3.565 0.000364 ***
#mployer_2fact           0.8039     0.6465   1.243 0.213695    
#arm1                    0.1933     0.2357   0.820 0.412283    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 129.47  on 122  degrees of freedom
#Residual deviance: 115.09  on 119  degrees of freedom
#(118 observations deleted due to missingness)
#AIC: 123.09
#
#Number of Fisher Scoring iterations: 4

orci <- as.data.frame(exp(cbind(OR = coef(model), confint(model))))



### Export ----

# create a df to export it
hp_summary2 <- data.frame(outcome = outcome.of.interest,
                          timepoint = timepoint.of.interest,
                          p = 0.412283,
                          or = orci$OR[4],
                          ci95.lo = orci$`2.5 %`[4],
                          ci95.hi = orci$`97.5 %`[4]) |>
  tidyr::pivot_wider(names_from = timepoint, values_from = c(p:ci95.hi))


# calculate effect sizes and merge with other effect sizes
Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = effectsize::oddsratio_to_d(orci$OR[4])*-1, 
                         or.bwgroups = orci$OR[4],
                         ci95.lo = orci$`2.5 %`[4],
                         ci95.hi = orci$`97.5 %`[4],
                         p.value = 0.412283,
                         submeasure = submeasure) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "or.bwgroups"] <- paste0("or.bwgroups_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "ci95.lo"] <- paste0("ci95.lo_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "ci95.hi"] <- paste0("ci95.hi_", timepoint.of.interest)


Effsizes_T2 <- dplyr::bind_rows(Effsizes_T2, Effsizes_t)



### Export results ----
hp_summary <- dplyr::left_join(hp_summary1, hp_summary2)

write.csv(hp_summary, here::here("./Data/RedCap/outputs/SWELL_HealthProblems_T12.csv"), row.names = FALSE)

################################################################################-
# WSAS ----
################################################################################-


################################################################################-
## T1 ----
outcome.of.interest <- "WSAS"
timepoint.of.interest <- "T1"

SWELL_MI_rds <- readRDS(here::here("./Data/MultipleImputation/Output/SWELL_forMI_T012_notcompleted.rds"))

model <- mice::complete(SWELL_MI_rds, action = "long", include = FALSE) |> 
  dplyr::mutate(arm = dplyr::case_when(arm == 1 ~ arm1,
                                      arm == 2 ~ arm2,
                                      TRUE ~ NA_character_), #this should not happen but is here for safeguarding
                arm = as.factor(arm),
                arm = factor(arm, levels = c(arm1, arm2, NA_character_)),
                employer_2fact = dplyr::case_when(employer_anon.factor == "Local Authority 1" ~ 1,
                                                  employer_anon.factor == "Local Authority 2" ~ 1,
                                                  employer_anon.factor == "Local Authority 3" ~ 1,
                                                  #is.na(employer_anon.factor) ~ NA_character_,
                                                  TRUE ~ 0),
                WSAS_T0bin = dplyr::case_when(WSAS_T0 == 0 ~ 0,
                                              TRUE ~ 1),
                WSAS_T1bin = dplyr::case_when(WSAS_T1 == 0 ~ 0,
                                              TRUE ~ 1),
                WSAS_T2bin = dplyr::case_when(WSAS_T2 == 0 ~ 0,
                                              TRUE ~ 1)) |>
  group_by(.imp) |>
  do(model = stats::glm(WSAS_T1bin ~ WSAS_T0bin + employer_2fact + arm,
                        data = .,
                        family = "binomial")) |>
  as.list() %>%
  .[[-1]] 


#model without exponantiation to get chi square
model_summary <- summary(pool(model))
#model_summary 

#term   estimate std.error  statistic        df     p.value
#1    (Intercept) -1.6261856 0.5150870 -3.1571088 121.44000 0.002010107
#2     WSAS_T0bin  1.2692064 0.4855712  2.6138419 114.19458 0.010159341
#3 employer_2fact  0.3695935 0.5330466  0.6933606 127.20762 0.489347339
#4           arm1  0.3028876 0.2219120  1.3649000  89.17607 0.175718379

chisq <- model_summary$statistic[4]^2
p  <- model_summary$p.value[4]
df <- model_summary$df[4]

#model with exponentiation to get OR (estimate) and corresponding CIs.
model_summary <- summary(pool(model), conf.int = TRUE, exponentiate = TRUE)
model_summary 

#term  estimate std.error  statistic        df     p.value      2.5 %   97.5 %
#  1    (Intercept) 0.1966784 0.5150870 -3.1571088 121.44000 0.002010107 0.07094156 0.545271
#2     WSAS_T0bin 3.5580279 0.4855712  2.6138419 114.19458 0.010159341 1.35976397 9.310118
#3 employer_2fact 1.4471463 0.5330466  0.6933606 127.20762 0.489347339 0.50399296 4.155281
#4           arm1 1.3537624 0.2219120  1.3649000  89.17607 0.175718379 0.87106845 2.103936


or  <- model_summary$estimate[4]
ci95.lo <- model_summary$`2.5 %`[4]
ci95.hi <- model_summary$`97.5 %`[4]


# create a df to export it
hp_summary1 <- data.frame(outcome = outcome.of.interest,
                          timepoint = timepoint.of.interest,
                          chisq = chisq,
                          p = p,
                          df = df,
                          or = model_summary$statistic[4],
                          ci95.lo = ci95.lo,
                          ci95.hi = ci95.hi) |>
  tidyr::pivot_wider(names_from = timepoint, values_from = c(chisq:ci95.hi))


# calculate effect sizes and merge with other effect sizes


Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = effectsize::oddsratio_to_d(or[1])*1, 
                         or.bwgroups = or,
                         ci95.lo = ci95.lo,
                         ci95.hi = ci95.hi,
                         p.value = p) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "or.bwgroups"] <- paste0("or.bwgroups_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "ci95.lo"] <- paste0("ci95.lo_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "ci95.hi"] <- paste0("ci95.hi_", timepoint.of.interest)


Effsizes_T1 <- dplyr::bind_rows(Effsizes_T1, Effsizes_t)




################################################################################-
## T2 ----
outcome.of.interest <- "WSAS"
timepoint.of.interest <- "T2"

SWELL_MI_rds <- readRDS(here::here("./Data/MultipleImputation/Output/SWELL_forMI_T012_notcompleted.rds"))

model <- mice::complete(SWELL_MI_rds, action = "long", include = FALSE) |> 
  dplyr::mutate(arm = dplyr::case_when(arm == 1 ~ arm1,
                                       arm == 2 ~ arm2,
                                       TRUE ~ NA_character_), #this should not happen but is here for safeguarding
                arm = as.factor(arm),
                arm = factor(arm, levels = c(arm1, arm2, NA_character_)),
                employer_2fact = dplyr::case_when(employer_anon.factor == "Local Authority 1" ~ 1,
                                                  employer_anon.factor == "Local Authority 2" ~ 1,
                                                  employer_anon.factor == "Local Authority 3" ~ 1,
                                                  #is.na(employer_anon.factor) ~ NA_character_,
                                                  TRUE ~ 0),
                WSAS_T0bin = dplyr::case_when(WSAS_T0 == 0 ~ 0,
                                              TRUE ~ 1),
                WSAS_T1bin = dplyr::case_when(WSAS_T1 == 0 ~ 0,
                                              TRUE ~ 1),
                WSAS_T2bin = dplyr::case_when(WSAS_T2 == 0 ~ 0,
                                              TRUE ~ 1)) |>
  group_by(.imp) |>
  do(model = stats::glm(WSAS_T2bin ~ WSAS_T0bin + employer_2fact + arm,
                        data = .,
                        family = "binomial")) |>
  as.list() %>%
  .[[-1]] 


#model without exponantiation to get chi square
model_summary <- summary(pool(model))
model_summary 

#term     estimate std.error   statistic        df     p.value
#1    (Intercept) -1.524960296 0.5266451 -2.89561279 105.09228 0.004603814
#2     WSAS_T0bin  1.219033700 0.4398661  2.77137476 141.23766 0.006334324
#3 employer_2fact  0.582286197 0.5436109  1.07114522 110.65362 0.286434896
#4           arm1 -0.003082212 0.2045677 -0.01506695  91.13938 0.988011700

chisq <- model_summary$statistic[4]^2
p  <- model_summary$p.value[4]
df <- model_summary$df[4]

#model with exponentiation to get OR (estimate) and corresponding CIs.
model_summary <- summary(pool(model), conf.int = TRUE, exponentiate = TRUE)
model_summary 

#term  estimate std.error   statistic        df     p.value     2.5 %    97.5 %
#  1    (Intercept) 0.2176297 0.5266451 -2.89561279 105.09228 0.004603814 0.0765976 0.6183312
#2     WSAS_T0bin 3.3839163 0.4398661  2.77137476 141.23766 0.006334324 1.4183033 8.0736535
#3 employer_2fact 1.7901263 0.5436109  1.07114522 110.65362 0.286434896 0.6096000 5.2568112
#4           arm1 0.9969225 0.2045677 -0.01506695  91.13938 0.988011700 0.6640336 1.4966931


or  <- model_summary$estimate[4]
ci95.lo <- model_summary$`2.5 %`[4]
ci95.hi <- model_summary$`97.5 %`[4]


# create a df to export it
hp_summary2 <- data.frame(outcome = outcome.of.interest,
                          timepoint = timepoint.of.interest,
                          chisq = chisq,
                          p = p,
                          df = df,
                          or = model_summary$statistic[4],
                          ci95.lo = ci95.lo,
                          ci95.hi = ci95.hi) |>
  tidyr::pivot_wider(names_from = timepoint, values_from = c(chisq:ci95.hi))


# calculate effect sizes and merge with other effect sizes


Effsizes_t <- data.frame(measure = outcome.of.interest, 
                         d.bwgroups = effectsize::oddsratio_to_d(or[1])*-1, 
                         or.bwgroups = or,
                         ci95.lo = ci95.lo,
                         ci95.hi = ci95.hi,
                         p.value = p) 

names(Effsizes_t)[names(Effsizes_t) == "p.value"] <- paste0("p.value_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "d.bwgroups"] <- paste0("d.bwgroups_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "or.bwgroups"] <- paste0("or.bwgroups_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "ci95.lo"] <- paste0("ci95.lo_", timepoint.of.interest)
names(Effsizes_t)[names(Effsizes_t) == "ci95.hi"] <- paste0("ci95.hi_", timepoint.of.interest)

Effsizes_T2 <- dplyr::bind_rows(Effsizes_T2, Effsizes_t)



### Export results ----
hp_summary <- dplyr::left_join(hp_summary1, hp_summary2)

write.csv(hp_summary, here::here("./Data/RedCap/outputs/SWELL_WSAS_T12.csv"), row.names = FALSE)


################################################################################-
# Export + cleanup ----
################################################################################-


Effsizes <- dplyr::left_join(Effsizes_T1, Effsizes_T2, by = c("measure", "submeasure")) |>
  dplyr::relocate(submeasure, .after = measure)

# add task effect sizes
Effsizes_tasks <- read.csv(here::here("./Data/RedCap/outputs/SWELL_effectsizes_tasks.csv"))

# combine everything

Effsizes <- dplyr::bind_rows(Effsizes, Effsizes_tasks)


write.csv(Effsizes, here::here("./Data/RedCap/outputs/SWELL_QT012_effectsizes_all.csv"), row.names = FALSE)

rm(list = ls())
