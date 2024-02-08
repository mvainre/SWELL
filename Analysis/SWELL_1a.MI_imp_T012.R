################################################################################-
#
# SWELL trial Vainre et al 2024 BMJ Mental Health
# Run multiple imputation for all data, except daily WRFQ-5
# Written by Maris Vainre, 2023
#
################################################################################-

# This script generates 100 datasets (n_datasets) based on seed = 71122. 
# A separate collection of datasets is created for each arm.
# The datasets are then merged.
# This script will take a long time to run, so best to run it on a cluster computer.

################################################################################-
# Set-up ----
################################################################################-

library(mice)
library(here)
library(dplyr)
seed = 71122

SWELL_unimp <- read.csv(here::here("Data/MultipleImputation/Input/SWELL_unimp_T012.csv"))

## Create MI parameters ----
n_datasets <- 100 



################################################################################-
# Split datasets ----
################################################################################-

#Arm 1
SWELL_unimp1 <- SWELL_unimp %>%
  dplyr::filter(arm == 1)


#Arm 2
SWELL_unimp2 <- SWELL_unimp %>%
  dplyr::filter(arm == 2)




################################################################################-
# Impute ----
################################################################################-

# Single level mice

n_iterations <- 50



################################################################################-
## Arm 1 ----

# empty mice imputation
predM <- mice::make.predictorMatrix(data = SWELL_unimp1)
impMethod <- mice::make.method(data = SWELL_unimp1)

impMethod[c("ethnicity.factor", "education")] <- "polyreg"
impMethod[c("ability_to_focus.factor")] <- "logreg"
impMethod[c("SSRTint.mean_neg_T1", "SSRTint.mean_neu_T1", 
            "SSRTint.mean_neg_T2", "SSRTint.mean_neu_T2", 
            "LT.accurancy.cummean_neg_T0", "LT.accurancy.cummean_neu_T0",
            "LT.accurancy.cummean_neg_T1", "LT.accurancy.cummean_neu_T1",
            "LT.accurancy.cummean_neg_T2", "LT.accurancy.cummean_neu_T2")] <- "rf"

SWELL.imp1 <- mice(SWELL_unimp1, 
                   m = n_datasets, 
                   maxit = n_iterations,
                   seed = seed,
                   visitSequence = "monotone", #monotone orders from low to high proportion of missing data
                   pred = predM,
                   meth = impMethod)

summary(SWELL.imp1)
             
densityplot(SWELL.imp1)
plot(SWELL.imp1)
stripplot(SWELL.imp1)



################################################################################-
## Arm 2 ----

# empty mice imputation
predM <- mice::make.predictorMatrix(data = SWELL_unimp2)
impMethod <- mice::make.method(data = SWELL_unimp2)

impMethod[c("caring_resp.factor", "ability_to_focus.factor")] <- "logreg"
impMethod[c("SSRTint.mean_neg_T1", "SSRTint.mean_neu_T1", 
            "SSRTint.mean_neg_T2", "SSRTint.mean_neu_T2", 
            "LT.accurancy.cummean_neg_T0", "LT.accurancy.cummean_neu_T0",
            "LT.accurancy.cummean_neg_T1", "LT.accurancy.cummean_neu_T1",
            "LT.accurancy.cummean_neg_T2", "LT.accurancy.cummean_neu_T2")] <- "rf"



SWELL.imp2 <- mice(SWELL_unimp2, 
                   m = n_datasets, 
                   maxit = n_iterations,
                   seed = seed,
                   visitSequence = "monotone", #monotone orders from low to high proportion of missing data
                   pred = predM,
                   meth = impMethod)
summary(SWELL.imp2)

densityplot(SWELL.imp2)



################################################################################-
## Merge imputations together ----

SWELL.imp_mids <- rbind(SWELL.imp1, SWELL.imp2)

t1 <- complete(SWELL.imp1, action = "long", include = TRUE) 
t2 <- complete(SWELL.imp2, action = "long", include = TRUE) 

SWELL_imp <- dplyr::bind_rows(t1, t2) 




################################################################################-
# Export ----
################################################################################-

saveRDS(SWELL.imp_mids, file  = here::here("Data/MultipleImputation/Output/SWELL_forMI_T012_notcompleted.rds")) 

saveRDS(SWELL_imp, file  = here::here("Data/MultipleImputation/Output/SWELL_forMI_T012.rds")) 

write.csv(SWELL_imp, here::here("Data/MultipleImputation/Output/SWELL_MIcompleted_T012.csv"), row.names = FALSE)


