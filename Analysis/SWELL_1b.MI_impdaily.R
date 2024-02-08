################################################################################-
#
# SWELL trial Vainre et al 2024 BMJ Mental Health
# Run multiple imputation for daily WRFQ-5 data. 
# Written by Maris Vainre, 2023
# 
################################################################################-

# The multiple imputation for the daily WRFQ scores needs a different imputation model
# because the data are hierarchical. 
# This script generates 100 datasets (n_datasets) after 50 iterations for each (n_iteration) 
# based on seed = 13032023. 
# A separate collection of datasets is created for each arm.
# The datasets are then merged.
# This script will take a long time to run, so best to run it on a cluster computer.

# When running this on publicly available data, please note some variables should be
# changed as they have been altered in the public dataset to ensure participant anonymity:
# 1. employer_anon.factor is changed to employer_2fact

################################################################################-
# Set-up ----
################################################################################-

library(mice)
library(miceadds)
library(here)
library(dplyr)
seed = 13032023

SWELL_unimp <- read.csv(here::here("Data/MultipleImputation/SWELL_DMforMI_long.csv"))



################################################################################-
## Create MI parameters ----


## Number of iterations ----
#function to calculate proportion of missings in a vector
prop_missings_vector <- function(x) {
  sum(is.na(x))/length(x)
}

#function to calculate missings for a whole dataframe and sort it
missings_df <- function(dataset) {
  dataset %>%
    apply(2, prop_missings_vector) %>%
    sort(decreasing = T)
}

SWELL_missingness <- (missings_df(SWELL_unimp))

n_iterations <- 50 # change to 50
n_datasets <- 100 #round(max(SWELL_missingness)*100, digits = 0) 
#using missingness, the n_datasets would have been 80. Picking 100 as easier to write in paper




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
# Imputation model ----
################################################################################-



################################################################################-
## Arm 1

### Trial 3: Long format, using ID as clusters, day as random intercept and questionnaire as random slope.
# Followed mice.impute.2l.contextual.pmm example from miceadds package

# define predictor matrix
predM <- mice::make.predictorMatrix(data = SWELL_unimp1)


impMethod <- mice::make.method(data = SWELL_unimp1)
impMethod[c("record_id", "day", "dailyQ_date", "dailyQ_time")] <- ""

# Questionnaire LEVEL (Level 1)
# Use a random slope + random intercept model for Score.

impMethod[ c("wrfq_score") ] <- "2l.pmm"
predM[ c("wrfq_score"), c("arm", "gender.factor", "age", "ethnicity.factor", 
                     "occupation.factor", "caring_resp.factor", 
                     "ability_to_focus.factor", "education", 
                     "employer_anon.factor", "day") ] <- 2 # random slopes on 'Score' # Attention! change employer_anon.factor if needed
predM[, "record_id" ] <- -2



# Participant level (Level 2)
# impute "ethnicity.factor", "caring_resp.factor", "ability_to_focus.factor", 
# and "education" at the level of record_id (= participant)

impMethod[ c("ethnicity.factor", "caring_resp.factor", "ability_to_focus.factor", 
             "education", "course_start", "dailyQ_date",  "dailyQ_time") ] <- ""


#record_id is the clustering factor
predM[ c("ethnicity.factor", "caring_resp.factor", "ability_to_focus.factor", "education",
         "course_start", "dailyQ_date",  "dailyQ_time"), 
       "record_id"] <- -2

#random slopes + rand intercept for all 4, based on the other variables with no missingness
predM[ c("ethnicity.factor", "caring_resp.factor", "ability_to_focus.factor", "education"), 
       c("arm", "gender.factor", "age", "occupation.factor", "employer_anon.factor")] <- 2

# set this to 0, as it should be
predM["record_id", "record_id"] <- 0

# do imputation
imp1 <- mice::mice(SWELL_unimp1, 
                  predictorMatrix = predM, 
                  m = n_datasets, 
                  maxit = n_iterations,
                  method = impMethod, 
                  paniter = 1000) #change to 1000 
summary(imp1)

densityplot(imp1)


data_mi1 <- complete(imp1)



################################################################################-
## Arm 2 ----
### Trial 3: Long format, using ID as clusters, day as random intercept and questionnaire as random slope.
# Followed mice.impute.2l.contextual.pmm example from miceadds package

# define predictor matrix
predM <- mice::make.predictorMatrix(data = SWELL_unimp2)
predM[, "record_id" ] <- -2


impMethod <- mice::make.method(data = SWELL_unimp2)
impMethod[c("record_id", "day", "dailyQ_date", "dailyQ_time")] <- ""

# Questionnaire LEVEL (Level 1)
# Use a random slope + random intercept model for Score.

impMethod[ c("wrfq_score") ] <- "2l.pmm"
predM[ c("wrfq_score"), c("arm", "gender.factor", "age", "ethnicity.factor", 
                          "occupation.factor", "caring_resp.factor", 
                          "ability_to_focus.factor", "education", 
                          "employer_anon.factor", "day") ] <- 2 # random slopes on 'Score'
predM[, "record_id" ] <- -2



# Participant level (Level 2)
# impute "ethnicity.factor", "caring_resp.factor", "ability_to_focus.factor", 
# and "education" at the level of record_id (= participant)

impMethod[ c("ethnicity.factor", "caring_resp.factor", "ability_to_focus.factor", 
             "education", "course_start", "dailyQ_date",  "dailyQ_time") ] <- ""


#record_id is the clustering factor
predM[ c("ethnicity.factor", "caring_resp.factor", "ability_to_focus.factor", "education",
         "course_start", "dailyQ_date",  "dailyQ_time"), 
       "record_id"] <- -2

#random slopes + rand intercept for all 4, based on the other variables with no missingness
predM[ c("ethnicity.factor", "caring_resp.factor", "ability_to_focus.factor", "education"), 
       c("arm", "gender.factor", "age", "occupation.factor", "employer_anon.factor")] <- 2

# set this to 0, as it should be
predM["record_id", "record_id"] <- 0

# do imputation
imp2 <- mice::mice(SWELL_unimp2, 
                   predictorMatrix = predM, 
                   m = n_datasets, 
                   maxit = n_iterations,
                   method = impMethod, 
                   paniter = 1000)
summary(imp2)

densityplot(imp2)


data_mi1 <- complete(imp2)




################################################################################-
# Combine arms ----
################################################################################-

SWELL.imp_mids <- rbind(imp1, imp2)

t1 <- complete(imp1, action = "long", include = TRUE) 
t2 <- complete(imp2, action = "long", include = TRUE) 

SWELL_imp <- dplyr::bind_rows(t1, t2) 




################################################################################-
# Export ----
################################################################################-

saveRDS(SWELL.imp_mids, file  = here::here("Data/MultipleImputation/Output/SWELL_MI_DM_notcompleted.rds")) 


