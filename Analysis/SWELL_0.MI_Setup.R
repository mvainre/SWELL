################################################################################-
#
# SWELL trial Vainre et al 2024 BMJ Mental Health
# Set up a dataframe for multiple imputation
# Written by Maris Vainre, 2023
#
################################################################################-

# This script combines multiple data frames to get ready to run multiple imputation.

################################################################################-
# Set-up ----
################################################################################-

library(here)
library(tidyr)
library(dplyr)
library(stringr)
library(mice)
library(VIM)

SWELL_outcomes         <- read.csv(here::here("./Data/Redcap/processed-data/SWELL_T012.outcomes_ITT.csv"))
SWELL_arm              <- read.csv(here::here("./Data/Redcap/cleaned-data/SWELL_arm_ITT.csv"))
SWELL_demographics_ITT <- read.csv(here::here("./Data/Redcap/cleaned-data/SWELL_demographics_ITT.csv"))
SWELL_adh_LE           <- read.csv(here::here("./Data/Interventions/Control_LE/processed-data/SWELL.LE_imp.csv")) |>
  unique()

SWELL_adh_bm           <- read.csv(here::here("./Data/Interventions/Intervention_BeMindful/clean-data/BM_retention.csv")) |>
  select(c(record_id, startedCourse)) |>
  unique()

SWELL_eSST             <- read.csv(here::here("./Data/Tasks/3.processed-data/JATOS_StopSignalTask_eSST_imp.csv"))
SWELL_eLT              <- read.csv(here::here("./Data/Tasks/3.processed-data/JATOS_LearningTask_eLTcomp.csv")) |>
  dplyr::select(record_id, timepoint, condition, accuracy_cummean)
SWELL_DMmi             <- read.csv(here::here("./Data/RedCap/processed-data/SWELL_daily.MI.csv")) 




################################################################################-
# Demographics clean ----
################################################################################-

SWELL_demographics <- SWELL_demographics_ITT %>%
  dplyr::select(c(record_id, gender.factor, age, ethnicity.factor, occupation.factor, 
                  caring_resp.factor, ability_to_focus.factor, education, employer_2fact)) %>% #when using published data, replace with employer_2fact
  dplyr::mutate(gender.factor = as.factor(gender.factor),
                education = as.factor(education),
                ethnicity.factor = as.factor(ethnicity.factor),
                occupation.factor = as.factor(occupation.factor),
                caring_resp.factor = as.factor(caring_resp.factor),
                ability_to_focus.factor = as.factor(ability_to_focus.factor),
                employer_anon.factor = as.factor(employer_2fact)
                )

SWELL_demographics[SWELL_demographics == "Prefer not to answer"] <- NA

rm(SWELL_demographics_ITT)



################################################################################-
## Pivot task data.frames ----


### Stop-Signal Task ----

SWELL_eSST.w <- SWELL_eSST |>
  unique() |>
  filter(!is.na(record_id)) |>
  dplyr::mutate(t = case_when(timepoint == "Baseline" ~ "T0",
                              timepoint == "PostInt" ~ "T1",
                              timepoint == "FUP" ~ "T2",
                              TRUE ~ "unknown"),
                #t.em = paste(t, emotion_type, sep = "_")
                ) |>
  select(-timepoint) |>
  tidyr::pivot_wider(id_cols = c(record_id), names_from = c(emotion_type, t), values_from = SSRTint.mean, names_prefix = "SSRTint.mean_")


### Learning Task ----

SWELL_eLT.w <- SWELL_eLT |>
  dplyr::mutate(t = case_when(timepoint == "Baseline" ~ "T0",
                              timepoint == "PostInt" ~ "T1",
                              timepoint == "FUP" ~ "T2",
                              TRUE ~ "unknown")) |>
  select(-timepoint) |>
  tidyr::pivot_wider(id_cols = record_id, names_from = c(condition, t), values_from = accuracy_cummean, names_prefix = "LT.accurancy.cummean_") 




################################################################################-
# Create unified dataframe ----
################################################################################-

SWELL_unimp <- left_join(SWELL_arm, SWELL_demographics, by = c("record_id"))
SWELL_unimp <- left_join(SWELL_unimp, SWELL_outcomes, by = c("record_id"))
SWELL_unimp <- left_join(SWELL_unimp, SWELL_adh_LE, by = c("record_id"))

SWELL_unimp <- left_join(SWELL_unimp, SWELL_adh_bm, by = c("record_id")) |>
  dplyr::mutate(startedCourse = case_when(startedCourse.x == "yes" ~ "yes",
                                          startedCourse.y == "yes" ~ "yes",
                                          TRUE ~ "no")) |>
  dplyr::select(-c(startedCourse.x, startedCourse.y))

SWELL_unimp <- left_join(SWELL_unimp, SWELL_eSST.w, by = c("record_id"))
SWELL_unimp <- left_join(SWELL_unimp, SWELL_eLT.w, by = c("record_id"))


# check if variables have the correct class
str(SWELL_unimp)

write.csv(SWELL_unimp, here::here("./Data/MultipleImputation/Input/SWELL_unimp_T012.csv"), row.names = FALSE)




SWELL_unimp <- left_join(SWELL_unimp, SWELL_DMmi, by = c("record_id")) |>
  select(-wrfq.s_d29)




################################################################################-
# Pivot to long format ----
################################################################################-

SWELL_unimp_lf <- SWELL_unimp %>%
    dplyr::relocate(startedCourse, .after = employer_anon.factor) |>
    tidyr::pivot_longer(cols = c(WRFQ_T0:wrfq.s_d28),
                        names_to = "Questionnaire",
                        values_to = "Score") %>%
    dplyr::mutate(Q = stringr::str_split(Questionnaire, pattern = "\\_T", simplify = TRUE)[,1],
                  day = as.numeric(stringr::str_split(Questionnaire, pattern = "\\_T", simplify = TRUE)[,2]),
                  day = ifelse((Questionnaire == "epref" | Questionnaire == "progress"), 30, day),
                  #day1 = ifelse(day == 1, 30, day))
                  day = case_when(day == 1 ~ 30,
                                   day == 2 ~ 90,
                                   day == 0 ~ 0,
                                   TRUE ~ day),
                  day1 = as.numeric(stringr::str_split(Questionnaire, pattern = "\\_d", simplify = TRUE)[,2]),
                  day = ifelse(is.na(day), day1, day)) %>%
    dplyr::mutate(Q = stringr::str_split(Q, pattern = "\\_d", simplify = TRUE)[,1]) %>%
    dplyr::mutate(Questionnaire = as.factor(Q)) %>%
    dplyr::select(-c(day1, Q)) 





################################################################################-
# Export ----
################################################################################-

write.csv(SWELL_unimp_lf, here::here("./Data/MultipleImputation/SWELL_forMI_long.csv"), row.names = FALSE)




################################################################################-
# Explore missingness ----
################################################################################-

mice::md.pattern(SWELL_unimp, rotate.names = TRUE)

aggr_plot <- VIM::aggr(SWELL_unimp, col = c(swell_cols("green"), swell_cols("pink")), number = TRUE, sortVars=TRUE,
                  labels=names(SWELL_unimp), cex.axis=.6, gap=0, ylab=c("Histogram of missing data","Pattern"))
aggr_plot




################################################################################-
# Split arms ----
################################################################################-


SWELL_unimp1 <- SWELL_unimp %>%
  dplyr::filter(arm == 1)

SWELL_unimp1_lf <- SWELL_unimp_lf %>%
  filter(arm == 1)

droplevels(SWELL_unimp1$gender.factor, exclude = if(anyNA(levels(SWELL_unimp1$gender.factor))) NULL else NA)
str(SWELL_unimp1)  


SWELL_unimp2 <- SWELL_unimp %>%
  filter(arm == 2) 



#function to calculate proportion of missingness in a vector
prop_missings_vector <- function(x) {
  sum(is.na(x))/length(x)
}

#function to calculate missings for a whole dataframe and sort it
missings_df <- function(dataset) {
  dataset %>%
    apply(2, prop_missings_vector) %>%
    sort(decreasing = T)
}

missings_df(SWELL_unimp1)
missings_df(SWELL_unimp2)
