################################################################################-
#
# SWELL trial Vainre et al 2024 BMJ Mental Health
# Questionnaire data
# Analysis: Data wrangling post MI
# Written by: Maris Vainre, 2023
#
################################################################################-




################################################################################-
# Set-up ----
################################################################################-


library(here)
library(dplyr)
library(mice)

SWELL_MI_rds <- readRDS(here::here("./Data/MultipleImputation/Output/SWELL_forMI_T012_notcompleted.rds"))


source(here::here("./Code/Analysis/SWELL_GlobalParameters.Formulae.R"))


################################################################################-
# Wrangle data ----
################################################################################-

# the data were imputed in one format but for the statistical testing, the data 
# need to be transfromed.



################################################################################-
## Full dataset ----

# 1. convert the mids to a data frame
SWELL_MI <- mice::complete(SWELL_MI_rds, action = "long", include = TRUE)

# 2. transform data

SWELL_MI_t <- list()

for(i in 0:max(SWELL_MI$.imp)) {
  SWELL_MI_t[[i+1]] <- SWELL_MI %>%
    subset(.imp == i)  %>%
    dplyr::mutate(arm = dplyr::case_when(arm == 1 ~ arm1,
                                         arm == 2 ~ arm2,
                                         TRUE ~ NA_character_), #this should not happen but is here for safeguarding
                  arm = as.factor(arm),
                  arm = factor(arm, levels = c(arm1, arm2, NA_character_)),
                  .id = 1:nrow(.),
                  employer_2fact = dplyr::case_when(employer_anon.factor == "Local Authority 1" ~ "Local Authority",
                                                    employer_anon.factor == "Local Authority 2" ~ "Local Authority",
                                                    employer_anon.factor == "Local Authority 3" ~ "Local Authority",
                                                    is.na(employer_anon.factor) ~ NA_character_,
                                                    TRUE ~ "Other"),
                  employer_2fact = as.factor(employer_2fact),
                  employer_2fact = factor(employer_2fact, levels = c("Other", "Local Authority", NA_character_)),
                  #employer_LA = dplyr::case_when(employer_2fact == "Local Authority" ~ 1,
                  #                               TRUE ~ 0),
                  WSAS_T0bin = dplyr::case_when(WSAS_T0 == 0 ~ 0,
                                                #is.na(WSAS_T0) ~ WSAS_T0,
                                                #WSAS_T0 == 0 ~ 0,
                                                TRUE ~ 1),
                  WSAS_T1bin = dplyr::case_when(WSAS_T1 == 0 ~ 0,
                                                #is.na(WSAS_T1) ~ WSAS_T1,
                                                #WSAS_T1 == 0 ~ 0,
                                                TRUE ~ 1),
                  #WSAS_T1bin = ifelse(WSAS_T1 == 0, 0,
                  #                    ifelse(WSAS_T1 >= 1, 1, WSAS_T1)),
                  WSAS_T2bin = dplyr::case_when(WSAS_T2 == 0 ~ 0,
                                                #WSAS_T2 == 0 ~ 0,
                                                TRUE ~ 1)#,
                  #WSAS_T1bin = as.factor(WSAS_T1bin),
                  #WSAS_T1bin = factor(WSAS_T1bin, levels = c("0", "1"))
                  )
}



# 3. convert back to mids
SWELL_MI_mids <- mice::as.mids(do.call(rbind, SWELL_MI_t))

saveRDS(SWELL_MI_mids, here::here("./Data/MultipleImputation/Output/SWELL_MI_Q.T012.rds"))

