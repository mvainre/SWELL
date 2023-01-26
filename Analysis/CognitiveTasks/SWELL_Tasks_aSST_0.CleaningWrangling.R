################################################################################-
#
# SWELL trial. Affective Stop-Signal Task.
# Cleaning + Calculating SSRT
# Written by: Maris Vainre, 2022-2023
#
################################################################################-

# Following https://osf.io/dw4ke but as variables differ slightly, 
# this script is not an exact copy of the original script.
# The differences:
## The original script has factors for "nosignal" and "signal". 
## Here, nosignal = target; signal = stop_signal

## Also, original script does not take into account of different conditions
## (here negative (neg) and neutral (neu)) and 

## different time points (here: 3) and

## arms of the trial (not included in the wrangling stage) for blinding reasons

################################################################################-
# Setup ----
library(here)
library(dplyr)
library(ggplot2)
library(SWELLColour) # This just makes the graphs fancy. Can be ignored and commented out in plotting
library(hrbrthemes)  # This just makes the graphs fancy. Can be ignored and commented out in plotting

options(scipen = 999)

JATOS_id <- read.csv(here::here("./Data/Tasks/3.processed-data/JATOS_ID_T012.csv"))
aSST_T0.Baseline_raw <- read.csv(here::here("./Data/Tasks/1.converted-data/JATOS_StopSignalTask_T0.Baseline.csv"))
aSST_T1.PostInt_raw <- read.csv(here::here("./Data/Tasks/1.converted-data/JATOS_StopSignalTask_T1.PostInt.csv"))
aSST_T2.FUP_raw <- read.csv(here::here("./Data/Tasks/1.converted-data/JATOS_StopSignalTask_T2.FUP.csv"))



# merge dataframes from different time points
aSST_raw <- dplyr::bind_rows(aSST_T0.Baseline_raw, aSST_T1.PostInt_raw, aSST_T2.FUP_raw)




################################################################################-
# Data cleaning ----


## Set parameters for exclusion ----

# exclude RT that are below or above the pre-specified cut off. 
excludeRT_below <- 250 #in milliseconds
excludeRT_above <- 3000 #in milliseconds

# Stop-signal delay should now be below 25 ms (this is the step size for SSD). 
# Otherwise, the SSD is displayed before the target. 
# The script used for the trial did not have that cap (it should have had). 
# We need to remove trials where SSD falls below 50ms because the SS would not 
# have been visible below that Marc suggested this as anything shorter is priming.

excludeSSD_below <- 50 #in milliseconds



# Add record_id and remove pilot data ----

# not all IDs have a match, this is because some participants were not randomised
# and so they cannot have a record_id (pilot data!). 
# Other times, the the participants entered an incorrect SWELL_id to JATOS. 
# This is the only info that could be used to match the JATOS data to their record_id in RedCap. 
# For most cases, the mismatch was discovered early and fixed manually by the study team. 
# In a small number of cases, the match could not be identified and the participants 
# will thus be excluded from the analysis as we do not know their study arm.


aSST <- dplyr::left_join(aSST_raw, JATOS_id, by = "jatos_worker_ID") |>
  #remove pilot data (missing Pilot data does not mean they are Pilot)
  dplyr::filter(Pilot != "Yes" | is.na(Pilot)) |>
  dplyr::filter(jatos_worker_ID != 15800) |>   #Pilot
  dplyr::filter(jatos_worker_ID != 16096) |>   #Pilot 
  dplyr::filter(jatos_worker_ID != 16575) |>   #Pilot
  dplyr::filter(jatos_worker_ID != 16544)      #Pilot 


#write.csv(aSST, here::here("./Data/Tasks/1.converted-data/SWELL_StopSignalTask_T012.csv"), row.names = FALSE)
#aSST <- read.csv(here::here("./Data/Tasks/1.converted-data/SWELL_StopSignalTask_T012.csv"))


# Data quality check:  
# all the participants in the aSST dataframe now are true participants (wasRandomised == "Yes")
# Some of them did the task several times, later in the script only the first attempt will be
# included. For now all attempts are in.



################################################################################-
# Calculate outcome variables ----

aSST <- aSST %>%
  dplyr::select(-c(X, jatos_study_ID, trial, view_history, trial_type, Pilot, wasRandomised)) %>%
  
  # only pick rows that are relevant to the outcomes
  dplyr::filter(trial_segment == "target" | trial_segment == "stop_signal") %>%
  
  # exclude practice rounds and missing blocks, identify fixation points and 
  # the image that was shown before the go signal. These can also be excluded 
  # from the analysis
  dplyr::filter(block != "practice" & !is.na(block)) %>%
  
  # set some variables to use them later
  dplyr::mutate(trial_neg_number.c = 1,
                trial_neu_number.c = 1,
                acc.c  = ifelse(correct == TRUE, 1, 0), # numeric variable for accuacy
                miss.c = ifelse(trial_response == "x", 1, 0), # numeric variable for missing response
                resp.c = ifelse(trial_response != "x", 1, 0), # numeric variable for when response was given
                goCorr.c = case_when((trial_segment == "target" & correct == "TRUE") ~ 1, # numeric variable for correct go trials
                                     (trial_segment == "target" & correct == "FALSE") ~ 0,
                                     TRUE ~ NA_real_)) %>%
  dplyr::group_by(jatos_worker_ID, emotion_type) %>%
  
  # how many trials were there in total
  dplyr::mutate(trial_neg_number.c = ifelse(emotion_type == "neg", cumsum(trial_neg_number.c), NA),
                trial_neu_number.c = ifelse(emotion_type == "neu", cumsum(trial_neu_number.c), NA)) %>%
  ungroup() %>%
  
  #exclude impossibly low SSDs (see above for justification)
  dplyr::filter(SSD > excludeSSD_below | is.na(SSD))



## Calculate outcomes related to stop trials ----

# signal data: prespond, ssd, true SSRT, and signal-respond RT
aSST_stopsignal <- aSST |>
  dplyr::filter(trial_segment == "stop_signal") |>
  dplyr::group_by(jatos_worker_ID, emotion_type, timepoint) |>
  dplyr::summarise(ssfail.prob = sum(successful_inhibit)/n(), # calculate stop signal failure probability
                   presp.mean = mean(resp.c),                 # calculate response time probability mean
                   ssd.mean = mean(SSD))                      # calculate stop signal delay mean

aSST_stopsignal_t <- aSST |>
  dplyr::filter(trial_segment == "stop_signal") |>
  dplyr::filter(resp.c == 1) |>
  dplyr::group_by(jatos_worker_ID, emotion_type, timepoint) |>
  dplyr::summarise(ssfail.rt.mean = mean(rt, na.rm = TRUE))   # calculate the mean reaction time for failed stop signal tasks


aSST_stopsignal <- left_join(aSST_stopsignal, aSST_stopsignal_t, 
                             by = c("jatos_worker_ID", "emotion_type", "timepoint" ))
rm(aSST_stopsignal_t)



## Calculate outcome related to stop trials ----
aSST_target <- aSST |>
  dplyr::filter(trial_segment == "target") |>
  ## Exclude RTs outside of the pre-specified range
  filter((rt > excludeRT_below & rt < excludeRT_above) | is.na(rt))

aSST_target_sum_t1 <- aSST |>
  dplyr::filter(resp.c == 1) |>
  dplyr::group_by(jatos_worker_ID, emotion_type, timepoint) |>
  dplyr::summarise(go.max.rt.c = max(rt),                    # calculate max rt for go trials
                   go.resp.rt_mean = mean(rt),               # calculate mean rt for go responses
                   goRT.SD = sd(rt))                         # calculate sd for go rts

aSST_target_sum_t2 <- aSST_target |>
  dplyr::group_by(jatos_worker_ID, emotion_type, timepoint) |>
  dplyr::summarise(go.pmiss.c = 1 - mean(resp.c)) |>         # calculate the probability to miss a go trial
  dplyr::left_join(aSST_target_sum_t1, by = c("jatos_worker_ID", "emotion_type", "timepoint"))

rm(aSST_target_sum_t1)


aSST_target <- aSST_target |>
  dplyr::left_join(aSST_target_sum_t2, by = c("jatos_worker_ID", "emotion_type", "timepoint")) |>
  dplyr::group_by(jatos_worker_ID, emotion_type, timepoint) |>
  dplyr::mutate(goRT_adj.c = ifelse(resp.c == 0, go.max.rt.c, rt))  # adjust go reaction time so that only correct responses to go trials are included


# Create a summary dataframe
aSST_t1 <- dplyr::left_join(aSST_target, aSST_stopsignal, by = c("jatos_worker_ID", "emotion_type", "timepoint"))

aSST <- dplyr::left_join(aSST, aSST_t1, by = c("component", "jatos_worker_ID",  "rt",  "trial_index",  "time_elapsed",  
                                               "internal_node_id",  "jatos_component_ID",  "jatos_component_result_ID", 
                                               "jatos_study_result_ID",  "trial_segment",  "block",  "trial_absolute",  
                                               "trial_within",  "trial_go_number",   "emotion_type",  "arrow_direction",  
                                               "arrow_key_correct",  "trial_response_correct",  "correct",  "trial_response",  
                                               "trial_stop_number",  "SSD",  "successful_inhibit",  "timepoint",  "record_id",  
                                               "SWELL_id", "trial_neg_number.c",  "trial_neu_number.c", 
                                               "acc.c",  "miss.c",  "resp.c",  "goCorr.c"))
  

## estimate 1 ---- 
# all no-signal trials are INcluded when the nth RT is determined

aSST_t <- aSST |>
  dplyr::filter(trial_segment == "target") |>
  dplyr::group_by(jatos_worker_ID, emotion_type, timepoint) |>
  dplyr::mutate(nthRT.c = quantile(goRT_adj.c, probs = presp.mean, type = 6, na.rm = TRUE), ## determine nth RT
                SSRTint.c = nthRT.c - ssd.mean) ## SSRT(integration) = nthRT - ssd 



aSST <- dplyr::left_join(aSST, aSST_t, by = c("component", "jatos_worker_ID",  "rt",  "trial_index",   
                                              "time_elapsed", "internal_node_id",  "jatos_component_ID",  
                                              "jatos_component_result_ID", "jatos_study_result_ID",  
                                              "trial_segment",  "block",  "trial_absolute",  "trial_within",  
                                              "trial_go_number",
                                              "emotion_type",  "arrow_direction",  
                                              "arrow_key_correct",  "trial_response_correct",  "correct",  
                                              "trial_response", "trial_stop_number",  "SSD",  
                                              "successful_inhibit",  "timepoint",  "record_id",  "SWELL_id",  
                                              "trial_neg_number.c",  "trial_neu_number.c", 
                                              "acc.c", "miss.c", "resp.c", "goCorr.c", "go.pmiss.c",
                                              "go.max.rt.c","go.resp.rt_mean", "goRT.SD", "goRT_adj.c", "presp.mean",
                                              "ssd.mean","ssfail.rt.mean", "ssfail.prob")) |>
  dplyr::group_by(jatos_worker_ID, emotion_type, timepoint) |>
  # Calculate the difference with signal-respond RT
  dplyr::mutate(race.check.c = goRT_adj.c - ssfail.rt.mean) 



#  --- estimate 2 ---  estimate SSRT with the mean method
# DO NOT USE; included only for comparison purposes

#aSST_t2 <- aSST |>
#  dplyr::group_by(jatos_worker_ID, emotion_type, timepoint) |>
#  dplyr::mutate(mRT = mean(goRT_adj.c, na.rm = TRUE),
#                SSRTmean = mRT-ssd.mean)




################################################################################-
# Create a summary dataset ----

aSST_target_sum <- aSST_target |>
  dplyr::group_by(jatos_worker_ID, emotion_type, timepoint) |>
  dplyr::filter(resp.c == 1) |>
  dplyr::summarise(goRT_mean = mean(goRT_adj.c)) |>
  dplyr::left_join(aSST_target_sum_t2, by = c("jatos_worker_ID", "emotion_type", "timepoint"))

rm(aSST_target_sum_t2)

aSST_sum_t <- aSST |>
  filter(goCorr.c == 1) |>
  dplyr::group_by(jatos_worker_ID, emotion_type, timepoint) |>
  dplyr::summarise(go_RT_correct.mean = mean(goRT_adj.c),
                   go_RT_correct.sd   = sd(goRT_adj.c))

aSST_sum <- dplyr::full_join(aSST_target_sum, aSST_sum_t, by = c("jatos_worker_ID", "emotion_type", "timepoint"))
  

aSST_sum_t <- aSST |>
  dplyr::filter(trial_segment == "target") |> 
  dplyr::group_by(jatos_worker_ID, emotion_type, timepoint) |>
  dplyr::summarise(SSRTint.mean = mean(SSRTint.c, na.rm = FALSE))

aSST_sum <- dplyr::full_join(aSST_target_sum, aSST_sum_t, by = c("jatos_worker_ID", "emotion_type", "timepoint"))

aSST_sum_t <- dplyr::full_join(aSST_sum, aSST_stopsignal, by = c("jatos_worker_ID", "emotion_type", "timepoint"))

rm(aSST_sum)

################################################################################-
# Clean ----

aSST_attempt <- aSST |>
  #some people did the task twice. Only count the first full completion or if neither is complete, 
  #take the first half-completion.
  dplyr::group_by(record_id, emotion_type, timepoint) |>
  dplyr::select(record_id, jatos_worker_ID, emotion_type, timepoint, trial_absolute) |>
  dplyr::mutate(trial_absolute_max = max(trial_absolute),
                finished.c = ifelse(trial_absolute == trial_absolute_max, "yes", "no")) |>
  #dplyr::filter(finished.c == "yes") |>
  #if the finished it, count as the earlier jatos_worker_ID within the record_id and timepoint as the first attempt
  dplyr::mutate(finished_attempt = ifelse(finished.c == "yes",
                                          ifelse(jatos_worker_ID == min(jatos_worker_ID), "first", "subsequent"), "not finished")) |>
  dplyr::filter(finished.c == "yes") |>
  ungroup() |>
  dplyr::select(c(record_id, jatos_worker_ID, timepoint, finished_attempt)) 

aSST_sum <- left_join(aSST_sum_t, aSST_attempt, by = c("jatos_worker_ID", "timepoint")) |>
  dplyr::relocate(c(finished_attempt), .after = jatos_worker_ID) 

rm(aSST_attempt)


#Exclude participants whose probability of responding on stop-signal tasks
# was below 0.25 and above 0.75

aSST_sum_t <- aSST_sum |>
  dplyr::filter(ssfail.prob > 0.25 & ssfail.prob < 0.75)




################################################################################-
# Imputing data ----
aSST_imp <- aSST_sum_t |>
  ungroup() |>
  dplyr::filter(finished_attempt == "first" & !is.na(record_id)) |>
  dplyr::select(c(record_id, emotion_type, timepoint, SSRTint.mean)) |>
  unique()

  


################################################################################-
# Export data ----
write.csv(aSST_sum_t, here::here("./Data/Tasks/3.processed-data/JATOS_StopSignalTask_SSRTcomp.csv"), row.names = FALSE)

rm(aSST_t, aSST_sum, aSST_target_sum, aSST)

write.csv(aSST_imp, here::here("./Data/Tasks/3.processed-data/JATOS_StopSignalTask_aSST_imp.csv"), row.names = FALSE)