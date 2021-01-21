##########################################################################################
                              ## Randomisation for the SWELL Trial ##
                              ##   Written by Maris Vainre, 2020   ##
##########################################################################################

# This script creates 6 files for randomisation: 
# For each of 3 sites (Site1, Site 2, Site3), it creates a development (trial) version, and a real one.


library(randomizeR)
# setwd() here

N <- 256 # integer for the total sample size of the trial. I've selected a much bigger number since that's what RedCap advices. It ensures I can also reallocate space from one site to another if I underrecruit in one of the sites.
rb <- c(2,4,6) #block lengths of the blocks that can be selected equiprobably at random
K <- 2 #number of groups
ratio <- rep(1, K) #vector of length K. The total sample number N and all used block lengths (bc)have to be divisible bysum(ratio)
groups <- c("1","2") #character vector of labels for the different treatments.


randProcedure <- rpbrPar(N,rb,K,ratio,groups,filledBlock = FALSE)

# Generate files for Site1
randSequenceSite1Dev <- genSeq(randProcedure, seed = 43431)
randSequenceSite1Dev <- getRandList(randSequenceSite1Dev)
RandTableSite1Dev <- as.data.frame(t(randSequenceSite1Dev))
names(RandTableSite1Dev)[names(RandTableSite1Dev) == "V1"] <- "group" # Changing the column name because that's the template RedCap requires
write.csv(RandTableSite1Dev, "RandTableSite1Dev.csv",
          row.names = FALSE)

randSequenceSite1Prod <- genSeq(randProcedure, seed = 94343)
randSequenceSite1Prod <- getRandList(randSequenceSite1Prod)
RandTableSite1Prod <- as.data.frame(t(randSequenceSite1Prod))
names(RandTableSite1Prod)[names(RandTableSite1Prod) == "V1"] <- "group" # Changing the column name because that's the template RedCap requires
write.csv(RandTableSite1Prod, 
          "RandTableSite1Prod.csv",
          row.names = FALSE)

# Generate files for Site2
randSequenceSite2Dev <- genSeq(randProcedure, seed = 17979)
randSequenceSite2Dev <- getRandList(randSequenceSite2Dev)
RandTableSite2Dev <- as.data.frame(t(randSequenceSite2Dev))
names(RandTableSite2Dev)[names(RandTableSite2Dev) == "V1"] <- "group" # Changing the column name because that's the template RedCap requires
write.csv(RandTableSite2Dev, 
          "C:/Users/maris/ownCloud.2/PhD/Trial/Documents/StudyMaterial/RedCap/Randomisation/RandTableSite2Dev.csv",
          row.names = FALSE)

randSequenceSite2Prod <- genSeq(randProcedure, seed = 97979)
randSequenceSite2Prod <- getRandList(randSequenceSite2Prod)
RandTableSite2Prod <- as.data.frame(t(randSequenceSite2Prod))
names(RandTableSite2Prod)[names(RandTableSite2Prod) == "V1"] <- "group" # Changing the column name because that's the template RedCap requires
write.csv(RandTableSite2Prod, 
          "C:/Users/maris/ownCloud.2/PhD/Trial/Documents/StudyMaterial/RedCap/Randomisation/RandTableSite2Prod.csv",
          row.names = FALSE)

# Generate files for Site3
randSequenceSite3Dev <- genSeq(randProcedure, seed = 15454)
randSequenceSite3Dev <- getRandList(randSequenceSite3Dev)
RandTableSite3Dev <- as.data.frame(t(randSequenceSite3Dev))
names(RandTableSite3Dev)[names(RandTableSite3Dev) == "V1"] <- "group" # Changing the column name because that's the template RedCap requires
write.csv(RandTableSite3Dev, 
          "C:/Users/maris/ownCloud.2/PhD/Trial/Documents/StudyMaterial/RedCap/Randomisation/RandTableSite3Dev.csv",
          row.names = FALSE)

randSequenceSite3Prod <- genSeq(randProcedure, seed = 95454)
randSequenceSite3Prod <- getRandList(randSequenceSite3Prod)
RandTableSite3Prod <- as.data.frame(t(randSequenceSite3Prod))
names(RandTableSite3Prod)[names(RandTableSite3Prod) == "V1"] <- "group" # Changing the column name because that's the template RedCap requires
write.csv(RandTableSite3Prod, 
          "Documents/StudyMaterial/RedCap/Randomisation/RandTableSite3Prod.csv",
          row.names = FALSE)



#Testing the randomisation
library(dplyr)

#Take the first 1 to a random number (b) of rows and count how many would end up in each arm
b = sample(2:160, 1) # a random number between 2 and 160

Test1 <- RandTableSite2Prod %>% #change table according to which one is to be tested
  slice(1:b) %>% # Take the first 1 to b rows
  group_by(group) %>%
  count()
b
Test1 #Didn't do proper modelling but in repeated reruns of the script (more than 30 times, the biggest difference I got is 2)
