################################################################################-
#
# SWELL trial Vainre et al 2024 BMJ Mental Health
# Global formulae and parameters
# Written by: Maris Vainre, 2023
#
################################################################################-



################################################################################-
# Parameters ----
################################################################################-
improvement.is.reduction = c("RT", #Learning Task 
                             "go.pmiss", "go.RT", "ssfailProb", "SSD", "ssfailRT", "SSRT", #Stop Signal Task
                             "GAD7", "PHQ9", "PSS")

roundto <- 2 # number of digits to round to


################################################################################-
# Labels ----
################################################################################-

arm1 <- "MBP" 
arm2 <- "LE" 

Arm1.l <- "Mindfulness" 
Arm2.l <- "Light exercise" 

arm1.l <- "mindfulness" 
arm2.l <- "light exercise" 


timepoint0 <- "T0"
timepoint1 <- "T1"
timepoint2 <- "T2"


timepoint0.l <- "Baseline"
timepoint1.l <- "Post-intervention"
timepoint2.l <- "Follow-up"

cond.labs <- c("Negative images", "Neutral images")
names(cond.labs) <- c("neg", "neu")


################################################################################-
# Formulae ----
################################################################################-

calculate_d <- function(m1, m2, sd_pooled){ #same as SMD
  d <- (m1-m2)/sd_pooled
  return(d)
}  


calculate_sd_pooled <- function(m1, sd1, n1, m2, sd2, n2) {
  #formula from: https://training.cochrane.org/handbook/current/chapter-06#section-6-5-2-10
  sd <- sqrt(((n1-1)*sd1^2+(n2-1)*sd2^2+n1*n2/(n1 + n2)*(m1^2+m2^2-2*m1*m2))/(n1 + n2 - 1))
  return(sd)
}

adj <- sqrt(1/122+1/119)

calculate_d_ttest <- function(t, n1, n2, df) {
  d <- t(n1 + n2)/sqrt(n1*n2)/df
}

