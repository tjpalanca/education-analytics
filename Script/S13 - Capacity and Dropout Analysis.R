# Data Analytics for Education
# S13 - Capacity and Dropout Analysis
# August 24, 2015
# Author: Troy James R Palanca

# This script explores the dataset in terms of capacity and dropout.

# Libraries ---------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
load("Data/D5 - Capacity Data.RData")


# Computation of School Level Dropout Rate --------------------------------

# Computation of Capacity Principal Component -----------------------------
capacity.pc <- princomp(scale(schools.dt %>%
                                select(all.teacher.ratio,
                                       full.room.ratio,
                                       mooe.ratio)))
summary(capacity.pc) # First principal component explains 68% of variance, but is negative so need to reverse.

schools.dt$capacity.pc1 <- -capacity.pc$scores[,1]



