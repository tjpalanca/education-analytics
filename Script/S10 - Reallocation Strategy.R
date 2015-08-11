# Data Analytics for Education
# S10 - Reallocation Strategies
# August 10, 2015
# Author: Troy James R Palanca

# This script explores possible reallocation strategies to improve capacity within the current
# resource constraints.

# Libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
load("Data/D5 - Capacity Data.RData")

# Maximizing Room Use ------------------------------------------------------

# Distribution of change in ratio from using vacant rooms
ggplot(schools.dt %>%
         filter(!(full.room.ratio == academic.room.ratio)),
       aes(x = (1/academic.room.ratio - 1/full.room.ratio))) +
  geom_density(aes(fill = school.classification), color = NA, alpha = 0.5) +
  scale_x_log10()

# Distribution of change in ratio from using nonstandard rooms
ggplot(schools.dt %>%
         filter(!(standard.room.ratio == academic.room.ratio)),
       aes(x = (1/standard.room.ratio - 1/academic.room.ratio))) +
  geom_density(aes(fill = school.classification), color = NA, alpha = 0.5) +
  scale_x_log10()

# Elementary and secondary schools do not seem to exhibit difference,
# so they will be treated as a whole entity.

