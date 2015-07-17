# Data Analytics for Education
# S5 - Capacity Outlier Analysis and Clustering
# July 16, 2015

# Libraries ---------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
load("Data/D5 - Capacity Data.RData")

# Hierarchical Clustering -------------------------------------------------
capacity.hc <- hclust(dist(scale(schools.dt %>%
                                   select(all.teacher.ratio, full.room.ratio,
                                          mooe.ratio))), method = "ward.D")
