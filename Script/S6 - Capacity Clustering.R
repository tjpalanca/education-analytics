# Data Analytics for Education
# S5 - Capacity Outlier Analysis and Clustering
# July 16, 2015

# Libraries ---------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
load("Data/D5 - Capacity Data.RData")
schools_elem.dt <- schools.dt %>% filter(school.classification == "Elementary")
schools_seco.dt <- schools.dt %>% filter(school.classification == "Secondary")
rm(schools.dt)

# Hierarchical Clustering -------------------------------------------------

set.seed(7292)
elem_capacity.kc <- kmeans(scale(schools_elem.dt %>% select(all.teacher.ratio, full.room.ratio, mooe.ratio)),
                             centers = round(nrow(schools_elem.dt)/3))
elem_capacity.hc <- hclust(dist(elem_capacity.kc$centers))
plot(elem_capacity.hc)
