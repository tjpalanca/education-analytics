# Data Analytics for Education
# S7 - Capacity Clustering - Execution
# July 19, 2015

# Libraries ---------------------------------------------------------------
library(dplyr)
library(rgl)
library(ggplot2)
library(reshape2)
library(extrafont)
loadfonts()

# Data --------------------------------------------------------------------
load("Data/D5 - Capacity Data.RData")
schools_elem.dt <- schools.dt %>% filter(school.classification == "Elementary")
schools_seco.dt <- schools.dt %>% filter(school.classification == "Secondary")
rm(schools.dt)

# Cluster Number Determination --------------------------------------------
source("Script/S8 - Clustergram (Hadley).r")
clustergram(many_kmeans(scale(schools_elem.dt %>%
                                select(all.teacher.ratio, full.room.ratio, mooe.ratio) %>%
                                mutate_each(funs(log10)))))ed
