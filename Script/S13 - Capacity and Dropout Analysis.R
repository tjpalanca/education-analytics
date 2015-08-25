# Data Analytics for Education
# S13 - Capacity and Dropout Analysis
# August 24, 2015
# Author: Troy James R Palanca

# This script explores the dataset in terms of capacity and dropout.

# Libraries ---------------------------------------------------------------
library(dplyr)
library(reshape2)
library(beepr)
library(ggplot2)

# Data --------------------------------------------------------------------
load("Data/D5 - Capacity Data.RData")
load("Data/D1 - Enrollment Data.RData")

# Computation of School Level Dropout Rate --------------------------------

# Clean up enrollment file
eligibleschools.dt <- enrolment.dt %>% filter(enrollment != 0) %>%
  group_by(school.id) %>% summarise(count = n()) %>%
  mutate(secondary = school.id >= 300000) %>%
  mutate(complete = (secondary == F & count == 56) |
           (secondary == T & count == 32)) %>%
  filter(complete == T)
enrolment.dt <- enrolment.dt[enrolment.dt$school.id %in%
                               eligibleschools.dt$school.id, 1:5]
rm(eligibleschools.dt)

# Computation of school level dropout

CumulativeDropout <- function(x) tail(cumprod(c(1,1+x)),1)

enrolment.dt <- enrolment.dt %>%
  mutate(cohort = as.numeric(factor(paste(school.id, year - as.numeric(grade)))))

survival_elem.dt <- enrolment.dt %>%
  group_by(school.id, year, grade, cohort) %>%
  summarise(enrollment = sum(enrollment)) %>%
  filter(as.numeric(grade) %in% 2:7) %>%
  group_by(cohort) %>% arrange(year) %>%
  mutate(dropout = enrollment/lag(enrollment) - 1) %>%
  filter(!is.na(dropout) & !is.infinite(dropout)) %>%
  ungroup() %>% group_by(cohort, grade) %>%
  summarise(school.id = mean(school.id),
            mean.dropout = mean(dropout)) %>%
  ungroup() %>%
  select(-cohort) %>%
  group_by(school.id) %>%
  summarise(cum.dropout = CumulativeDropout(mean.dropout))

survival_seco.dt <- enrolment.dt %>%
  group_by(school.id, year, grade, cohort) %>%
  summarise(enrollment = sum(enrollment)) %>%
  filter(as.numeric(grade) %in% 8:11) %>%
  group_by(cohort) %>% arrange(year) %>%
  mutate(dropout = enrollment/lag(enrollment) - 1) %>%
  filter(!is.na(dropout) & !is.infinite(dropout)) %>%
  ungroup() %>% group_by(cohort, grade) %>%
  summarise(school.id = mean(school.id),
            mean.dropout = mean(dropout)) %>%
  ungroup() %>%
  select(-cohort) %>%
  group_by(school.id) %>%
  summarise(cum.dropout = CumulativeDropout(mean.dropout))

# Add to schools database
schools.dt <- schools.dt %>% left_join(rbind(survival_elem.dt, survival_seco.dt))

# Cleanup
rm(CumulativeDropout, enrolment.dt,
   survival_elem.dt, survival_seco.dt, survival.dt)

# Computation of Capacity Principal Component -----------------------------
capacity.pc <- princomp(scale(schools.dt %>%
                                select(all.teacher.ratio,
                                       full.room.ratio,
                                       mooe.ratio)))
summary(capacity.pc) # 68% of variance explained

schools.dt$capacity.pc <- capacity.pc$scores[,1] - min(capacity.pc$scores[,1]) + 1

rm(capacity.pc)

# Winsorization ----------------------------------------------------------------
Winsorize <- function(x, p = 0.05, upper = TRUE, lower = TRUE) {
  new <- x
  if (upper == TRUE) new[x >= quantile(x, 1-p, na.rm = T)] <-
      quantile(x, 1-p, na.rm = T)
  if (lower == TRUE) new[x <= quantile(x, p, na.rm = T)] <-
      quantile(x, p, na.rm = T)
  new
}

# Test proper winsorization
hist(Winsorize(log10(schools.dt$cum.dropout), p = 0.01))
schools.dt$cum.dropout_win <-
  Winsorize(log10(schools.dt$cum.dropout), p = 0.01)
hist(Winsorize(log10(schools.dt$capacity.pc), p = 0.025, upper = F))
schools.dt$capacity.pc_win <-
  Winsorize(log10(schools.dt$capacity.pc), p = 0.025, upper = F)


# Clustering --------------------------------------------------------------
set.seed(721992)
schools.kc <- kmeans(scale(schools.dt %>%
                             filter(!is.na(cum.dropout_win),
                                    !is.na(capacity.pc_win)) %>%
                             select(cum.dropout_win,
                                    capacity.pc_win)),
                     centers = 4)

schools_clustered.dt <- schools.dt %>%
  filter(!is.na(cum.dropout_win),
         !is.na(capacity.pc_win))

schools_clustered.dt$capdrop.cluster <- schools.kc$cluster

# Plotting ----------------------------------------------------------------

ggplot(schools_clustered.dt %>%
         filter(school.classification == "Elementary"),
       aes(x = capacity.pc_win, y = cum.dropout_win,
           color = as.factor(capdrop.cluster))) +
  geom_point(size = 1, alpha = 0.5)

beep(sound = 2)
