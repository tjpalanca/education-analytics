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
  mutate(complete = (secondary == F & count == 56) | (secondary == T & count == 32)) %>%
  filter(complete == T)
enrolment.dt <- enrolment.dt[enrolment.dt$school.id %in% eligibleschools.dt$school.id, 1:5]
rm(eligibleschools.dt)

# Computation of school level dropout

CumulativeDropout <- function(x) tail(cumprod(c(1,1+x)),1)

enrolment.dt <- enrolment.dt %>%
  mutate(cohort = as.numeric(factor(paste(school.id, year - as.numeric(grade)))))

survival_elem.dt <- enrolment.dt %>%
  filter(as.numeric(grade) %in% 2:7) %>%
  group_by(cohort, gender) %>% arrange(year) %>%
  mutate(dropout = enrollment/lag(enrollment) - 1) %>%
  filter(!is.na(dropout) & !is.infinite(dropout)) %>%
  ungroup() %>% group_by(cohort, gender, grade) %>%
  summarise(school.id = mean(school.id),
            mean.dropout = mean(dropout)) %>%
  ungroup() %>%
  select(-cohort) %>%
  dcast(school.id + grade ~ gender, value.var = "mean.dropout", fun.aggregate = mean) %>%
  ungroup() %>%
  group_by(school.id) %>%
  summarise(Female = CumulativeDropout(Female),
            Male = CumulativeDropout(Male))

survival_elem.pc <- princomp(scale(survival_elem.dt %>% select(Male, Female)))
summary(survival_elem.pc)

survival_seco.dt <- enrolment.dt %>%
  filter(as.numeric(grade) %in% 8:11) %>%
  group_by(cohort, gender) %>% arrange(year) %>%
  mutate(dropout = enrollment/lag(enrollment) - 1) %>%
  filter(!is.na(dropout) & !is.infinite(dropout)) %>%
  ungroup() %>% group_by(cohort, gender, grade) %>%
  summarise(school.id = mean(school.id),
            mean.dropout = mean(dropout)) %>%
  ungroup() %>%
  select(-cohort) %>%
  dcast(school.id + grade ~ gender, value.var = "mean.dropout", fun.aggregate = mean) %>%
  ungroup() %>%
  group_by(school.id) %>%
  summarise(Female = CumulativeDropout(Female),
            Male = CumulativeDropout(Male))

survival_seco.pc <- princomp(scale(survival_seco.dt %>% select(Male, Female)))
summary(survival_seco.pc)

# Principal components are adequately descriptive, so we include it in schools
survival.dt <- rbind(cbind(survival_elem.dt,
                           data.frame(dropout.pc = survival_elem.pc$scores[,1])),
                     cbind(survival_seco.dt,
                           data.frame(dropout.pc = survival_seco.pc$scores[,1])))
schools.dt <- schools.dt %>% left_join(survival.dt %>% select(school.id, dropout.pc))

# Cleanup
rm(CumulativeDropout, enrolment.dt, survival_elem.pc, survival_seco.pc,
   survival_elem.dt, survival_seco.dt, survival.dt)

# Computation of Capacity Principal Component -----------------------------
capacity.pc <- princomp(scale(schools.dt %>%
                                select(all.teacher.ratio,
                                       full.room.ratio,
                                       mooe.ratio)))
summary(capacity.pc) # 68% of variance explained

schools.dt$capacity.pc <- capacity.pc$scores[,1] - min(capacity.pc$scores[,1]) + 1

rm(capacity.pc)

# Plotting ----------------------------------------------------------------
hist(log10(schools.dt$capacity.pc))

ggplot(schools.dt %>% filter(school.classification == "Elementary"),
       aes(x = capacity.pc, y = dropout.pc)) +
  geom_point(size = 1, alpha = 0.05)
