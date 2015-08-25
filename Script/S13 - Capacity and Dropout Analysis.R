# Data Analytics for Education
# S13 - Capacity and Dropout Analysis
# August 24, 2015
# Author: Troy James R Palanca

# This script explores the dataset in terms of capacity and dropout.

# Libraries ---------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
load("Data/D5 - Capacity Data.RData")
load("Data/D1 - Enrollment Data.RData")

# Computation of School Level Dropout Rate --------------------------------

enrolment.dt <- enrolment.dt %>%
  mutate(cohort = as.numeric(factor(paste(school.id, year - as.numeric(grade)))))
survival_elem.dt <- enrolment.dt %>%
  filter(as.numeric(grade) %in% 2:7) %>%
  group_by(cohort, gender) %>% arrange(year) %>%
  mutate(dropout = enrollment/lag(enrollment) - 1) %>%
  ungroup() %>%
  filter(!is.na(dropout) & !is.infinite(dropout)) %>%
  summarise(
    mean.dropout = mean(dropout),
    se.dropout = sd(dropout)/sqrt(n()),
    ci99.upper.dropout = mean.dropout + se.dropout * qnorm(0.99),
    ci99.lower.dropout = mean.dropout - se.dropout * qnorm(0.99),
    ci95.upper.dropout = mean.dropout + se.dropout * qnorm(0.95),
    ci95.lower.dropout = mean.dropout - se.dropout * qnorm(0.95),
    ci90.upper.dropout = mean.dropout + se.dropout * qnorm(0.90),
    ci90.lower.dropout = mean.dropout - se.dropout * qnorm(0.90),
    ci80.upper.dropout = mean.dropout + se.dropout * qnorm(0.80),
    ci80.lower.dropout = mean.dropout - se.dropout * qnorm(0.80)) %>%
  ungroup()

survival_seco.dt <- enrolment.dt %>%
  filter(as.numeric(grade) %in% 8:11) %>%
  group_by(cohort, gender) %>% arrange(year) %>%
  mutate(dropout = enrollment/lag(enrollment) - 1) %>%
  filter(!is.na(dropout) & !is.infinite(dropout)) %>%
  left_join(schools_seco.dt %>% select(school.id, cluster)) %>%
  filter(!is.na(cluster)) %>%
  group_by(year, gender, grade, cluster) %>%
  summarise(
    mean.dropout = mean(dropout),
    se.dropout = sd(dropout)/sqrt(n()),
    ci99.upper.dropout = mean.dropout + se.dropout * qnorm(0.99),
    ci99.lower.dropout = mean.dropout - se.dropout * qnorm(0.99),
    ci95.upper.dropout = mean.dropout + se.dropout * qnorm(0.95),
    ci95.lower.dropout = mean.dropout - se.dropout * qnorm(0.95),
    ci90.upper.dropout = mean.dropout + se.dropout * qnorm(0.90),
    ci90.lower.dropout = mean.dropout - se.dropout * qnorm(0.90),
    ci80.upper.dropout = mean.dropout + se.dropout * qnorm(0.80),
    ci80.lower.dropout = mean.dropout - se.dropout * qnorm(0.80)) %>%
  ungroup()

# Generate initial rows for cumulative dropout rates
survival_elem_append.dt <-
  data.frame(grade = "Grade 1",
             year = rep(2013:2015, each = 12),
             gender = rep(c("Female", "Male"), 6*3),
             cluster = rep(rep(1:6, each = 2),3),
             mean.dropout = 0,
             se.dropout = 0,
             ci99.upper.dropout = 0,
             ci99.lower.dropout = 0,
             ci95.upper.dropout = 0,
             ci95.lower.dropout = 0,
             ci90.upper.dropout = 0,
             ci90.lower.dropout = 0,
             ci80.upper.dropout = 0,
             ci80.lower.dropout = 0,
             mean.survival = 1,
             ci99.upper.survival = 1,
             ci99.lower.survival = 1,
             ci95.upper.survival = 1,
             ci95.lower.survival = 1,
             ci90.upper.survival = 1,
             ci90.lower.survival = 1,
             ci80.upper.survival = 1,
             ci80.lower.survival = 1)
survival_seco_append.dt <-
  data.frame(grade = "Year 1 / Grade 7",
             year = rep(2013:2015, each = 12),
             gender = rep(c("Female", "Male"), 6*3),
             cluster = rep(rep(1:6, each = 2),3),
             mean.dropout = 0,
             se.dropout = 0,
             ci99.upper.dropout = 0,
             ci99.lower.dropout = 0,
             ci95.upper.dropout = 0,
             ci95.lower.dropout = 0,
             ci90.upper.dropout = 0,
             ci90.lower.dropout = 0,
             ci80.upper.dropout = 0,
             ci80.lower.dropout = 0,
             mean.survival = 1,
             ci99.upper.survival = 1,
             ci99.lower.survival = 1,
             ci95.upper.survival = 1,
             ci95.lower.survival = 1,
             ci90.upper.survival = 1,
             ci90.lower.survival = 1,
             ci80.upper.survival = 1,
             ci80.lower.survival = 1)

# Combine and compute cumulative dropout rates
survival_elementary.dt <- survival_elem.dt %>%
  mutate(mean.survival = 1 + mean.dropout,
         ci99.upper.survival = 1 + ci99.upper.dropout,
         ci99.lower.survival = 1 + ci99.lower.dropout,
         ci95.upper.survival = 1 + ci95.upper.dropout,
         ci95.lower.survival = 1 + ci95.lower.dropout,
         ci90.upper.survival = 1 + ci90.upper.dropout,
         ci90.lower.survival = 1 + ci90.lower.dropout,
         ci80.upper.survival = 1 + ci80.upper.dropout,
         ci80.lower.survival = 1 + ci80.lower.dropout) %>%
  rbind(survival_elem_append.dt) %>%
  arrange(year, cluster, gender, grade) %>%
  group_by(year, cluster, gender) %>%
  mutate(cum.mean.survival = cumprod(mean.survival),
         cum.ci99.upper.survival = cumprod(ci99.upper.survival),
         cum.ci99.lower.survival = cumprod(ci99.lower.survival),
         cum.ci95.upper.survival = cumprod(ci95.upper.survival),
         cum.ci95.lower.survival = cumprod(ci95.lower.survival),
         cum.ci90.upper.survival = cumprod(ci90.upper.survival),
         cum.ci90.lower.survival = cumprod(ci90.lower.survival),
         cum.ci80.upper.survival = cumprod(ci80.upper.survival),
         cum.ci80.lower.survival = cumprod(ci80.lower.survival)) %>%
  ungroup()

survival_secondary.dt <- survival_seco.dt %>%
  mutate(mean.survival = 1 + mean.dropout,
         ci99.upper.survival = 1 + ci99.upper.dropout,
         ci99.lower.survival = 1 + ci99.lower.dropout,
         ci95.upper.survival = 1 + ci95.upper.dropout,
         ci95.lower.survival = 1 + ci95.lower.dropout,
         ci90.upper.survival = 1 + ci90.upper.dropout,
         ci90.lower.survival = 1 + ci90.lower.dropout,
         ci80.upper.survival = 1 + ci80.upper.dropout,
         ci80.lower.survival = 1 + ci80.lower.dropout) %>%
  rbind(survival_seco_append.dt) %>%
  arrange(year, cluster, gender, grade) %>%
  group_by(year, cluster, gender) %>%
  mutate(cum.mean.survival = cumprod(mean.survival),
         cum.ci99.upper.survival = cumprod(ci99.upper.survival),
         cum.ci99.lower.survival = cumprod(ci99.lower.survival),
         cum.ci95.upper.survival = cumprod(ci95.upper.survival),
         cum.ci95.lower.survival = cumprod(ci95.lower.survival),
         cum.ci90.upper.survival = cumprod(ci90.upper.survival),
         cum.ci90.lower.survival = cumprod(ci90.lower.survival),
         cum.ci80.upper.survival = cumprod(ci80.upper.survival),
         cum.ci80.lower.survival = cumprod(ci80.lower.survival)) %>%
  ungroup()

# Clean up
rm(survival_elem.dt, survival_elem_append.dt,
   survival_seco.dt, survival_seco_append.dt)

# Computation of Capacity Principal Component -----------------------------
capacity.pc <- princomp(scale(schools.dt %>%
                                select(all.teacher.ratio,
                                       full.room.ratio,
                                       mooe.ratio)))
summary(capacity.pc) # First principal component explains 68% of variance, but is negative so need to reverse.

schools.dt$capacity.pc1 <- -capacity.pc$scores[,1]



