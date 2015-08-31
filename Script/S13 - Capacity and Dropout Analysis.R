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
library(extrafont)
loadfonts(quiet = T)
library(scales)
library(maptools)
library(rgeos)

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

# Elementary
schools_elem.dt <- schools.dt %>%
  filter(!is.na(cum.dropout_win), !is.na(capacity.pc_win),
         school.classification == "Elementary")

set.seed(19927292)
schools_elem.kc <- kmeans(scale(schools_elem.dt %>%
                                  select(cum.dropout_win,
                                         capacity.pc_win)),
                          centers = 4)

schools_elem.dt$clust <- schools_elem.kc$cluster
schools_elem.kc$unscaled.centers <- schools_elem.dt %>%
  group_by(clust) %>%
  summarise(cum.dropout_win = mean(cum.dropout_win),
            capacity.pc_win = mean(capacity.pc_win)) %>%
  mutate(name = c("Emptying Out",
                  "Pulling Ahead",
                  "Desolate",
                  "Filling Up"))

# Secondary

schools_seco.dt <- schools.dt %>%
  filter(!is.na(cum.dropout_win), !is.na(capacity.pc_win),
         school.classification == "Secondary")

schools_seco.kc <- kmeans(scale(schools_seco.dt %>%
                                  select(cum.dropout_win,
                                         capacity.pc_win)),
                          centers = 4)

schools_seco.dt$clust <- schools_seco.kc$cluster
schools_seco.kc$unscaled.centers <- schools_seco.dt %>%
  group_by(clust) %>%
  summarise(cum.dropout_win = mean(cum.dropout_win),
            capacity.pc_win = mean(capacity.pc_win)) %>%
  mutate(name = c("Pulling Ahead","Emptying Out","Desolate","Filling Up"))

# Plotting ----------------------------------------------------------------

ggplot(data = schools_elem.dt,
       aes(x = capacity.pc_win, y = cum.dropout_win)) +
  geom_point(aes(color = as.factor(clust)), size = 1, alpha = 0.5) +
  geom_text(data = schools_elem.kc$unscaled.centers,
            aes(label = name), family = "Open Sans") +
  scale_y_continuous(labels = percent) +
  ylab("Cumulative Dropout Rate\n(% of original cohort)") +
  xlab("Capacity Index") +
  theme_minimal(base_family = "Open Sans") +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 14)) +
  ggtitle("Elementary\n")

ggplot(data = schools_seco.dt,
       aes(x = capacity.pc_win, y = cum.dropout_win)) +
  geom_point(aes(color = as.factor(clust)), size = 1, alpha = 0.5) +
  geom_text(data = schools_seco.kc$unscaled.centers,
            aes(label = name), family = "Open Sans") +
  scale_y_continuous(labels = percent) +
  ylab("Cumulative Dropout Rate\n(% of original cohort)") +
  xlab("Capacity Index") +
  theme_minimal(base_family = "Open Sans") +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 14)) +
  ggtitle("Secondary\n")