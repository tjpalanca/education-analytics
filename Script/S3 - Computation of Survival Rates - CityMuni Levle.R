# Data Analytics for Education
# S3 - Computation of Survival Rates per City and Municipality
# July 7, 2015

# Libraries ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(reshape2)
library(extrafont)
loadfonts()
library(scales)
library(stringr)
library(gridExtra)
library(stringdist)

# Data --------------------------------------------------------------------
load("Data/D1 - Enrollment Data.RData")
load("Data/D2 - Schools Data.RData")
load("Data/D3 - CityMuni Data.RData")

# CityMuni Level Cohort ---------------------------------------------------

eligibleschools.dt <- enrolment.dt %>% group_by(school.id) %>% summarise(count = n()) %>%
  mutate(secondary = school.id > 300000) %>%
  mutate(complete = (secondary == F & count == 56) | (secondary == T & count == 32)) %>%
  filter(complete == T)

survival.dt <- enrolment.dt[enrolment.dt$school.id %in% eligibleschools.dt$school.id, ]  %>%
  filter (grade != "Kinder") %>%
  left_join(schools.dt[c("school.id", "school.citymuni", "map.lat", "map.lon")]) %>%
  group_by(school.citymuni, grade, gender, year) %>%
  summarise(enrollment = sum(enrollment), map.lat = mean(map.lat, na.rm = T),
            map.lon = mean(map.lon, na.rm = T)) %>% ungroup() %>%
  mutate(cohort = year - as.numeric(grade) - 2001 + 1) %>%
  group_by(school.citymuni, gender, cohort) %>% arrange(year) %>%
  mutate(dropout = enrollment/lag(enrollment) - 1) %>% ungroup() %>% filter(!is.na(dropout))

survival_append.dt <- data.frame(school.citymuni = rep(unique(survival.dt$school.citymuni), 6),
                                 grade = rep("Grade 1", 6*length(unique(survival.dt$school.citymuni))),
                                 year = rep(2013:2015, each = 2*length(unique(survival.dt$school.citymuni))),
                                 gender = rep(c("Female", "Male"),3*length(unique(survival.dt$school.citymuni))),
                                 mean.dropout = rep(0, 6),
                                 min.dropout = rep(0, 6),
                                 max.dropout = rep(0, 6),
                                 mean.survival = rep(1, 6),
                                 min.survival = rep(1, 6),
                                 max.survival = rep(1, 6))

survival.year.dt <- survival.dt %>%  filter(grade != "Grade 1") %>%
  group_by(school.citymuni, grade, gender, year) %>%
  summarise(mean.dropout = mean(dropout), mean.survival = 1 + mean.dropout,
            min.dropout = min(dropout), max.dropout = max(dropout),
            min.survival = 1 + max.dropout, max.survival = 1 + min.dropout) %>%
  rbind(survival_append.dt) %>%
  group_by(school.citymuni, gender, year) %>% arrange(grade) %>%
  mutate(mean.cum.survival = cumprod(mean.survival),
         max.cum.survival = cumprod(max.survival),
         min.cum.survival = cumprod(min.survival)) %>% ungroup() %>%
  left_join(schools.dt %>% group_by(school.citymuni) %>% summarise(map.lat = mean(map.lat, na.rm = T),
                                                                   map.lon = mean(map.lon, na.rm = T)))

rm(survival_append.dt)
