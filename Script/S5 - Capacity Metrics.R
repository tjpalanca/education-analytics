# Data Analytics for Education
# S5 - Capacity Metrics

# July 10, 2015

# Libraries ---------------------------------------------------------------
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(extrafont)
loadfonts(quiet = T)

# Data --------------------------------------------------------------------
load("Data/D4 - Schools Data Expanded.RData")
load("Data/D1 - Enrollment Data.RData")

# Metric Construction ------------------------------------------------------

enrolment.dt <- enrolment.dt %>% group_by(school.id, year) %>%
  summarise(enrolment = sum(enrollment)) %>% ungroup() %>%
  dcast(school.id ~ year) %>%
  rename(enrollment.2012 = `2012`,
         enrollment.2013 = `2013`,
         enrollment.2014 = `2014`,
         enrollment.2015 = `2015`)

schools.dt <- schools.dt %>% left_join(enrolment.dt)
rm(enrolment.dt)

schools.dt <- schools.dt %>% mutate(
  all.teacher.ratio =
    (teachers.instructor + teachers.mobile + teachers.regular + teachers.sped)/
    enrollment.2014,
  regular.teacher.ratio =
    (teachers.regular + teachers.sped) / enrollment.2014,
  academic.room.ratio =
    (rooms.standard.academic + rooms.nonstandard.academic) / enrollment.2013,
  standard.room.ratio =
    (rooms.standard.academic) / enrollment.2013,
  full.room.ratio =
    (rooms.standard.academic + rooms.standard.unused +
     rooms.nonstandard.academic + rooms.nonstandard.unused) / enrollment.2013,
  mooe.ratio = school.mooe / enrollment.2015
)

# Standardization of schools ------------------------------------------------
# Makes sure that only schools with complete capacity metrics are included in the analysis
schools.dt <- schools.dt %>%
  filter(is.na(all.teacher.ratio) + is.na(regular.teacher.ratio) +
           is.na(academic.room.ratio) + is.na(standard.room.ratio) +
           is.na(full.room.ratio) + is.na(mooe.ratio) == 0,
         is.infinite(all.teacher.ratio) + is.infinite(regular.teacher.ratio) +
           is.infinite(academic.room.ratio) + is.infinite(standard.room.ratio) +
           is.infinite(full.room.ratio) + is.infinite(mooe.ratio) == 0)

# Exploratory Plots ----------------------------------------------------------

histtheme.tm <-
  theme_minimal() +
  theme(text = element_text(family = "Open Sans"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold"))

grid.arrange(
  ggplot(schools.dt, aes(x = 1/all.teacher.ratio)) +
    geom_histogram(binwidth = 1, color = NA, fill = "darkgreen") +
    geom_rug() + xlab("Student to Teacher Ratio") +
    scale_x_reverse() +
    # coord_cartesian(xlim = c(0,100)) +
    histtheme.tm,
  ggplot(schools.dt, aes(x = 1/regular.teacher.ratio)) +
    geom_histogram(binwidth = 1, color = NA, fill = "darkgreen") +
    geom_rug() + xlab("Regular Teachers to Student Ratio") +
    scale_x_reverse() +
    # coord_cartesian(xlim = c(0,100)) +
    histtheme.tm,
  ggplot(schools.dt, aes(x = mooe.ratio)) +
    geom_histogram(binwidth = 20,  color = NA, fill = "darkblue") +
    geom_rug() + xlab("MOOE to Student Ratio") +
    # coord_cartesian(xlim = c(0,2000)) +
    histtheme.tm,
  ggplot(schools.dt, aes(x = 1/academic.room.ratio)) +
    geom_histogram(binwidth = 1, color = NA, fill = "darkorange") +
    scale_x_reverse() +
    geom_rug() + xlab("Student to Academic Room Ratio") +
    # coord_cartesian(xlim = c(0,100)) +
    histtheme.tm,
  ggplot(schools.dt, aes(x = 1/standard.room.ratio)) +
    geom_histogram(binwidth = 1, color = NA, fill = "darkorange") +
    scale_x_reverse() +
    geom_rug() + xlab("Student to Standard Room Ratio") +
    # coord_cartesian(xlim = c(0,100)) +
    histtheme.tm,
  ggplot(schools.dt, aes(x = 1/full.room.ratio)) +
    geom_histogram(binwidth = 1, color = NA, fill = "darkorange") +
    scale_x_reverse() +
    geom_rug() + xlab("Student to All Rooms Ratio") +
    # coord_cartesian(xlim = c(0,100)) +
    histtheme.tm,
  ncol = 3, main = "Capacity Metrics\n")

# Many positive outliers, logarithmic transformation will unskew the distributions
# (except MOOE per student which is still positively skewed).






# To Do:
# 4. Compute for capacity metrics and cluster the schools
# 5. Check for the upper limit on capacity metrics using correlates