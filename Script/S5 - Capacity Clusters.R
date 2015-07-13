# Data Analytics for Education
# S5 - Capacity Clustering
# July 10, 2015

# Libraries ---------------------------------------------------------------
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)

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
schools.dt %>%


# Checking Plots ----------------------------------------------------------

grid.arrange(ggplot(schools.dt, aes(x = all.teacher.ratio)) +
  geom_histogram(binwidth = 0.005) +
  geom_rug() +
  coord_cartesian(xlim = c(0,0.1)),

ggplot(schools.dt, aes(x = regular.teacher.ratio)) +
  geom_histogram(binwidth = 0.005) +
  geom_rug() +
  coord_cartesian(xlim = c(0,0.1)),

ggplot(schools.dt, aes(x = academic.room.ratio)) +
  geom_histogram(binwidth = 0.005) +
  geom_rug() +
  coord_cartesian(xlim = c(0,0.075)),

ggplot(schools.dt, aes(x = standard.room.ratio)) +
  geom_histogram(binwidth = 0.005) +
  geom_rug() +
  coord_cartesian(xlim = c(0,0.075)),

ggplot(schools.dt, aes(x = full.room.ratio)) +
  geom_histogram(binwidth = 0.005) +
  geom_rug() +
  coord_cartesian(xlim = c(0,0.075)),

ggplot(schools.dt, aes(x = mooe.ratio)) +
  geom_histogram(binwidth = 100) +
  geom_rug() +
  coord_cartesian(xlim = c(0,2000)),

ncol = 2, main = "Capacity Metrics")

ggplot(schools.dt %>% select(school.id, regular.teacher.ratio, all.teacher.ratio) %>%
         melt(id.var = "school.id", variable.name = "metric", value.name = "value"),
       aes(x = metric, y = value)) +
  geom_violin()

ggplot(schools.dt %>% select(school.id, standard.room.ratio, academic.room.ratio,
                             full.room.ratio) %>%
         melt(id.var = "school.id", variable.name = "metric", value.name = "value"),
       aes(x = metric, y = value)) +
  geom_violin() +
  coord_cartesian(ylim = c(0,0.075))

ggplot(schools.dt %>% select(school.id, standard.room.ratio, academic.room.ratio,
                             full.room.ratio) %>%
         melt(id.var = "school.id", variable.name = "metric", value.name = "value"),
       aes(x = metric, y = value)) +
  geom_violin() +
  coord_cartesian(ylim = c(0,0.075))

ggplot(schools.dt %>% select(school.id, mooe.ratio) %>%
         melt(id.var = "school.id", variable.name = "metric", value.name = "value"),
       aes(x = metric, y = value)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,2000))

# To Do:
# 4. Compute for capacity metrics and cluster the schools
# 5. Check for the upper limit on capacity metrics using correlates