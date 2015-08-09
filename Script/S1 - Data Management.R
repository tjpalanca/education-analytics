# Data Analytics for Education
# S1 - Data Management
# June 20, 2015
# Author: Troy James R Palanca

# Libraries ---------------------------------------------------------------
library(dplyr)
library(readxl)
library(reshape2)
library(ggplot2)

# Enrollment Data ---------------------------------------------------------

# Data --------------------------------------------------------------------
e2012.dt <- read_excel("Data/Enrollment Master Data.xlsx", 1) %>% mutate(year = 2012)
s2012.dt <- read_excel("Data/Enrollment Master Data.xlsx", 2) %>% mutate(year = 2012) %>%
  rename(`Year 1 / Grade 7 Male` = `Year 1 Male`,
         `Year 2 / Grade 8 Male` = `Year 2 Male`,
         `Year 3 / Grade 9 Male` = `Year 3 Male`,
         `Year 4 / Grade 10 Male` = `Year 4 Male`,
         `Year 1 / Grade 7 Female` = `Year 1 Female`,
         `Year 2 / Grade 8 Female` = `Year 2 Female`,
         `Year 3 / Grade 9 Female` = `Year 3 Female`,
         `Year 4 / Grade 10 Female` = `Year 4 Female`)
e2013.dt <- read_excel("Data/Enrollment Master Data.xlsx", 3) %>% mutate(year = 2013)
s2013.dt <- read_excel("Data/Enrollment Master Data.xlsx", 4) %>% mutate(year = 2013) %>%
  rename(`Year 1 / Grade 7 Male` = `Grade 7 Male`,
         `Year 2 / Grade 8 Male` = `Year 2 Male`,
         `Year 3 / Grade 9 Male` = `Year 3 Male`,
         `Year 4 / Grade 10 Male` = `Year 4 Male`,
         `Year 1 / Grade 7 Female` = `Grade 7 Female`,
         `Year 2 / Grade 8 Female` = `Year 2 Female`,
         `Year 3 / Grade 9 Female` = `Year 3 Female`,
         `Year 4 / Grade 10 Female` = `Year 4 Female`)
e2014.dt <- read_excel("Data/Enrollment Master Data.xlsx", 5) %>% mutate(year = 2014)
s2014.dt <- read_excel("Data/Enrollment Master Data.xlsx", 6) %>% mutate(year = 2014) %>%
  rename(`Year 1 / Grade 7 Male` = `Grade 7 Male`,
         `Year 2 / Grade 8 Male` = `Grade 8 Male`,
         `Year 3 / Grade 9 Male` = `Year 3 Male`,
         `Year 4 / Grade 10 Male` = `Year 4 Male`,
         `Year 1 / Grade 7 Female` = `Grade 7 Female`,
         `Year 2 / Grade 8 Female` = `Grade 8 Female`,
         `Year 3 / Grade 9 Female` = `Year 3 Female`,
         `Year 4 / Grade 10 Female` = `Year 4 Female`)
e2015.dt <- read_excel("Data/Enrollment Master Data.xlsx", 7)[,1:15] %>% mutate(year = 2015)
s2015.dt <- read_excel("Data/Enrollment Master Data.xlsx", 8)[,1:9] %>% mutate(year = 2015) %>%
  rename(`Year 1 / Grade 7 Male` = `Grade 7 Male`,
         `Year 2 / Grade 8 Male` = `Grade 8 Male`,
         `Year 3 / Grade 9 Male` = `Grade 9 Male`,
         `Year 4 / Grade 10 Male` = `Grade 10 Male`,
         `Year 1 / Grade 7 Female` = `Grade 7 Female`,
         `Year 2 / Grade 8 Female` = `Grade 8 Female`,
         `Year 3 / Grade 9 Female` = `Grade 9 Female`,
         `Year 4 / Grade 10 Female` = `Grade 10 Female`)

elementary.dt <- rbind(e2012.dt, e2013.dt, e2014.dt, e2015.dt) %>%
  rename(school.id = `School ID`)
secondary.dt <- rbind(s2012.dt, s2013.dt, s2014.dt, s2015.dt) %>%
  rename(school.id = `School ID`)
rm(e2012.dt, e2013.dt, e2014.dt, e2015.dt, s2012.dt, s2013.dt, s2014.dt, s2015.dt)

# Data Crunching ----------------------------------------------------------

elementary.dt <- elementary.dt %>% melt(id.vars = c("school.id", "year"),
                                        variable.name = "grade",
                                        value.name = "enrollment")
secondary.dt <- secondary.dt %>% melt(id.vars = c("school.id", "year"),
                                        variable.name = "grade",
                                        value.name = "enrollment")

elementary_bind.dt <-
  data.frame(do.call(rbind, strsplit(as.character(elementary.dt$grade), ' (?=[^ ]+$)',
                                     perl=TRUE)))
secondary_bind.dt <-
  data.frame(do.call(rbind, strsplit(as.character(secondary.dt$grade), ' (?=[^ ]+$)',
                                     perl=TRUE)))
names(elementary_bind.dt) <- names(secondary_bind.dt) <- c("grade", "gender")
elementary.dt <- cbind(elementary.dt, elementary_bind.dt)
secondary.dt <- cbind(secondary.dt, secondary_bind.dt)
elementary.dt$grade <- NULL
secondary.dt$grade <- NULL
rm(elementary_bind.dt, secondary_bind.dt)
enrolment.dt <- rbind(elementary.dt, secondary.dt)
rm(elementary.dt, secondary.dt)

enrolment.dt$grade <- factor(enrolment.dt$grade,
                              levels = c("Kinder", "Grade 1", "Grade 2", "Grade 3",
                                         "Grade 4", "Grade 5", "Grade 6", "Year 1 / Grade 7",
                                         "Year 2 / Grade 8", "Year 3 / Grade 9",
                                         "Year 4 / Grade 10"))

enrolment.dt <- enrolment.dt[complete.cases(enrolment.dt),]

# Write Out ---------------------------------------------------------------
save.image("Data/D1 - Enrollment Data.RData")
rm(enrolment.dt)

# School Data -------------------------------------------------------------

# Data --------------------------------------------------------------------

location.dt <- read.csv("Data/Schools Location Data.csv", stringsAsFactors = F) %>%
  select(school.id = School.ID, map.lat = Latitude, map.lon = Longitude)
location.dt$map.lat <- as.numeric(location.dt$map.lat)
location.dt$map.lon <- as.numeric(location.dt$map.lon)
location.dt <- location.dt[(complete.cases(location.dt) & location.dt$map.lat != 0) &
                             location.dt$map.lon != 0,]
schools.dt <- read_excel("Data/Masterlist of Schools.xlsx") %>% left_join(location.dt)
sum(is.na(schools.dt$map.lon))
rm(location.dt)
save.image("Data/D2 - Schools Data.RData")
rm(schools.dt)

# CityMuni Data -----------------------------------------------------------
citymuni.dt <- read.csv("Data/CityMuni.csv", stringsAsFactors = F)
save.image("Data/D3 - CityMuni Data.RData")

# Capacity Data -----------------------------------------------------------
load("Data/D2 - Schools Data.RData")
mooe.dt <- read_excel("Data/MOOE data.xlsx",2) %>%
  select(school.id, school.mooe)
rooms.dt <- read_excel("Data/Rooms data.xls", 1) %>%
  rename(school.id = `School ID`)
teachers.dt <- read_excel("Data/Teachers data.xls") %>%
  mutate(school.id = as.numeric(school.id))

schools_expanded.dt <- schools.dt %>% left_join(mooe.dt) %>%
  left_join(rooms.dt) %>% left_join(teachers.dt)

sum(is.na(schools_expanded.dt$school.mooe)) #2578 schools without MOOE data
nrow(mooe.dt) - nrow(schools.dt) #2575 less?

sum(is.na(schools_expanded.dt$rooms.standard.academic)) #246
nrow(rooms.dt) - nrow(schools.dt) #191 more?

sum(is.na(schools_expanded.dt$teachers.instructor)) #1563
nrow(teachers.dt) - nrow(schools.dt) #1563 less

schools.dt <- schools_expanded.dt

rm(schools_expanded.dt, mooe.dt, rooms.dt, teachers.dt)
save.image("Data/D4 - Schools Data Expanded.RData")

# End ---------------------------------------------------------------------
