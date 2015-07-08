# Data Analytics for Education
# S2 - Survival Rate
# June 30, 2015

# Libraries ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(reshape2)
library(extrafont)
loadfonts()
library(scales)
library(stringr)
library(gridExtra)

# Data --------------------------------------------------------------------
load("Data/D1 - Enrollment Data.RData")
load("Data/D2 - Schools Data.RData")
load("Data/D3 - CityMuni Data.RData")

# National Level Cohort -------------------------------------------------------

# Make sure that the schools are conistent. Schools with incomplete data are ignored.
eligibleschools.dt <- enrolment.dt %>% group_by(school.id) %>% summarise(count = n()) %>%
  mutate(secondary = school.id > 300000) %>%
  mutate(complete = (secondary == F & count == 56) | (secondary == T & count == 32)) %>%
  filter(complete == T)

survival.dt <- enrolment.dt[enrolment.dt$school.id %in% eligibleschools.dt$school.id, ] %>%
  group_by(grade, gender, year) %>% summarise(enrollment = sum(enrollment)) %>% ungroup() %>%
  mutate(cohort = year - as.numeric(grade) - 2001 + 1) %>%
  group_by(gender, cohort) %>% arrange(year) %>%
  mutate(dropout = enrollment/lag(enrollment) - 1) %>% ungroup() %>% filter(!is.na(dropout))

rm(eligibleschools.dt)

ggplot(survival.dt %>% filter(grade != "Kinder"), aes(x = grade, y = enrollment, group = cohort)) +
  facet_wrap(~gender) +
  geom_path() +
  theme_bw()

survival_append.dt <- data.frame(grade = rep("Grade 1", 6),
                                 year = rep(seq(2013,2015), 2),
                                 gender = rep(c("Female", "Male"),3),
                                 mean.dropout = rep(0, 6),
                                 min.dropout = rep(0, 6),
                                 max.dropout = rep(0, 6),
                                 mean.survival = rep(1, 6),
                                 min.survival = rep(1, 6),
                                 max.survival = rep(1, 6))

survival.year.dt <- survival.dt %>%  filter(grade != "Grade 1") %>% group_by(grade, gender, year) %>%
  summarise(mean.dropout = mean(dropout), mean.survival = 1 + mean.dropout,
            min.dropout = min(dropout), max.dropout = max(dropout),
            min.survival = 1 + max.dropout, max.survival = 1 + min.dropout) %>%
  rbind(survival_append.dt) %>%
  group_by(gender, year) %>% arrange(grade) %>%
  mutate(mean.cum.survival = cumprod(mean.survival),
         max.cum.survival = cumprod(max.survival),
         min.cum.survival = cumprod(min.survival)) %>% ungroup()

rm(survival_append.dt)

# Plot: Survival Rates over Time ------------------------------------------

time.cumulative.gg <-
  ggplot(survival.year.dt, aes(x = str_replace_all(str_wrap(grade,8)," /",""),
                             y = mean.cum.survival, color = as.factor(year), group = year)) +
  facet_wrap(~gender) +
  geom_path() +
  geom_text(data = survival.year.dt %>% filter(as.numeric(grade) == max(as.numeric(grade))),
            aes(label = year), vjust = 1, family = "Open Sans", size = 4) +
  scale_y_continuous(name = "Cumulative Survival Rate (% of Cohort)", labels = percent,
                     limits = c(0,1)) +
  scale_x_discrete(name = "Grade Level") +
  ggtitle("And Then There Were None\n") +
  theme_bw() +
  theme(text = element_text(family = "Open Sans"),
        legend.position = "none",
        axis.title = element_text(face = "bold", size = 12),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        plot.margin = unit(c(10,10,0,6), "points"),
        panel.margin = unit(0, "points"),
        plot.background = element_rect(fill = "gray98", color = NA),
        plot.title = element_text(face = "bold", family = "Raleway", size = 18, hjust = -0.1))

time.hazard.gg <-
  ggplot(survival.year.dt, aes(x = str_replace_all(str_wrap(grade,8)," /",""),
                             y = mean.dropout, color = as.factor(year), group = year,
                             fill = as.factor(year))) +
  facet_wrap(~gender) +
  geom_path() +
  geom_text(data = survival.year.dt %>% filter(as.numeric(grade) == max(as.numeric(grade))),
            aes(label = year), vjust = 1, family = "Open Sans", size = 4) +
  scale_y_continuous(name = "Dropout Rate (%)", labels = percent) +
  scale_x_discrete(name = "Grade Level") +
  theme_bw() +
  theme(text = element_text(family = "Open Sans"),
        legend.position = "none",
        axis.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 90),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.margin = unit(c(0,10,10,10), "points"),
        panel.margin = unit(0, "points"),
        plot.background = element_rect(fill = "gray98", color = NA),
        plot.title = element_text(face = "bold", family = "Raleway", size = 18, hjust = -0.1))

svg("Output/O1 - Survival Rates by Year.svg", width = 7, height = 7)
grid.arrange(time.cumulative.gg, time.hazard.gg, ncol = 1, heights = c(0.6,0.4))
dev.off()

# Plot: Survival Rates per Gender -----------------------------------------

gender.cumulative.gg <-
  ggplot(survival.year.dt, aes(x = str_replace_all(str_wrap(grade,8)," /",""),
                             y = mean.cum.survival, color = gender, group = gender)) +
  facet_wrap(~year) +
  geom_path() +
  geom_text(data = survival.year.dt %>% filter(as.numeric(grade) == max(as.numeric(grade))),
            aes(label = str_sub(gender,1,1)), vjust = 1, family = "Open Sans", size = 6) +
  scale_y_continuous(name = "Cumulative Survival Rate (% of Cohort)", labels = percent,
                     limits = c(0,1)) +
  ggtitle("BATTLE OF THE SEXES\n") +
  theme_bw() +
  theme(text = element_text(family = "Open Sans"),
        legend.position = "none",
        axis.title = element_text(face = "bold", size = 12, family = "Raleway"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        plot.background = element_rect(fill = "gray98", color = NA),
        plot.title = element_text(face = "bold", family = "Raleway", size = 18, hjust = -0.1))

gender.hazard.gg <-
  ggplot(survival.year.dt, aes(x = str_replace_all(str_wrap(grade,8)," /",""),
                             y = mean.dropout, color = gender, group = gender,
                             fill = as.factor(year))) +
  facet_wrap(~year) +
  geom_path() +
  geom_text(data = survival.year.dt %>% filter(as.numeric(grade) == max(as.numeric(grade))),
            aes(label = str_sub(gender,1,1)), vjust = 1, family = "Open Sans", size = 6) +
  scale_y_continuous(name = "Dropout Rate (%)", labels = percent) +
  scale_x_discrete(name = "Grade Level") +
  theme_bw() +
  theme(text = element_text(family = "Open Sans"),
        legend.position = "none",
        axis.title = element_text(face = "bold", size = 12, family = "Raleway"),
        axis.text.x = element_text(angle = 90),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.background = element_rect(fill = "gray98", color = NA),
        plot.title = element_text(face = "bold", family = "Raleway", size = 18, hjust = -0.1))

grid.arrange(gender.cumulative.gg, gender.hazard.gg, ncol = 1, heights = c(0.6,0.4))

svg("Output/O2 - Survival Rates by Gender.svg", width = 9, height = 6)
grid.arrange(gender.cumulative.gg, gender.hazard.gg, ncol = 1, heights = c(0.6,0.4))
dev.off()


