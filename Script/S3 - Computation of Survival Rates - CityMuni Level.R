# Data Analytics for Education
# S3 - Computation of Survival Rates per City and Municipality
# July 7, 2015
# Author: Troy James R Palanca

# Libraries ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(reshape2)
library(extrafont)
library(scales)
library(stringr)
library(gridExtra)
library(ggmap)
library(stringdist)
loadfonts(quiet = T)
library(png)

# Data --------------------------------------------------------------------
load("Data/D1 - Enrollment Data.RData")
load("Data/D2 - Schools Data.RData")
load("Data/D3 - CityMuni Data.RData")

PH.map <- get_googlemap (
  center = c(121.8347,12.45113),
  zoom = 5,
  scale = 4,
  maptype = "roadmap",
  region = "PH",
  filename = "Philippines Map",
  style = "feature:all|element:labels|visibility:off",
  color = "bw"
)

# CityMuni Level Cohort ---------------------------------------------------

schools.dt <- schools.dt %>%
  mutate(school.citymuni = paste(school.citymuni, school.province))

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
  mutate(dropout = enrollment/lag(enrollment) - 1,
         valid = as.numeric(grade) - lag(as.numeric(grade)) == 1) %>%
  ungroup() %>% filter(!is.na(dropout))

survival_append.dt <- data.frame(
  school.citymuni = rep(unique(survival.dt$school.citymuni), 6),
  grade = rep("Grade 1", 6*length(unique(survival.dt$school.citymuni))),
  year = rep(2013:2015,
             each = 2*length(unique(survival.dt$school.citymuni))),
  gender = rep(c("Female", "Male"),
               3*length(unique(survival.dt$school.citymuni))),
  mean.dropout = 0,
  min.dropout = 0,
  max.dropout = 0,
  mean.survival = 1,
  min.survival = 1,
  max.survival = 1)

elem.survival.dt <- survival.dt %>%  filter(grade != "Grade 1" & as.numeric(grade) <= 7) %>%
  group_by(school.citymuni, grade, gender, year) %>%
  summarise(mean.dropout = mean(dropout), mean.survival = 1 + mean.dropout,
            min.dropout = min(dropout), max.dropout = max(dropout),
            min.survival = 1 + max.dropout, max.survival = 1 + min.dropout) %>%
  rbind(survival_append.dt) %>%
  group_by(school.citymuni, gender, year) %>% arrange(grade) %>%
  mutate(mean.cum.survival = cumprod(mean.survival),
         max.cum.survival = cumprod(max.survival),
         min.cum.survival = cumprod(min.survival)) %>% ungroup() %>%
  left_join(schools.dt %>% group_by(school.citymuni) %>%
              summarise(map.lat = mean(map.lat, na.rm = T),
                        map.lon = mean(map.lon, na.rm = T)))
rm(survival_append.dt)
rm(eligibleschools.dt)

survival_append.dt <- data.frame(
  school.citymuni = rep(unique(survival.dt$school.citymuni), 6),
  grade = rep("Year 1 / Grade 7",
              6*length(unique(survival.dt$school.citymuni))),
  year = rep(2013:2015, each =
               2*length(unique(survival.dt$school.citymuni))),
  gender = rep(c("Female", "Male"),
               3*length(unique(survival.dt$school.citymuni))),
  mean.dropout = 0,
  min.dropout = 0,
  max.dropout = 0,
  mean.survival = 1,
  min.survival = 1,
  max.survival = 1)

hs.survival.dt <- survival.dt %>%  filter(as.numeric(grade) > 8) %>%
  group_by(school.citymuni, grade, gender, year) %>%
  summarise(mean.dropout = mean(dropout), mean.survival = 1 + mean.dropout,
            min.dropout = min(dropout), max.dropout = max(dropout),
            min.survival = 1 + max.dropout, max.survival = 1 + min.dropout) %>%
  rbind(survival_append.dt) %>%
  group_by(school.citymuni, gender, year) %>% arrange(grade) %>%
  mutate(mean.cum.survival = cumprod(mean.survival),
         max.cum.survival = cumprod(max.survival),
         min.cum.survival = cumprod(min.survival)) %>% ungroup() %>%
  left_join(schools.dt %>% group_by(school.citymuni) %>%
              summarise(map.lat = mean(map.lat, na.rm = T),
                        map.lon = mean(map.lon, na.rm = T)))
rm(survival_append.dt)

# Cleaning of Latitude and Longitude Data ---------------------------------

# Elementary

for (i in 1:length(elem.survival.dt$school.citymuni[is.na(elem.survival.dt$map.lat)])) {
  distances.vc <- stringdist(
    tolower((elem.survival.dt$school.citymuni[is.na(elem.survival.dt$map.lat)])[i]),
    tolower(paste(citymuni.dt$citymuni, citymuni.dt$province)))
  elem.survival.dt$matched.citymuni[is.na(elem.survival.dt$map.lat)][i] <-
    citymuni.dt$citymuni[distances.vc == min(distances.vc)]
  elem.survival.dt$matched.map.lat[is.na(elem.survival.dt$map.lat)][i] <-
    citymuni.dt$map.lat[distances.vc == min(distances.vc)]
  elem.survival.dt$matched.map.lon[is.na(elem.survival.dt$map.lat)][i] <-
    citymuni.dt$map.lon[distances.vc == min(distances.vc)]
  print(citymuni.dt$citymuni[distances.vc == min(distances.vc)])
}

# checked for consistency - OK, so transfer over to columns

elem.survival.dt$map.lon[is.na(elem.survival.dt$map.lat)] <-
  elem.survival.dt$matched.map.lon[is.na(elem.survival.dt$map.lat)]
elem.survival.dt$map.lat[is.na(elem.survival.dt$map.lat)] <-
  elem.survival.dt$matched.map.lat[is.na(elem.survival.dt$map.lat)]

elem.survival.dt$matched.citymuni <- elem.survival.dt$matched.map.lat <-
  elem.survival.dt$matched.map.lon <- NULL

# Secondary

for (i in 1:length(hs.survival.dt$school.citymuni[is.na(hs.survival.dt$map.lat)])) {
  distances.vc <- stringdist(
    tolower((hs.survival.dt$school.citymuni[is.na(hs.survival.dt$map.lat)])[i]),
    tolower(paste(citymuni.dt$citymuni, citymuni.dt$province)))
  hs.survival.dt$matched.citymuni[is.na(hs.survival.dt$map.lat)][i] <-
    citymuni.dt$citymuni[distances.vc == min(distances.vc)]
  hs.survival.dt$matched.map.lat[is.na(hs.survival.dt$map.lat)][i] <-
    citymuni.dt$map.lat[distances.vc == min(distances.vc)]
  hs.survival.dt$matched.map.lon[is.na(hs.survival.dt$map.lat)][i] <-
    citymuni.dt$map.lon[distances.vc == min(distances.vc)]
  print(citymuni.dt$citymuni[distances.vc == min(distances.vc)])
}

# checked for consistency - OK, so transfer over to columns

hs.survival.dt$map.lon[is.na(hs.survival.dt$map.lat)] <-
  hs.survival.dt$matched.map.lon[is.na(hs.survival.dt$map.lat)]
hs.survival.dt$map.lat[is.na(hs.survival.dt$map.lat)] <-
  hs.survival.dt$matched.map.lat[is.na(hs.survival.dt$map.lat)]

hs.survival.dt$matched.citymuni <- hs.survival.dt$matched.map.lat <-
  hs.survival.dt$matched.map.lon <- NULL

if (sum(is.na(elem.survival.dt$map.lat)) + sum(is.na(hs.survival.dt$map.lon)) == 0) {
  print("OK for map coordinates.")
} else {
  stop("ERROR: Map coordinates incomplete.")
}

if (sum(is.na(hs.survival.dt$map.lat)) + sum(is.na(hs.survival.dt$map.lon)) == 0) {
  print("OK for map coordinates.")
} else {
  stop("ERROR: Map coordinates incomplete.")
}

# Plot Production ---------------------------------------------------------

elementary.dt <- elem.survival.dt %>% filter(grade == "Grade 6") %>%
  mutate(mean.cum.survival = ifelse(mean.cum.survival >= 1, 1, mean.cum.survival),
         level = "Elementary", cum.dropout = 1-mean.cum.survival) %>%
  group_by(year, gender) %>%
  mutate(poor.performance = cum.dropout >= quantile(cum.dropout, 0.75)) %>%
  ungroup()

secondary.dt <- hs.survival.dt %>% filter(grade == "Year 4 / Grade 10") %>%
  mutate(mean.cum.survival = ifelse(mean.cum.survival >= 1, 1, mean.cum.survival),
         level = "Secondary", cum.dropout = 1-mean.cum.survival) %>%
  group_by(year, gender) %>%
  mutate(poor.performance = cum.dropout >= quantile(cum.dropout, 0.75)) %>%
  ungroup()

elementary_survival.gg <- ggmap(PH.map, base_layer =
                                  ggplot(data = elementary.dt,
                                         mapping = aes(x = map.lon, y = map.lat))) +
  facet_grid(gender ~ year) +
  geom_point(aes(color = mean.cum.survival), size = 1, alpha = 0.75) +
  geom_density2d(data = secondary.dt[secondary.dt$poor.performance,], bins = 2, color = "red") +
  scale_color_gradient(low = "darkred", high = "darkgreen", limits = c(0,1),
                       name = "Survival Rate\nup to Grade 6 (%)",
                       labels = percent) +
  coord_map(xlim = c(116.812721, 126.856628), ylim = c(4.46811, 21.23415)) +
  ggtitle("Elementary") +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(family = "Open Sans"),
        plot.title = element_text(hjust = 0, family = "Raleway", face = "bold"),
        plot.background = element_rect(color = NA, fill = "grey98"),
        legend.background = element_blank(),
        strip.text = element_text(size = 14))


secondary_survival.gg <- ggmap(PH.map, base_layer =
                                 ggplot(data = secondary.dt,
                                        mapping = aes(x = map.lon, y = map.lat))) +
  facet_grid(gender ~ year) +
  geom_point(aes(color = mean.cum.survival), size = 1, alpha = 0.75) +
  geom_density2d(data = secondary.dt[secondary.dt$poor.performance,], bins = 2, color = "red") +
  scale_color_gradient(low = "darkred", high = "darkgreen", limits = c(0,1),
                       name = "Survival Rate\nup to Year 4 /\nGrade 10 (%)",
                       labels = percent) +
  coord_map(xlim = c(116.812721, 126.856628), ylim = c(4.46811, 21.23415)) +
  ggtitle("Secondary") +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(family = "Open Sans"),
        plot.title = element_text(hjust = 0, family = "Raleway", face = "bold"),
        plot.background = element_rect(color = NA, fill = "grey98"),
        legend.background = element_blank(),
        strip.text = element_text(size = 14))

survivalmaps.grob <- arrangeGrob(elementary_survival.gg, secondary_survival.gg, ncol = 2)

survivaltitle.grob <-
  arrangeGrob(
    arrangeGrob(
      textGrob(
        label = paste("Educational Gaps"),
        gp = gpar(
          fontfamily = "Raleway",
          fontsize = 24,
          fontface = "bold"
        ),
        just = "left",
        x = unit(0.03, "npc"),
        y = unit(0.35, "npc")
      ),
      textGrob(
        label = paste("Cumulative completion rates per city/municipality, 2013-2015"),
        gp = gpar(
          fontfamily = "Open Sans",
          fontsize = 12,
          fontface = "italic"
        ),
        just = "left",
        x = unit(0.03, "npc"),
        y = unit(0.75, "npc")
      ),
      ncol = 1,
      heights = c(0.6, 0.4)
    ),
    rasterGrob(readPNG("Data/JDT Watermark.png")),
    ncol = 2,
    widths = c(0.6, 0.4)
  )

survivalnotes.grob <-
  textGrob(x = unit(0.03, "npc"), just = "left",
           label = paste(c(
             "Explanatory notes:",
             "1. Survival rates were computed from the empirical proportion of students that move from the previous grade level, to the current grade level. Therefore,\n     the survival rates cannot be interpreted as  the survival experience of a specific cohort.",
             "2. Migration (moving from one school to another) cannot be tracked and excluded from the dropout computation.",
             "3. Red lines indicate a kernel density estimate for schools whose dropout rates belong to the highest 25%."),
             collapse = "\n"),
           gp = gpar(fontfamily = "Open Sans",
                     fontsize = 8,
                     lineheight = 0.8))

survivalfooter.gg <-
  textGrob(x = unit(0.03, "npc"), just = "left",
           label = paste(c(
             "Troy James R Palanca | www.jumbodumbothoughts.com",
             "Data Source: Department of Education",
             "Disclaimer: Content is provided for information purposes only."),
             collapse = "\n"),
           gp = gpar(fontfamily = "Open Sans",
                     fontsize = 7,
                     lineheight = 0.8))

png("Output/O3 - Survival Map.png",
    width = 14*300, height = 12*300,
    bg = "gray98",
    res = 350)
print(
  arrangeGrob(
    survivaltitle.grob,
    survivalmaps.grob,
    survivalnotes.grob,
    survivalfooter.gg,
    heights = c(0.1, 0.8, 0.05, 0.05),
    ncol = 1
  )
)
dev.off()