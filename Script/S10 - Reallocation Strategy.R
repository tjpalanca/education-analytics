# Data Analytics for Education
# S10 - Reallocation Strategies
# August 10, 2015
# Author: Troy James R Palanca

# This script explores possible reallocation strategies to improve capacity within the current
# resource constraints.

# Libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(extrafont)
loadfonts(quiet = T)
library(stringdist)
library(ggmap)

# Data --------------------------------------------------------------------
load("Data/D5 - Capacity Data.RData")
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

# Difference between elementary and secondary schools effect --------------

# Distribution of change in ratio from using vacant rooms
ggplot(schools.dt %>%
         filter(!(full.room.ratio == academic.room.ratio)),
       aes(x = (full.room.ratio - academic.room.ratio))) +
  geom_density(aes(fill = school.classification), color = NA, alpha = 0.5) +
  scale_x_log10()

# Distribution of change in ratio from using nonstandard rooms
ggplot(schools.dt %>%
         filter(!(standard.room.ratio == academic.room.ratio)),
       aes(x = (academic.room.ratio - standard.room.ratio))) +
  geom_density(aes(fill = school.classification), color = NA, alpha = 0.5) +
  scale_x_log10()

# Elementary and secondary schools do not seem to exhibit difference,
# so they will be treated as a whole.

# Cleaning of Latitude and Longitude Data ---------------------------------

for (i in 1:length(schools.dt$school.citymuni[is.na(schools.dt$map.lat)])) {
  distances.vc <- stringdist(
    tolower(paste(schools.dt$school.citymuni[is.na(schools.dt$map.lat)][i],
                  schools.dt$school.province[is.na(schools.dt$map.lat)][i])),
    tolower(paste(citymuni.dt$citymuni, citymuni.dt$province)))
  if (length(citymuni.dt$citymuni[distances.vc == min(distances.vc)]) == 1) {
    schools.dt$matched.citymuni[is.na(schools.dt$map.lat)][i] <-
      citymuni.dt$citymuni[distances.vc == min(distances.vc)]
    schools.dt$matched.map.lat[is.na(schools.dt$map.lat)][i] <-
      citymuni.dt$map.lat[distances.vc == min(distances.vc)]
    schools.dt$matched.map.lon[is.na(schools.dt$map.lat)][i] <-
      citymuni.dt$map.lon[distances.vc == min(distances.vc)]
    print(citymuni.dt$citymuni[distances.vc == min(distances.vc)])
  }
}

# checked for consistency - OK, so transfer over to columns

schools.dt$map.lon[is.na(schools.dt$map.lat)] <-
  schools.dt$matched.map.lon[is.na(schools.dt$map.lat)]
schools.dt$map.lat[is.na(schools.dt$map.lat)] <-
  schools.dt$matched.map.lat[is.na(schools.dt$map.lat)]

schools.dt$matched.citymuni <- schools.dt$matched.map.lat <-
  schools.dt$matched.map.lon <- NULL

sum(is.na(schools.dt$map.lat))
sum(is.na(schools.dt$map.lon))

rm(citymuni.dt)

# Using Vacant Rooms (Waterfall plot) -------------------------------------

# Data transformation for waterfall plot
schools_roomuse.dt <-
  schools.dt %>%
  select(full.room.ratio,
         academic.room.ratio,
         standard.room.ratio) %>%
  summarise_each(funs(. = 1/mean(.))) %>%
  melt() %>%
  mutate(variable = factor(variable,
                           levels = rev(variable))) %>%
  arrange(variable)

schools_roomuse_waterfall.dt <-
  data.frame(
    phase = factor(x = str_wrap(c("Standard Capacity",
                                  "Effect of using non-standard rooms",
                                  "Effect of using vacant rooms",
                                  "Full Capacity"),20),
                   levels = str_wrap(c("Standard Capacity",
                                       "Effect of using non-standard rooms",
                                       "Effect of using vacant rooms",
                                       "Full Capacity"),20)),
    start = c(0, schools_roomuse.dt$value),
    end   = c(schools_roomuse.dt$value, 0)
  ) %>%
  mutate(upper = ifelse(start > end, start, end),
         lower = ifelse(start < end, start, end),
         label = (end - start))

# Waterfall plot
rooms_waterfall.gg <-
  ggplot(schools_roomuse_waterfall.dt) +
  geom_rect(aes(
    x = phase,
    xmin = as.numeric(phase) - 0.2,
    xmax = as.numeric(phase) + 0.2,
    ymin = lower,
    ymax = upper,
    fill = start > end)) +
  geom_text(data = schools_roomuse_waterfall.dt[2:3,],
            aes(
              x = phase,
              y = end,
              label = round(label,2)
            ), vjust = -0.5, color = "white", family = "Open Sans") +
  geom_text(data = schools_roomuse_waterfall.dt[c(1,4),],
            aes(
              x = phase,
              y = upper,
              label = round(abs(label),2)
            ), vjust = 1, color = "white", family = "Open Sans") +
  ylab("Students per Room") +
  scale_fill_manual(values = c("firebrick", "forestgreen")) +
  coord_cartesian(ylim = range(schools_roomuse.dt$value) * c(0.98, 1.02)) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Open Sans"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text.x = element_text(size = 12),
        panel.grid.major.x = element_blank())

ggplot(schools.dt %>% filter(academic.room.ratio != standard.room.ratio) %>%
         mutate(change = academic.room.ratio - standard.room.ratio) %>%
         arrange(change) %>%
         mutate(school.name = factor(school.name, levels = unique(school.name))) %>%
         filter(row_number() %in% 1:50),
       aes(x = school.name, y = change)) +
  geom_bar(stat = "identity") +
  coord_flip()

# Maps of high need cities ------------------------------------------------

schools.dt <- schools.dt %>%
  mutate(use.nonstandard = academic.room.ratio - standard.room.ratio,
         use.vacant = full.room.ratio - academic.room.ratio)

schools_nonstandard.dt <- schools.dt %>% filter(use.nonstandard > 0)

ggmap(ggmap = PH.map,
      base_layer = ggplot(data = schools_nonstandard.dt,
                          aes(x = map.lon, y = map.lat))) +
  geom_point(aes(color = log10(use.nonstandard),
                 alpha = log10(use.nonstandard)),
             size = 1) +
  geom_density2d(data = schools_nonstandard.dt %>%
                   filter(use.nonstandard > quantile(use.nonstandard, 0.9)),
                 aes(z = use.nonstandard), bins = 2) +
  scale_color_gradient(low = "red1", high = "red4") +
  coord_map(xlim = c(116.812721, 126.856628),
            ylim = c(4.46811, 21.23415))