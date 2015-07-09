# Data Analytics for Education
# S4 - Computation of Path of Least Resistance
# July 7, 2015

# Libraries ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(reshape2)
library(extrafont)
loadfonts(quiet = T)
library(scales)
library(stringr)
library(gridExtra)
library(ggmap)
library(stringdist)
library(geosphere)

# Data --------------------------------------------------------------------
load("Data/D1 - Enrollment Data.RData")
load("Data/D2 - Schools Data.RData")
load("Data/D3 - CityMuni Data.RData")
schools.dt <- schools.dt %>% mutate(school.citymuni = paste(school.citymuni, school.province))

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

# Clean up missing mapping variables --------------------------------------
for (i in 1:length(schools.dt$school.citymuni[is.na(schools.dt$map.lat)])) {
  distances.vc <- stringdist(
    tolower((schools.dt$school.citymuni[is.na(schools.dt$map.lat)])[i]),
    tolower(paste(citymuni.dt$citymuni, citymuni.dt$province)))
  schools.dt$matched.citymuni[is.na(schools.dt$map.lat)][i] <-
    citymuni.dt$citymuni[distances.vc == min(distances.vc)][1]
  schools.dt$matched.map.lat[is.na(schools.dt$map.lat)][i] <-
    citymuni.dt$map.lat[distances.vc == min(distances.vc)][1]
  schools.dt$matched.map.lon[is.na(schools.dt$map.lat)][i] <-
    citymuni.dt$map.lon[distances.vc == min(distances.vc)][1]
  print(citymuni.dt$citymuni[distances.vc == min(distances.vc)][1])
}

schools.dt$map.lon[is.na(schools.dt$map.lat)] <-
  schools.dt$matched.map.lon[is.na(schools.dt$map.lat)]
schools.dt$map.lat[is.na(schools.dt$map.lat)] <-
  schools.dt$matched.map.lat[is.na(schools.dt$map.lat)]

schools.dt$matched.citymuni <- schools.dt$matched.map.lat <-
  schools.dt$matched.map.lon <- NULL

if (sum(is.na(schools.dt$map.lat)) + sum(is.na(schools.dt$map.lon)) == 0) {
  print("OK for map coordinates.")
} else {
  stop("ERROR: Map coordinates incomplete.")
}

rm(distances.vc, i)

# Computation of path of least resistance ---------------------------------
elementary.dt <- schools.dt %>% filter(school.classification == "Elementary")

for (i in 1:nrow(citymuni.dt)){
  distances.vc <- distGeo(
    as.matrix(data.frame(map.lon = rep(citymuni.dt$map.lon[i], nrow(elementary.dt)),
               map.lat = rep(citymuni.dt$map.lat[i], nrow(elementary.dt)))),
    as.matrix(elementary.dt[c("map.lon", "map.lat")]))
  citymuni.dt$nearest.elementary.school[i] <-
    elementary.dt$school.name[distances.vc == min(distances.vc)][1]
  citymuni.dt$nearest.elementary.school.lat[i] <-
    elementary.dt$map.lat[distances.vc == min(distances.vc)][1]
  citymuni.dt$nearest.elementary.school.lon[i] <-
    elementary.dt$map.lon[distances.vc == min(distances.vc)][1]
  citymuni.dt$nearest.elementary.school.dist[i] <- min(distances.vc)
  print(paste(citymuni.dt$nearest.elementary.school[i],
              format(i/nrow(citymuni.dt), digits = 2)))
}

secondary.dt <- schools.dt %>% filter(school.classification == "Secondary")

for (i in 1:nrow(citymuni.dt)){
  distances.vc <- distGeo(
    as.matrix(data.frame(map.lon = rep(citymuni.dt$map.lon[i], nrow(secondary.dt)),
                         map.lat = rep(citymuni.dt$map.lat[i], nrow(secondary.dt)))),
    as.matrix(secondary.dt[c("map.lon", "map.lat")]))
  citymuni.dt$nearest.secondary.school[i] <-
    secondary.dt$school.name[distances.vc == min(distances.vc)][1]
  citymuni.dt$nearest.secondary.school.lat[i] <-
    secondary.dt$map.lat[distances.vc == min(distances.vc)][1]
  citymuni.dt$nearest.secondary.school.lon[i] <-
    secondary.dt$map.lon[distances.vc == min(distances.vc)][1]
  citymuni.dt$nearest.secondary.school.dist[i] <- min(distances.vc)
  print(paste(citymuni.dt$nearest.secondary.school[i],
              format(i/nrow(citymuni.dt), digits = 2)))
}

rm(distances.vc, i)

# Plot of Paths -----------------------------------------------------------
paths.dt <- citymuni.dt %>% mutate(citymuniprov = paste(citymuni, province)) %>%
  select(citymuniprov, map.lat, map.lon,
         nearest.elementary.school.lat,
         nearest.elementary.school.lon,
         nearest.secondary.school.lat,
         nearest.secondary.school.lon) %>%
  melt(id.var = "citymuniprov", variable.name = "stage", value.name = "coordinate")

paths.dt <- cbind(paths.dt, do.call("rbind", strsplit(as.character(paths.dt$stage), "\\."))) %>%
  mutate(type_coord = ifelse(`1` == "map", as.character(`2`), as.character(`4`)),
         level = ifelse(`1` == "map", "origin", as.character(`2`))) %>%
  select(-`1`, -`2`, -`3`, -`4`, -stage) %>%
  dcast(citymuniprov + level ~ type_coord, value.var = "coordinate")

paths.dt$level <- factor(as.character(paths.dt$level),
                         levels = c("origin", "elementary", "secondary"))

paths.dt <- paths.dt %>% arrange(citymuniprov, level) %>%
  left_join(citymuni.dt %>% mutate(citymuniprov = paste(citymuni, province)) %>%
              select(citymuniprov, nearest.elementary.school.dist,
                     nearest.secondary.school.dist))

ggmap(PH.map, base_layer = ggplot(paths.dt, aes(x = lon, y = lat))) +
  geom_path(aes(group = citymuniprov)) +
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


# Plot of Distances -------------------------------------------------------


# Correlate with Dropout Rates --------------------------------------------




# To Do:
# 4. Compute for capacity metrics and cluster the schools
# 5. Check for the upper limit on capacity metrics using correlates