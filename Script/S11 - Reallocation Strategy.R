# Data Analytics for Education
# S11 - Reallocation Strategies
# August 12, 2015
# Author: Troy James R Palanca

# Libraries ---------------------------------------------------------------
library(maptools)
library(dplyr)
library(ggplot2)
library(rjson)
library(geosphere)

# Data --------------------------------------------------------------------
load("Data/D5 - Capacity Data.RData")

# Shapefiles are from PhilGIS.org
PH.shp <- readShapeSpatial("Data/PH Shapefile/Country",
                           proj4string = CRS("+proj=longlat +datum=WGS84"))
PHprov.shp <- (readShapeSpatial("Data/PH Provinces Shapefile/Provinces",
                                proj4string =
                                  CRS("+proj=longlat +datum=WGS84"),
                                IDvar = "PROVINCE"))

# Map Theme
map.thm <-
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray98", color = NA),
        legend.background = element_rect(fill = NA, color = NA))

# MapQuest API Key (for computing distances)
MapQuestAPIKey <- "Fmjtd%7Cluu82q0zl9%2Crn%3Do5-94y55f"

# Data Preprocessing ------------------------------------------------------

# Since these methods require the *exact* location of all schools, schools without
# map coordinates are not imputed, but ignored. This results in loss of 3,648 (8.75%) of schools.

(with(schools.dt, sum(is.na(map.lat) | is.na(map.lon)))) # 3,648
(with(schools.dt, sum(is.na(map.lat) | is.na(map.lon))))/nrow(schools.dt) # 8.75%
schools.dt <- schools.dt %>%
  filter(!is.na(map.lat) & !is.na(map.lon))

# Functions ---------------------------------------------------------------

MapQuestRoutingAPI <- function (fromLat, fromLng, toLat, toLng, APIKey) {
  url <- paste0("http://open.mapquestapi.com/directions/v2/route?key=",APIKey,
               "&ambiguities=ignore&unit=m&routeType=fastest",
               "&from=",fromLat,",",fromLng,
               "&to=",toLat,",",toLng)
  APIresult <- fromJSON(file = url)
  print(APIresult$route$distance)
  return(APIresult$route$distance)
}

GetRadiusSchools <- function (schoolLatLng,
                              schools.list,
                              APIKey,
                              radius,
                              initial.search = 2) {
  # Remove own observation
  schools.list <- schools.list %>% filter(!(map.lat == schoolLatLng[1] & map.lon == schoolLatLng[2]))
  # Compute Meeus distances
  schools.Meeusdist <- distMeeus(
    p1 = c(schoolLatLng[2], schoolLatLng[1]),
    p2 = schools.list %>% select(map.lon, map.lat)
  )/1000
  # Reduce to schools within radius * initial.search
  schools.list <- schools.list[schools.Meeusdist <= radius * initial.search,]
  # Compute MapQuest Distances
  schools.Roaddist <-
    mapply(
      MapQuestRoutingAPI,
      simplify2array(schools.list[,"map.lat"]),
      simplify2array(schools.list[,"map.lon"]),
      MoreArgs = list(fromLat = schoolLatLng[1],
                      fromLng = schoolLatLng[2],
                      APIKey = APIKey)
    )
  # Return school ids within radius distance
  return(schools.list$school.id[schools.Roaddist <= radius])
}

GetRadiusSchools(schoolLatLng = simplify2array(c(schools.dt[1,"map.lat"],
                                                 schools.dt[2,"map.lon"])),
                 APIKey = MapQuestAPIKey,
                 schools.list = schools.dt %>% select(school.id, map.lat, map.lon),
                 radius = 3,
                 initial.search = 1.5)

# Teacher Reallocations ---------------------------------------------------


