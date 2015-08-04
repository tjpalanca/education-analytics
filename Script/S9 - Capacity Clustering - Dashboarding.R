# Data Analytics for Education
# S9 - Capacity Clustering - Development of Cluster Dashboards
# July 19, 2015

# Libraries ---------------------------------------------------------------
library(maptools)
library(ggplot2)

# Data --------------------------------------------------------------------
load("Data/D7 - Cluster Profiles.RData")

schools_elem.dt <-
  schools_elem.dt %>% left_join(schools_elem_profiles_formatted.dt %>%
                                  select(cluster.num, cluster.name = Cluster),
                                by = c("cluster" = "cluster.num"))

PH.shp <- fortify(readShapeSpatial("Data/PH Shapefile/Country",
                                   proj4string = CRS("+proj=longlat +datum=WGS84")))

# Map ---------------------------------------------------------------------

schools_elem_byprovince.dt <-
  schools_elem.dt %>% group_by(school.province) %>% mutate(total.schools = n()) %>%
  group_by(school.province, cluster.name) %>%
  summarise(perc.schools = n()/mean(total.schools)) %>% ungroup()

ggplot(schools_elem_byprovince.dt, aes(x = school.province, y = perc.schools,
                                       color = cluster.name, fill = cluster.name)) +
  geom_bar(stat = "identity") +
  coord_flip()


