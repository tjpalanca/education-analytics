# Data Analytics for Education
# S9 - Capacity Clustering - Development of Cluster Dashboards
# July 19, 2015

# Libraries ---------------------------------------------------------------
library(maptools)
library(ggplot2)
library(dplyr)
library(reshape2)
library(pbapply)

# Data --------------------------------------------------------------------
load("Data/D7 - Cluster Profiles.RData")
rm(schools.dt)

# Shapefiles are from PhilGIS.org

PH.shp <- readShapeSpatial("Data/PH Shapefile/Country",
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))
PHprov.shp <- (readShapeSpatial("Data/PH Provinces Shapefile/Provinces",
                                proj4string =
                                  CRS("+proj=longlat +datum=WGS84"),
                                IDvar = "PROVINCE"))

# Data Adjustments --------------------------------------------------------

# Conform provinces to shapefile provinces
PHprov.ref <- data.frame(school.province = unique(schools.dt$school.province),
                         stringsAsFactors = F)
PHprov.ref$school.province.shp <-
  sapply(PHprov.ref$school.province,
         function(x) {
           if (toupper(x) %in% toupper(PHprov.shp@data$PROVINCE)) {
             return(as.character(PHprov.shp@data$PROVINCE[
               toupper(PHprov.shp@data$PROVINCE) == toupper(x)]))
           } else {
             return(NA)
           }
         })

## Manual referencing
PHprov.ref$school.province.shp[is.na(PHprov.ref$school.province.shp)] <-
  c("Samar", "Basilan", "Maguindanao", unlist(rep("Metropolitan Manila", 4)))

# Add cluster name and shapefile province to datasets
schools_elem.dt <- schools_elem.dt %>%
  left_join(schools_elem_profiles_formatted.dt %>%
              select(cluster.num, cluster.name = Cluster),
            by = c("cluster" = "cluster.num")) %>%
  left_join(PHprov.ref)

schools_seco.dt <- schools_seco.dt %>%
  left_join(schools_seco_profiles_formatted.dt %>%
              select(cluster.num, cluster.name = Cluster),
            by = c("cluster" = "cluster.num")) %>%
  left_join(PHprov.ref)

# Clean up
rm(PHprov.ref)

# Metric Means Plot ----------------------------------------------------
PlotMetricMeans <- function(data, cluster.num) {

}

ggplot(schools_elem.dt %>% filter(cluster.name == "Head of the Pack") %>%
         select(all.teacher.ratio, full.room.ratio, mooe.ratio) %>%
         melt(variable.name = "metric", value.name = "value"),
       aes(x = log10(value))) +
  facet_wrap(~metric, ncol = 1, scales = "free_x") +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_density(data = schools_elem.dt %>%
                 select(all.teacher.ratio, full.room.ratio, mooe.ratio) %>%
                 melt(variable.name = "metric", value.name = "value"), fill = "orange",
               alpha = 0.5)

# Geographical Distribution Plot ----------------------------------------

schools_elem_byprovince.dt <-
  schools_elem.dt %>% group_by(school.province) %>% mutate(total.schools = n()) %>%
  group_by(school.region, cluster.name) %>%
  summarise(perc.schools = n()/mean(total.schools)) %>% ungroup()

ggplot(schools_elem_byregion.dt, aes(x = school.region, y = perc.schools,
                                       color = cluster.name, fill = cluster.name)) +
  facet_wrap(~cluster.name, ncol = 6) +
  geom_bar(stat = "identity") +
  coord_flip()



unique(PHprov.shp@data$PROVINCE)[!(
  toupper(unique(PHprov.shp@data$PROVINCE)) %in%
    toupper(unique(schools_elem.dt$school.province)))]

unique(schools.dt$school.province)

length(unique(schools_elem.dt$school.province))

length()


