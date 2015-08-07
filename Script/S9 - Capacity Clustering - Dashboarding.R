# Data Analytics for Education
# S9 - Capacity Clustering - Development of Cluster Dashboards
# July 19, 2015

# Libraries ---------------------------------------------------------------
library(maptools)
library(ggplot2)
library(dplyr)
library(reshape2)
library(pbapply)
library(extrafont)
loadfonts()
library(scales)
library(stringr)

# Data --------------------------------------------------------------------
load("Data/D7 - Cluster Profiles.RData")
load("Data/D3 - CityMuni Data.RData")
national.cohort.dt <- readRDS("Data/D8 - National Level Cohort Surival.rds")
load("Data/D1 - Enrollment Data.RData")

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


# Functions ---------------------------------------------------------------

ggplot_colors <- function (n) {
  colors = seq(15, 375, length = n+1)
  hcl(h = colors, l = 65, c = 100)[1:n]
}

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
rm(schools.dt)

# Clean up enrollment file
enrolment.dt <- enrolment.dt[,1:5]
eligibleschools.dt <- enrolment.dt %>% group_by(school.id) %>% summarise(count = n()) %>%
  mutate(secondary = school.id > 300000) %>%
  mutate(complete = (secondary == F & count == 56) | (secondary == T & count == 32)) %>%
  filter(complete == T)
enrolment.dt <- enrolment.dt[enrolment.dt$school.id %in% eligibleschools.dt$school.id, ]
rm(eligibleschools.dt)

# Compute dropout rates per school
enrolment.dt$cohort <- enrolment.dt$school.id + enrolment.dt$year - as.numeric(enrolment.dt$grade)

View(enrolment.dt %>% arrange(gender, year, grade))
# Metric Means Plot ----------------------------------------------------

PlotMetrics <- function (data, cluster.num) {
  Trim <- function(vec, p = 0.01) {
    newvec <- vec
    newvec[vec > quantile(vec, 1-p)] <- NA
    newvec[vec < quantile(vec, p)] <- NA
    newvec
  }

  # All data
  all.data <- data %>%
    select(`Students\nper Teacher` = all.teacher.ratio,
           `Students\nper Room` = full.room.ratio,
           `MOOE\nper Student` = mooe.ratio) %>%
    melt(variable.name = "metric", value.name = "value") %>%
    group_by(metric) %>% summarise(mean.value = mean(value),
                                   max.value = quantile(value, 0.99),
                                   min.value = quantile(value, 0.01)) %>%
    ungroup()

  # Winsorize data
  cluster.data <- data %>%
    rename(`Students\nper Teacher` = all.teacher.ratio,
           `Students\nper Room` = full.room.ratio,
           `MOOE\nper Student` = mooe.ratio)
  cluster.data$`Students\nper Teacher` <- Trim(cluster.data$`Students\nper Teacher`)
  cluster.data$`Students\nper Room` <- Trim(cluster.data$`Students\nper Room`)
  cluster.data$`MOOE\nper Student` <- Trim(cluster.data$`MOOE\nper Student`)

  # Generate cluster data
  cluster.data <- cluster.data %>% filter(cluster == cluster.num) %>%
    select(`Students\nper Teacher`,
           `Students\nper Room`,
           `MOOE\nper Student`) %>%
    melt(variable.name = "metric", value.name = "value")

  # Plot chart
  ggplot(data = all.data, aes(y = value, x = 1)) +
    facet_wrap(~metric, ncol = 3,
               scales = "free_y") +
    geom_hline(data = all.data,
               aes(yintercept = mean.value),
               color = "red", alpha = 0.5) +
    geom_violin(data = cluster.data,
                aes(fill = metric),
                alpha = 0.5) +
    geom_blank(data = all.data, aes(y = max.value)) +
    geom_blank(data = all.data, aes(y = min.value)) +
    ggtitle("Capacity Metrics\n") +
    scale_y_continuous(labels = function(x) {
      format((x >= 1) * x + (x < 1) * (1/x), digits = 2)}) +
    theme_minimal() +
    theme(text = element_text(family = "Open Sans"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(family = "Raleway", face = "bold", hjust = 0),
          strip.text = element_text(family = "Raleway", face = "bold", size = 10))
}

# Survival Rate Plot ------------------------------------------------------

PlotSurvivalRates <- function (data, cluster.num) {
  # Computation of Survival Rates

}

# Geographical Distribution Plot ----------------------------------------

PlotProvincialMap <- function (data, shp, cluster.num) {
  data_byprovince.dt <- data %>%
    group_by(school.province.shp) %>%
    mutate(total.schools = n()) %>%
    group_by(school.province.shp, cluster, cluster.name) %>%
    summarise(perc.schools = n()/mean(total.schools)) %>% ungroup() %>%
    filter(cluster == cluster.num)
  map.dt <- fortify(shp) %>%
    left_join(data_byprovince.dt, by = c("id" = "school.province.shp"))
  ggplot(map.dt, aes(x = long, y = lat, group = group, fill = perc.schools)) +
    geom_polygon() +
    geom_text(data = map.dt %>%
                group_by(id) %>%
                summarise(long = mean(long),
                          lat = mean(lat),
                          perc.schools = mean(perc.schools)) %>%
                arrange(desc(perc.schools)) %>%
                filter(row_number() %in% 1:5),
              aes(label = str_wrap(id,10), group = str_wrap(id,8)),
              size = 3, family = "Open Sans",
              lineheight = 0.7) +
    scale_fill_gradient(limits = c(0,NA), labels = percent,
                        low = "gray95", high = ggplot_colors(6)[cluster.num], na.value = "gray98",
                        name = "% of Schools") +
    coord_map() +
    ggtitle("Geographic Distribution") +
    map.thm +
    theme(plot.title = element_text(size = 16, face = "bold", family = "Raleway", hjust = 0),
          text = element_text(family = "Open Sans"),
          legend.position = "bottom")
}

PlotProvincialMap(schools_elem.dt, PHprov.shp, cluster.num = 4)

# Dashboard Construction --------------------------------------------------




