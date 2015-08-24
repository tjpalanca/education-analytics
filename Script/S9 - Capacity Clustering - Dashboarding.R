# Data Analytics for Education
# S9 - Capacity Clustering - Development of Cluster Dashboards
# July 19, 2015
# Author: Troy James R Palanca

# Libraries ---------------------------------------------------------------
library(maptools)
library(ggplot2)
library(dplyr)
library(reshape2)
library(extrafont)
library(scales)
library(stringr)
library(grid)
library(rgeos)
library(gridExtra)
library(png)
loadfonts(quiet = TRUE)

# Data --------------------------------------------------------------------
load("Data/D7 - Cluster Profiles.RData")
load("Data/D3 - CityMuni Data.RData")
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

# Original Order of Clusters
orig_clust_elem.ls <- as.numeric(schools_elem_profiles_formatted.dt$cluster.num)
orig_clust_seco.ls <- as.numeric(schools_seco_profiles_formatted.dt$cluster.num)

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
eligibleschools.dt <- enrolment.dt %>% group_by(school.id) %>% summarise(count = n()) %>%
  mutate(secondary = school.id >= 300000) %>%
  mutate(complete = (secondary == F & count == 56) | (secondary == T & count == 32)) %>%
  filter(complete == T)
enrolment.dt <- enrolment.dt[enrolment.dt$school.id %in% eligibleschools.dt$school.id, 1:5]
rm(eligibleschools.dt)

# Computation of survival rates (per cluster) -------------------------------------------

# Compute dropout rates per school
enrolment.dt <- enrolment.dt %>%
  mutate(cohort = as.numeric(factor(paste(school.id, year - as.numeric(grade)))))
survival_elem.dt <- enrolment.dt %>%
  filter(as.numeric(grade) %in% 2:7) %>%
  group_by(cohort, gender) %>% arrange(year) %>%
  mutate(dropout = enrollment/lag(enrollment) - 1) %>%
  filter(!is.na(dropout) & !is.infinite(dropout)) %>%
  left_join(schools_elem.dt %>% select(school.id, cluster)) %>%
  filter(!is.na(cluster)) %>%
  group_by(year, gender, grade, cluster) %>%
  summarise(
    mean.dropout = mean(dropout),
    se.dropout = sd(dropout)/sqrt(n()),
    ci99.upper.dropout = mean.dropout + se.dropout * qnorm(0.99),
    ci99.lower.dropout = mean.dropout - se.dropout * qnorm(0.99),
    ci95.upper.dropout = mean.dropout + se.dropout * qnorm(0.95),
    ci95.lower.dropout = mean.dropout - se.dropout * qnorm(0.95),
    ci90.upper.dropout = mean.dropout + se.dropout * qnorm(0.90),
    ci90.lower.dropout = mean.dropout - se.dropout * qnorm(0.90),
    ci80.upper.dropout = mean.dropout + se.dropout * qnorm(0.80),
    ci80.lower.dropout = mean.dropout - se.dropout * qnorm(0.80)) %>%
  ungroup()
survival_seco.dt <- enrolment.dt %>%
  filter(as.numeric(grade) %in% 8:11) %>%
  group_by(cohort, gender) %>% arrange(year) %>%
  mutate(dropout = enrollment/lag(enrollment) - 1) %>%
  filter(!is.na(dropout) & !is.infinite(dropout)) %>%
  left_join(schools_seco.dt %>% select(school.id, cluster)) %>%
  filter(!is.na(cluster)) %>%
  group_by(year, gender, grade, cluster) %>%
  summarise(
    mean.dropout = mean(dropout),
    se.dropout = sd(dropout)/sqrt(n()),
    ci99.upper.dropout = mean.dropout + se.dropout * qnorm(0.99),
    ci99.lower.dropout = mean.dropout - se.dropout * qnorm(0.99),
    ci95.upper.dropout = mean.dropout + se.dropout * qnorm(0.95),
    ci95.lower.dropout = mean.dropout - se.dropout * qnorm(0.95),
    ci90.upper.dropout = mean.dropout + se.dropout * qnorm(0.90),
    ci90.lower.dropout = mean.dropout - se.dropout * qnorm(0.90),
    ci80.upper.dropout = mean.dropout + se.dropout * qnorm(0.80),
    ci80.lower.dropout = mean.dropout - se.dropout * qnorm(0.80)) %>%
  ungroup()

# Generate initial rows for cumulative dropout rates
survival_elem_append.dt <-
  data.frame(grade = "Grade 1",
             year = rep(2013:2015, each = 12),
             gender = rep(c("Female", "Male"), 6*3),
             cluster = rep(rep(1:6, each = 2),3),
             mean.dropout = 0,
             se.dropout = 0,
             ci99.upper.dropout = 0,
             ci99.lower.dropout = 0,
             ci95.upper.dropout = 0,
             ci95.lower.dropout = 0,
             ci90.upper.dropout = 0,
             ci90.lower.dropout = 0,
             ci80.upper.dropout = 0,
             ci80.lower.dropout = 0,
             mean.survival = 1,
             ci99.upper.survival = 1,
             ci99.lower.survival = 1,
             ci95.upper.survival = 1,
             ci95.lower.survival = 1,
             ci90.upper.survival = 1,
             ci90.lower.survival = 1,
             ci80.upper.survival = 1,
             ci80.lower.survival = 1)
survival_seco_append.dt <-
  data.frame(grade = "Year 1 / Grade 7",
             year = rep(2013:2015, each = 12),
             gender = rep(c("Female", "Male"), 6*3),
             cluster = rep(rep(1:6, each = 2),3),
             mean.dropout = 0,
             se.dropout = 0,
             ci99.upper.dropout = 0,
             ci99.lower.dropout = 0,
             ci95.upper.dropout = 0,
             ci95.lower.dropout = 0,
             ci90.upper.dropout = 0,
             ci90.lower.dropout = 0,
             ci80.upper.dropout = 0,
             ci80.lower.dropout = 0,
             mean.survival = 1,
             ci99.upper.survival = 1,
             ci99.lower.survival = 1,
             ci95.upper.survival = 1,
             ci95.lower.survival = 1,
             ci90.upper.survival = 1,
             ci90.lower.survival = 1,
             ci80.upper.survival = 1,
             ci80.lower.survival = 1)

# Combine and compute cumulative dropout rates
survival_elementary.dt <- survival_elem.dt %>%
  mutate(mean.survival = 1 + mean.dropout,
         ci99.upper.survival = 1 + ci99.upper.dropout,
         ci99.lower.survival = 1 + ci99.lower.dropout,
         ci95.upper.survival = 1 + ci95.upper.dropout,
         ci95.lower.survival = 1 + ci95.lower.dropout,
         ci90.upper.survival = 1 + ci90.upper.dropout,
         ci90.lower.survival = 1 + ci90.lower.dropout,
         ci80.upper.survival = 1 + ci80.upper.dropout,
         ci80.lower.survival = 1 + ci80.lower.dropout) %>%
  rbind(survival_elem_append.dt) %>%
  arrange(year, cluster, gender, grade) %>%
  group_by(year, cluster, gender) %>%
  mutate(cum.mean.survival = cumprod(mean.survival),
         cum.ci99.upper.survival = cumprod(ci99.upper.survival),
         cum.ci99.lower.survival = cumprod(ci99.lower.survival),
         cum.ci95.upper.survival = cumprod(ci95.upper.survival),
         cum.ci95.lower.survival = cumprod(ci95.lower.survival),
         cum.ci90.upper.survival = cumprod(ci90.upper.survival),
         cum.ci90.lower.survival = cumprod(ci90.lower.survival),
         cum.ci80.upper.survival = cumprod(ci80.upper.survival),
         cum.ci80.lower.survival = cumprod(ci80.lower.survival)) %>%
  ungroup()

survival_secondary.dt <- survival_seco.dt %>%
  mutate(mean.survival = 1 + mean.dropout,
         ci99.upper.survival = 1 + ci99.upper.dropout,
         ci99.lower.survival = 1 + ci99.lower.dropout,
         ci95.upper.survival = 1 + ci95.upper.dropout,
         ci95.lower.survival = 1 + ci95.lower.dropout,
         ci90.upper.survival = 1 + ci90.upper.dropout,
         ci90.lower.survival = 1 + ci90.lower.dropout,
         ci80.upper.survival = 1 + ci80.upper.dropout,
         ci80.lower.survival = 1 + ci80.lower.dropout) %>%
  rbind(survival_seco_append.dt) %>%
  arrange(year, cluster, gender, grade) %>%
  group_by(year, cluster, gender) %>%
  mutate(cum.mean.survival = cumprod(mean.survival),
         cum.ci99.upper.survival = cumprod(ci99.upper.survival),
         cum.ci99.lower.survival = cumprod(ci99.lower.survival),
         cum.ci95.upper.survival = cumprod(ci95.upper.survival),
         cum.ci95.lower.survival = cumprod(ci95.lower.survival),
         cum.ci90.upper.survival = cumprod(ci90.upper.survival),
         cum.ci90.lower.survival = cumprod(ci90.lower.survival),
         cum.ci80.upper.survival = cumprod(ci80.upper.survival),
         cum.ci80.lower.survival = cumprod(ci80.lower.survival)) %>%
  ungroup()

# Clean up
rm(survival_elem.dt, survival_elem_append.dt,
   survival_seco.dt, survival_seco_append.dt)

# Computation of survival rates (national) -----------------------------

# Compute dropout rates
enrolment.dt <- enrolment.dt %>%
  mutate(cohort = as.numeric(factor(paste(year - as.numeric(grade)))))

survival_elem_append.dt <-
  data.frame(grade = "Grade 1",
             year = rep(2013:2015, each = 2),
             gender = rep(c("Female", "Male"), 3),
             mean.dropout = 0,
             mean.survival = 1)

survival_elem_national.dt <- enrolment.dt %>%
  filter(as.numeric(grade) %in% 2:7) %>%
  group_by(year, gender, grade, cohort) %>%
  summarise(enrollment = sum(enrollment)) %>%
  ungroup() %>%
  arrange(cohort, gender, grade) %>%
  group_by(cohort, gender) %>%
  mutate(dropout = enrollment/lag(enrollment) - 1) %>%
  filter(!is.na(dropout) & !is.infinite(dropout)) %>%
  group_by(year, gender, grade) %>%
  summarise(
    mean.dropout = mean(dropout),
    mean.survival = 1 + mean.dropout) %>%
  ungroup() %>%
  rbind(survival_elem_append.dt) %>%
  arrange(year, gender, grade) %>%
  group_by(year, gender) %>%
  mutate(cum.mean.survival = cumprod(mean.survival)) %>%
  ungroup()

survival_seco_append.dt <-
  data.frame(grade = "Year 1 / Grade 7",
             year = rep(2013:2015, each = 2),
             gender = rep(c("Female", "Male"), 3),
             mean.dropout = 0,
             mean.survival = 1)

survival_seco_national.dt <- enrolment.dt %>%
  filter(as.numeric(grade) %in% 8:11) %>%
  group_by(year, gender, grade, cohort) %>%
  summarise(enrollment = sum(enrollment)) %>%
  ungroup() %>%
  arrange(cohort, gender, grade) %>%
  group_by(cohort, gender) %>%
  mutate(dropout = enrollment/lag(enrollment) - 1) %>%
  filter(!is.na(dropout) & !is.infinite(dropout)) %>%
  group_by(year, gender, grade) %>%
  summarise(
    mean.dropout = mean(dropout),
    mean.survival = 1 + mean.dropout) %>%
  ungroup() %>%
  rbind(survival_seco_append.dt) %>%
  arrange(year, gender, grade) %>%
  group_by(year, gender) %>%
  mutate(cum.mean.survival = cumprod(mean.survival)) %>%
  ungroup()

# Clean up
rm(survival_elem_append.dt,
   survival_seco_append.dt,
   enrolment.dt,
   citymuni.dt)

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
PlotSurvivalRates <- function (cluster.data, national.data, cluster.num,
                               original.cluster.numbers) {
  # Perform means transform
  cluster.data <- cluster.data %>%
    group_by(year, grade, cluster) %>%
    summarise_each(funs(mean), -gender) %>%
    filter(cluster == cluster.num)
  national.data <- national.data %>%
    group_by(year, grade) %>%
    summarise_each(funs(mean), -gender)
  ggplot(cluster.data, aes(x = grade, group = year)) +
    facet_wrap(~year, ncol = 3) +
    geom_ribbon(aes(ymin = cum.ci99.lower.survival,
                    ymax = cum.ci99.upper.survival),
                alpha = 0.2,
                fill = ggplot_colors(6)[which(original.cluster.numbers == cluster.num)]) +
    geom_line(aes(y = cum.mean.survival),
              color = ggplot_colors(6)[which(original.cluster.numbers == cluster.num)]) +
    geom_line(data = national.data,
              aes(y = cum.mean.survival),
              color = "black",
              linetype = 2) +
    geom_text(data = cluster.data %>% ungroup() %>%
                filter(as.numeric(grade) == max(as.numeric(grade))),
              aes(label = percent(cum.mean.survival),
                  y = cum.mean.survival),
              size = 4,
              color = ggplot_colors(6)[which(original.cluster.numbers ==cluster.num)]) +
    geom_text(data = national.data %>% ungroup() %>%
                filter(as.numeric(grade) == max(as.numeric(grade))),
              aes(label = percent(cum.mean.survival),
                  y = cum.mean.survival),
              size = 4,
              color = "black") +
    ggtitle("Comparative Survival Rates\n") +
    scale_y_continuous(labels = percent,
                       name = "Cumulative Survival Rate (%)") +
    theme_minimal(base_family = "Open Sans") +
    coord_cartesian(ylim = c(0,1)) +
    theme(axis.text.x = element_text(angle = 90),
          axis.title.x = element_blank(),
          plot.title = element_text(face = "bold", size = 14, hjust = 0,
                                    family = "Raleway"),
          axis.title.y = element_text(face = "bold", size = 12))
}

# Geographical Distribution Plot ----------------------------------------
PlotProvincialMap <- function (data, shp, cluster.num,
                               original.cluster.numbers) {
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
                        low = "gray95", high = ggplot_colors(6)[
                          which(original.cluster.numbers ==cluster.num)],
                        na.value = "gray98",
                        name = "% of Schools") +
    coord_map() +
    ggtitle("Geographic Distribution") +
    map.thm +
    theme(plot.title = element_text(size = 16,
                                    face = "bold",
                                    family = "Raleway",
                                    hjust = 0),
          text = element_text(family = "Open Sans"),
          legend.position = "bottom")
}

# Distribution Chart ------------------------------------------------------
PlotDistributionChart <- function (data) {
  ggplot(data %>%
           group_by(cluster.name, cluster) %>%
           summarise(count = n()) %>%
           ungroup() %>%
           mutate(rel.count = round(count/sum(count)*100, 0),
                  cum.rel.count = cumsum(rel.count),
                  text.pos = cum.rel.count - rel.count/2,
                  label = paste0(cluster.name, "\n", comma(count),
                                 " (", rel.count,"%)")),
         aes(x = 1, y = rel.count, color = cluster.name, fill = cluster.name)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = 1,
                  y = text.pos, label = label),
              color = "black",
              family = "Open Sans",
              size = 5) +
    coord_flip(ylim = c(-10, 110)) +
    map.thm +
    theme(legend.position = "none")
}

# Cluster Panel Construction --------------------------------------------------
ClusterPanel<- function(capacity.data,
                        survival.cluster.data,
                        survival.national.data,
                        cluster.profile.data,
                        original.cluster.numbers,
                        shp, clust) {

  # Create title grob
  title.gg <- arrangeGrob(
    textGrob(
      label = cluster.profile.data$Cluster[cluster.profile.data$cluster.num == clust][1],
      gp = gpar(fontfamily = "Raleway",
                fontsize = 24,
                fontface = "bold"),
      just = "left",
      x = 0.05, y = 0.5
    ),
    textGrob(
      label = str_wrap(str_replace_all(
        cluster.profile.data$Description[cluster.profile.data$cluster.num == clust][1],
        "\n", " "), 75),
      gp = gpar(fontfamily = "Open Sans",
                fontsize = 14,
                lineheight = 0.8),
      just = "left",
      x = 0.05, y = 0.5
    ),
    heights = c(0.6, 0.4)
  )

  # Define themes
  footer.gp <- gpar(
    fontfamily = "Open Sans",
    lineheight = 0.8,
    fontsize = 9
  )

  # Load up main plots
  geographical_distribution.gg <- PlotProvincialMap(capacity.data, shp, clust,
                                                    original.cluster.numbers)
  metric_means.gg <- PlotMetrics(capacity.data, clust)
  survival_rate.gg <- PlotSurvivalRates(survival.cluster.data, survival.national.data, clust,
                                        original.cluster.numbers)

  # Define plot areas
  title.grob <- arrangeGrob(title.gg)
  left.grob <- arrangeGrob(geographical_distribution.gg)
  right.grob <- arrangeGrob(
    arrangeGrob(
      metric_means.gg,
      textGrob(
        label = "Red line indicates system-wide average.
Data is winsorized at the 1st and 99th percentile.",
        gp = footer.gp,
        just = "left", x = 0
      ),
      heights = c(0.9, 0.1)
    ),
    arrangeGrob(
      survival_rate.gg,
      textGrob(
        label = "Dotted line indicates system-wide average.
Solid line and shaded area indicate cluster-wide average with 99% confidence.",
        gp = footer.gp,
        just = "left", x = 0
      ),
      heights = c(0.9, 0.1)
    )
  )
  plot.grob <- arrangeGrob(left.grob, right.grob, ncol = 2,
                           widths = c(0.45,0.55))

  # Return Plot
  arrangeGrob(title.grob,
              plot.grob,
              ncol = 1,
              heights = c(0.15,0.85))
}

# Final Dashboard Construction --------------------------------------------

PlotDashboard <- function(capacity.data,
                          survival.cluster.data,
                          survival.national.data,
                          cluster.profile.data,
                          shp,
                          original.cluster.numbers,
                          title = "Title") {

  # Generate Panels
  panels.ls <- lapply(
    original.cluster.numbers,
    ClusterPanel,
    capacity.data = capacity.data,
    survival.cluster.data = survival.cluster.data,
    survival.national.data = survival.national.data,
    cluster.profile.data = cluster.profile.data,
    original.cluster.numbers = original.cluster.numbers,
    shp = shp
  )

  # Generate title grob with distribution chart
  title.grob <-
    arrangeGrob(
      arrangeGrob(
        textGrob(
          label = paste("   ", title),
          gp = gpar(
            fontfamily = "Raleway",
            fontsize = 40,
            fontface = "bold"
          ),
          just = "left",
          x = 0
        ),
        rasterGrob(readPNG("Data/JDT Watermark.png")),
        ncol = 2,
        widths = c(0.6, 0.4)
      ),
      PlotDistributionChart(data = capacity.data),
      heights = c(0.6, 0.4),
      ncol = 1
    )

  plots.grob <-
    arrangeGrob(
      arrangeGrob(
        panels.ls[[1]],
        panels.ls[[2]],
        ncol = 2
      ),
      arrangeGrob(
        panels.ls[[3]],
        panels.ls[[4]],
        ncol = 2
      ),
      arrangeGrob(
        panels.ls[[5]],
        panels.ls[[6]],
        ncol = 2
      ),
      ncol = 1
    )

  footer.grob <-
    textGrob(x = 0, just = "left",
             label = paste(c(
               "         Troy James R Palanca | www.jumbodumbothoughts.com",
               "         Data Sources: Department of Education, PhilGIS.org",
               "         Disclaimer: Content is provided for information purposes only."),
               collapse = "\n"),
             gp = gpar(fontfamily = "Open Sans",
                       fontsize = 14))

  return(
    arrangeGrob(
      title.grob,
      plots.grob,
      footer.grob,
      ncol = 1,
      heights = c(0.1,0.85,0.05)
    )
  )
}


# Output to PNG file (Elementary Schools)

png(bg = "gray98", file = "Output/O17A - Elementary Schools Capacity Dashboard.png",
    width = 6000, height = 8000, res = 300)
print(
  PlotDashboard(
    capacity.data = schools_elem.dt,
    survival.cluster.data = survival_elementary.dt,
    survival.national.data = survival_elem_national.dt,
    cluster.profile.data = schools_elem_profiles_formatted.dt,
    shp = PHprov.shp,
    original.cluster.numbers = orig_clust_elem.ls,
    title = "Elementary Schools Capacity Clusters")
)
dev.off()

# Output to PNG file (Secondary Schools)

png(bg = "gray98", file = "Output/O17B - Secondary Schools Capacity Dashboard.png",
    width = 6000, height = 8000, res = 300)
print(
  PlotDashboard(
    capacity.data = schools_seco.dt,
    survival.cluster.data = survival_secondary.dt,
    survival.national.data = survival_seco_national.dt,
    cluster.profile.data = schools_seco_profiles_formatted.dt,
    shp = PHprov.shp,
    original.cluster.numbers = orig_clust_seco.ls,
    title = "Secondary Schools Capacity Clusters")
)
dev.off()

