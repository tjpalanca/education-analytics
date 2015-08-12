# Data Analytics for Education
# S10 - Capacity Utilization Strategy
# August 10, 2015
# Author: Troy James R Palanca

# This script explores the effect of using nonstandard and vacant rooms.

# Libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(extrafont)
library(stringdist)
library(ggmap)
library(gridExtra)
library(png)
loadfonts(quiet = T)

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

map.thm <-
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(family = "Open Sans"),
        plot.title = element_text(hjust = 0, family = "Raleway", face = "bold"),
        plot.background = element_rect(color = NA, fill = "grey98"),
        legend.background = element_blank(),
        strip.text = element_text(size = 14))

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

# Check for residual blank values
sum(is.na(schools.dt$map.lat))
sum(is.na(schools.dt$map.lon))

rm(citymuni.dt, i, distances.vc)

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
                                  "Full Capacity"),18),
                   levels = str_wrap(c("Standard Capacity",
                                       "Effect of using non-standard rooms",
                                       "Effect of using vacant rooms",
                                       "Full Capacity"),18)),
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
            aes(x = phase,
                y = end,
                label = round(label,2)),
            vjust = -0.5, color = "white", family = "Open Sans") +
  geom_text(data = schools_roomuse_waterfall.dt[c(1,4),],
            aes(x = phase,
                y = upper,
                label = round(abs(label),2)),
            vjust = 1.2, color = "white", family = "Open Sans") +
  ylab("Students per Room") +
  scale_fill_manual(values = c("firebrick", "forestgreen")) +
  coord_cartesian(ylim = range(schools_roomuse.dt$value) * c(0.98, 1.02)) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Open Sans"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text.x = element_text(size = 11),
        panel.grid.major.x = element_blank())

# Maps of high need cities ------------------------------------------------

schools.dt <- schools.dt %>%
  mutate(use.nonstandard = academic.room.ratio - standard.room.ratio,
         use.vacant = full.room.ratio - academic.room.ratio)

schools_nonstandard.dt <- schools.dt %>% filter(use.nonstandard > 0)
schools_vacant.dt <- schools.dt %>% filter(use.vacant > 0)

rooms_nonstandard.gg <-
  ggmap(ggmap = PH.map,
      base_layer = ggplot(data = schools_nonstandard.dt,
                          aes(x = map.lon, y = map.lat))) +
  geom_point(aes(color = log10(use.nonstandard),
                 alpha = log10(use.nonstandard)),
             size = 1) +
  geom_density2d(data = schools_nonstandard.dt %>%
                   filter(use.nonstandard > quantile(use.nonstandard, 0.9)),
                 bins = 2, color = "red") +
  scale_color_gradient(low = "white", high = "darkblue",
                       labels = function(x) 10^x,
                       name = "Change in\nrooms per student") +
  coord_map(xlim = c(116.812721, 126.856628),
            ylim = c(4.46811, 21.23415)) +
  scale_alpha_continuous(guide = FALSE) +
  ggtitle("Nonstandard Room Use") +
  map.thm

rooms_vacant.gg <-
  ggmap(ggmap = PH.map,
      base_layer = ggplot(data = schools_vacant.dt,
                          aes(x = map.lon, y = map.lat))) +
  geom_point(aes(color = log10(use.vacant),
                 alpha = log10(use.vacant)),
             size = 1) +
  geom_density2d(data = schools_vacant.dt %>%
                   filter(use.vacant > quantile(use.vacant, 0.9)),
                 bins = 2, color = "red") +
  scale_color_gradient(low = "white", high = "darkred",
                       labels = function(x) 10^x,
                       name = "Change in\nrooms per student") +
  scale_alpha_continuous(guide = FALSE) +
  coord_map(xlim = c(116.812721, 126.856628),
            ylim = c(4.46811, 21.23415)) +
  ggtitle("Vacant Room Use") +
  map.thm

# Plot Arrangement and Output ---------------------------------------------
title.grob <-
  arrangeGrob(
    arrangeGrob(
      textGrob(
        label = paste("Ramshackle Rooms"),
        gp = gpar(
          fontfamily = "Raleway",
          fontsize = 20,
          fontface = "bold"
        ),
        just = "left",
        x = unit(0.05, "npc"),
        y = unit(0.4, "npc")
      ),
      textGrob(
        label = paste("Effect of using nonstandard and/or vacant rooms, 2013"),
        gp = gpar(
          fontfamily = "Open Sans",
          fontsize = 12,
          fontface = "italic"
        ),
        just = "left",
        x = unit(0.05, "npc"),
        y = unit(0.75, "npc")
      ),
      ncol = 1,
      heights = c(0.6, 0.4)
    ),
    rasterGrob(readPNG("Data/JDT Watermark.png"),
               just = "right", x = unit(1, "npc")),
    ncol = 2,
    widths = c(0.6, 0.4)
  )

footer.grob <-
  textGrob(x = unit(0.03, "npc"), just = "left",
           label = paste(c(
             "Troy James R Palanca | www.jumbodumbothoughts.com",
             "Data Source: Department of Education",
             "Disclaimer: Content is provided for information purposes only."),
             collapse = "\n"),
           gp = gpar(fontfamily = "Open Sans",
                     fontsize = 7,
                     lineheight = 0.8))

notes.grob <-
  textGrob(x = unit(0.03, "npc"), just = "left",
           label = paste(c(
             "Explanatory notes:",
             "1. Red lines indicate kernel density estimate for schools whose reliance on nonstandard/vacant rooms belong to the top 10%."),
             collapse = "\n"),
           gp = gpar(fontfamily = "Open Sans",
                     fontsize = 8,
                     lineheight = 0.8))

plots.grob <-
  arrangeGrob(
    arrangeGrob(
      rooms_waterfall.gg,
      notes.grob,
      footer.grob,
      ncol = 1,
      heights = c(0.9, 0.05, 0.05)),
    rooms_nonstandard.gg,
    rooms_vacant.gg,
    ncol = 3,
    widths = c(0.5, 0.25, 0.25)
  )

png("Output/O18 - Room Reallocation.png",
    width = 8000, height = 4000,
    bg = "gray98",
    res = 600)
grid.arrange(title.grob,
             plots.grob,
             heights = c(0.1,0.9),
             ncol = 1)
dev.off()

# Cleanup
rm(footer.grob, notes.grob, plots.grob, title.grob,
   rooms_nonstandard.gg, rooms_vacant.gg, rooms_waterfall.gg,
   schools_nonstandard.dt, schools_roomuse_waterfall.dt, schools_vacant.dt, schools_roomuse.dt)