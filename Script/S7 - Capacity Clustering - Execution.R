# Data Analytics for Education
# S7 - Capacity Clustering - Execution
# July 19, 2015
# Author: Troy James R Palanca

# Libraries ---------------------------------------------------------------
library(dplyr)
library(rgl)
library(ggplot2)
library(reshape2)
library(extrafont)
library(gridExtra)
loadfonts(quiet = T)

# Data --------------------------------------------------------------------
load("Data/D5 - Capacity Data.RData")
schools_elem.dt <- schools.dt %>% filter(school.classification == "Elementary")
schools_seco.dt <- schools.dt %>% filter(school.classification == "Secondary")
rm(schools.dt)

# Cluster Number Determination --------------------------------------------

# Within Sum of Squares "Elbow" method

plotWSS <- function (data, max.num, title = "WSS Plot", ...) {
  result <- data.frame(clusters = 1L:max.num)
  for (i in 1:max.num) {
    set.seed(721992)
    result$wss[i] <- sum(kmeans(data, centers = i, ...)$withinss)
  }
  result$reduction <- paste0(round(-(result$wss / lag(result$wss) - 1)*100, 0), "%")
  ggplot(result, aes(x = clusters, y = wss)) +
    geom_path(aes(group = 1)) +
    geom_point() +
    ggtitle(title) +
    geom_text(aes(label = reduction), vjust = -1.5) +
    scale_x_continuous(breaks = result$clusters) +
    theme_minimal()
}

wss_elem.gg <-
  plotWSS(scale(schools_elem.dt %>% select(all.teacher.ratio,
                                           full.room.ratio,
                                           mooe.ratio) %>%
                  mutate_each(funs = funs(log10))), max.num = 12, iter.max = 1000,
          algorithm = "Lloyd", title = "WSS Plot - Elementary")


wss_seco.gg <-
  plotWSS(scale(schools_seco.dt %>% select(all.teacher.ratio,
                                           full.room.ratio,
                                           mooe.ratio) %>%
                  mutate_each(funs = funs(log10))), max.num = 12, iter.max = 1000,
          algorithm = "Lloyd", title = "WSS Plot - Secondary")

png("Output/O10 - WSS Plots.png", height = 400, width = 800)
grid.arrange(wss_elem.gg, wss_seco.gg, ncol = 2)
dev.off()

rm(plotWSS, wss_elem.gg, wss_seco.gg)

## Results from the WSS plot seem to be inconclusive. However, if we were to stop
## producing clusters at a point before marginal reduction in WSS < 10%, then 6
## clusters is ideal.

# Clustering --------------------------------------------------------------

k <- 6 # set number of clusters

# Perform kmeans clustering for both elementary and secondary schools
set.seed(721992)
schools_elem.dt$cluster <- factor((kmeans(scale(schools_elem.dt %>%
                                                  select(all.teacher.ratio,
                                                         full.room.ratio,
                                                         mooe.ratio) %>%
                                                  mutate_each(funs = funs(log10))),
                                          centers = k, iter.max = 1000))$cluster)

set.seed(721992)
schools_seco.dt$cluster <- factor((kmeans(scale(schools_seco.dt %>%
                                                  select(all.teacher.ratio,
                                                         full.room.ratio,
                                                         mooe.ratio) %>%
                                                  mutate_each(funs = funs(log10))),
                                          centers = k, iter.max = 1000))$cluster)

# Write Out ---------------------------------------------------------------
rm(k)
schools.dt <- rbind(schools_elem.dt, schools_seco.dt)
save.image("Data/D6 - Capacity Clustering.RData")




