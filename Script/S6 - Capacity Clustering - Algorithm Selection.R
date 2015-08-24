# Data Analytics for Education
# S6 - Capacity Clustering - Algorithm Selection
# July 16, 2015
# Author: Troy James R Palanca

# Libraries ---------------------------------------------------------------
library(dplyr)
library(rgl)
library(ggplot2)
library(reshape2)
library(extrafont)
loadfonts()

# Clustering Alternative 1 (Elementary) -----------------------------------
# Two steps: first k-means to cut down the number of observations to half (computational constraint),
# then hierarchical clustering on the centers on the centers of the k-means clusters.

# Load in elementary school data
load("Data/D5 - Capacity Data.RData")
schools_elem.dt <- schools.dt %>% filter(school.classification == "Elementary")
rm(schools.dt)

# Perform two-step clustering procedure
set.seed(721992)
elem_capacity.kc <- kmeans(scale(schools_elem.dt %>%
                                   select(all.teacher.ratio, full.room.ratio, mooe.ratio) %>%
                                   mutate_each(funs(log10))),
                             centers = round(nrow(schools_elem.dt)/2), iter.max = 100)

max(table(elem_capacity.kc$cluster)) # max 7 observations in a cluster. Minimal distortion.

# Create a plot to compare with original capacity metrics plot
par3d(windowRect  = c(100, 100, 600, 600), family = "Open Sans", cex = 0.8)
plot3d(x = (elem_capacity.kc$centers[,1]),
       y = (elem_capacity.kc$centers[,2]),
       z = (elem_capacity.kc$centers[,3]),
       size = 1, col = "darkgreen", box = F,
       xlab = "Teachers", ylab = "Rooms", zlab = "Budget")
movie3d(spin3d(axis = c(0,0,1), rpm = 5), duration = 12,
        dir = paste(getwd(),"/Output", sep = ""),
        movie = "condensedplot")
file.rename(from = "Output/condensedplot.gif", to = "Output/O7 - Condensed Plot.gif")

# Inspect plot and determine ideal number of clusters
elem_capacity.hc <- hclust(dist(elem_capacity.kc$centers), method = "ward.D")
plot(elem_capacity.hc, labels = F)
rect.hclust(elem_capacity.hc, k = 9, border = "red") # k = 9 produces balanced clusters.
elem_capacity.hc <- cutree(elem_capacity.hc, k = 9)

# Assign clusters to original plots
schools_elem.dt$k.cluster <- elem_capacity.kc$cluster
cluster.ref <- data.frame(cbind(k.cluster = 1:max(elem_capacity.kc$cluster), h.cluster = elem_capacity.hc))
schools_elem.dt <- schools_elem.dt %>% left_join(cluster.ref)
rm(elem_capacity.hc, elem_capacity.kc, cluster.ref)

# Plot clustered points
par3d(windowRect  = c(100, 100, 600, 600), family = "Open Sans", cex = 0.8)
plot3d(x = log10(schools_elem.dt$all.teacher.ratio),
       y = log10(schools_elem.dt$full.room.ratio),
       z = log10(schools_elem.dt$mooe.ratio),
       col = schools_elem.dt$h.cluster,
       size = 1, box = F,
       xlab = "Teachers", ylab = "Rooms", zlab = "Budget")
view3d(75, 15)
snapshot3d("Output/O20A - Capacity Clusters (Hierarchical).png", fmt = "png")
movie3d(spin3d(axis = c(0,0,1), rpm = 5), duration = 12,
        dir = paste(getwd(),"/Output", sep = ""),
        movie = "clusterplot")
file.rename(from = "Output/clusterplot.gif", to = "Output/O8A - Clustering Procedure 1.gif")

# Plot cluster profiles
ggsave("Output/O9A - Clustering Alternative 1 - Profiles.png",
       ggplot(schools_elem.dt %>% melt(id.vars = "h.cluster",
                                       measure.vars = c("all.teacher.ratio", "full.room.ratio",
                                                        "mooe.ratio")),
              aes(x = as.factor(h.cluster), y = value, color = as.factor(h.cluster))) +
         facet_wrap(~variable, ncol = 3, scales = "free") +
         geom_jitter(alpha = 0.05) +
         ggtitle("Alternative 1 - K-Means + Hierarchical Clustering\n") + xlab("Cluster") + ylab("") +
         scale_y_log10() +
         theme_minimal() +
         theme(text = element_text(family = "Open Sans"),
               plot.title = element_text(face = "bold", hjust = 0),
               legend.position = "none"))

# Clustering Alternative 2 (Elementary) --------------------------------------------------------
# One-step clustering: just k-means directly (with same k).

# Perform clustering
set.seed(72921992)
elem_capacity2.kc <- kmeans(scale(schools_elem.dt %>%
                                    select(all.teacher.ratio, full.room.ratio, mooe.ratio) %>%
                                    mutate_each(funs(log10))), centers = 9, iter.max = 100)
schools_elem.dt$k2.cluster <- elem_capacity2.kc$cluster
rm(elem_capacity2.kc)

# Plot clustered points
par3d(windowRect  = c(100, 100, 600, 600), family = "Open Sans", cex = 0.8)
plot3d(x = log10(schools_elem.dt$all.teacher.ratio),
       y = log10(schools_elem.dt$full.room.ratio),
       z = log10(schools_elem.dt$mooe.ratio),
       col = schools_elem.dt$k2.cluster,
       size = 1, box = F,
       xlab = "Teachers", ylab = "Rooms", zlab = "Budget")
table(schools_elem.dt$k2.cluster, schools_elem.dt$h.cluster)
view3d(75, 15)
snapshot3d("Output/O20B - Capacity Clusters (K-means).png", fmt = "png")
movie3d(spin3d(axis = c(0,0,1), rpm = 5), duration = 12,
        dir = paste(getwd(),"/Output", sep = ""),
        movie = "clusterplot")
file.rename(from = "Output/clusterplot.gif", to = "Output/O8B - Clustering Procedure 2.gif")

# Plot cluster profiles
ggsave("Output/O9B - Clustering Alternative 2 - Profiles.png",
ggplot(schools_elem.dt %>% melt(id.vars = "k2.cluster",
                                measure.vars = c("all.teacher.ratio", "full.room.ratio", "mooe.ratio")),
       aes(x = as.factor(k2.cluster), y = value, color = as.factor(k2.cluster))) +
  facet_wrap(~variable, ncol = 3, scales = "free") +
  geom_jitter(alpha = 0.05) +
  ggtitle("Alternative 2 - Direct K-Means\n") + xlab("Cluster") + ylab("") +
  scale_y_log10() +
  theme_minimal() +
  theme(text = element_text(family = "Open Sans"),
        plot.title = element_text(face = "bold", hjust = 0),
        legend.position = "none"))

# CONCLUSION --------------------------------------------------------------
# It seems that direct k-means is better, probably due to the low number of dimensions.
# For alternative 1, the clusters are still well defined. However, cluster 6 seems fuzzy.
# For alternative 2, the clusters are much more tightly defined, with clear separation.
