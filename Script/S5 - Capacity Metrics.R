# Data Analytics for Education
# S5 - Capacity Metrics
# July 10, 2015
# Author: Troy James R Palanca

# Libraries ---------------------------------------------------------------
library(dplyr)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
library(extrafont)
library(rgl)
library(png)
loadfonts(quiet = T)

# Data --------------------------------------------------------------------
load("Data/D4 - Schools Data Expanded.RData")
load("Data/D1 - Enrollment Data.RData")
imconvertstring <-"\"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert.exe\" -delay 1x%d %s*.png %s.%s"

# Metric Construction ------------------------------------------------------

enrolment.dt <- enrolment.dt %>% group_by(school.id, year) %>%
  summarise(enrolment = sum(enrollment)) %>% ungroup() %>%
  dcast(school.id ~ year) %>%
  rename(enrollment.2012 = `2012`,
         enrollment.2013 = `2013`,
         enrollment.2014 = `2014`,
         enrollment.2015 = `2015`)

schools.dt <- schools.dt %>% left_join(enrolment.dt)
rm(enrolment.dt)

schools.dt <- schools.dt %>% mutate(
  all.teacher.ratio =
    (teachers.instructor + teachers.mobile + teachers.regular + teachers.sped)/
    enrollment.2014,
  regular.teacher.ratio =
    (teachers.regular + teachers.sped) / enrollment.2014,
  academic.room.ratio =
    (rooms.standard.academic + rooms.nonstandard.academic) / enrollment.2013,
  standard.room.ratio =
    (rooms.standard.academic) / enrollment.2013,
  full.room.ratio =
    (rooms.standard.academic + rooms.standard.unused +
     rooms.nonstandard.academic + rooms.nonstandard.unused) / enrollment.2013,
  mooe.ratio = school.mooe / enrollment.2015
)

# Data Cleaning ------------------------------------------------

# Remove schools with zero rooms or teachers.
schools.dt <- schools.dt %>%
  filter(all.teacher.ratio != 0,
         full.room.ratio != 0,
         mooe.ratio != 0)

# Makes sure that only schools with complete capacity metrics are included in the analysis
schools.dt <- schools.dt %>%
  filter(is.na(all.teacher.ratio) + is.na(regular.teacher.ratio) +
           is.na(academic.room.ratio) + is.na(standard.room.ratio) +
           is.na(full.room.ratio) + is.na(mooe.ratio) == 0,
         is.infinite(all.teacher.ratio) + is.infinite(regular.teacher.ratio) +
           is.infinite(academic.room.ratio) + is.infinite(standard.room.ratio) +
           is.infinite(full.room.ratio) + is.infinite(mooe.ratio) == 0)

# Exploratory Plots ----------------------------------------------------------

histtheme.tm <-
  theme_minimal() +
  theme(text = element_text(family = "Open Sans"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.text.x = element_text(size = 6))

reciprocal <- function(x) 1/x

png("Output/O19A - Capacity Metrics (Unlogged).png", width = 1200, height = 800, res = 150)
grid.arrange(
  ggplot(schools.dt, aes(x = all.teacher.ratio)) +
    geom_histogram(color = NA, fill = "darkgreen") +
    geom_rug() + xlab("Student to Teacher Ratio") +
    histtheme.tm,
  ggplot(schools.dt, aes(x = regular.teacher.ratio)) +
    geom_histogram(color = NA, fill = "darkgreen") +
    geom_rug() + xlab("Student to Regular Teacher Ratio") +
    histtheme.tm,
  ggplot(schools.dt, aes(x = mooe.ratio)) +
    geom_histogram(color = NA, fill = "darkblue") +
    geom_rug() + xlab("MOOE to Student Ratio") +
    histtheme.tm,
  ggplot(schools.dt, aes(x = academic.room.ratio)) +
    geom_histogram(color = NA, fill = "darkorange") +
    geom_rug() + xlab("Student to Academic Room Ratio") +
    histtheme.tm,
  ggplot(schools.dt, aes(x = standard.room.ratio)) +
    geom_histogram(color = NA, fill = "darkorange") +
    geom_rug() + xlab("Student to Standard Room Ratio") +
    histtheme.tm,
  ggplot(schools.dt, aes(x = full.room.ratio)) +
    geom_histogram(color = NA, fill = "darkorange") +
    geom_rug() + xlab("Student to Room Ratio") +
    histtheme.tm,
  ncol = 3, main = textGrob("\nCapacity Metrics\n",
                            gp = gpar(fontfamily = "Raleway", fontface = "bold")))
dev.off()

# Many positive outliers, logarithmic transformation will unskew the distributions
# (except MOOE per student which is still positively skewed).

# Reversed the Teacher/Student and Room/Student ratios to produce more intuitive labels

# Log Transformed Charts --------------------------------------------------

png("Output/O19B - Capacity Metrics (Logged).png", width = 1200, height = 800, res = 150)
grid.arrange(
  ggplot(schools.dt, aes(x = all.teacher.ratio)) +
    geom_histogram(color = NA, fill = "darkgreen") +
    geom_rug() + xlab("Student to Teacher Ratio") +
    scale_x_log10(labels = reciprocal, breaks = 1/c(1,10,15,25,50,75,100,250,1000)) +
    histtheme.tm,
  ggplot(schools.dt, aes(x = regular.teacher.ratio)) +
    geom_histogram(color = NA, fill = "darkgreen") +
    geom_rug() + xlab("Student to Regular Teacher Ratio") +
    scale_x_log10(labels = reciprocal, breaks = 1/c(1,10,15,25,50,75,100,250,1000)) +
    histtheme.tm,
  ggplot(schools.dt, aes(x = mooe.ratio)) +
    geom_histogram(color = NA, fill = "darkblue") +
    geom_rug() + xlab("MOOE to Student Ratio") +
    scale_x_log10(breaks = c(100, 500, 1000, 3000, 7000, 10000)) +
    histtheme.tm,
  ggplot(schools.dt, aes(x = academic.room.ratio)) +
    geom_histogram(color = NA, fill = "darkorange") +
    geom_rug() + xlab("Student to Academic Room Ratio") +
    scale_x_log10(labels = reciprocal, breaks = 1/c(1,10,15,25,50,75,100,250,500)) +
    histtheme.tm,
  ggplot(schools.dt, aes(x = standard.room.ratio)) +
    geom_histogram(color = NA, fill = "darkorange") +
    geom_rug() + xlab("Student to Standard Room Ratio") +
    scale_x_log10(labels = reciprocal, breaks = 1/c(1,10,15,25,50,75,100,250,500)) +
    histtheme.tm,
  ggplot(schools.dt, aes(x = full.room.ratio)) +
    geom_histogram(color = NA, fill = "darkorange") +
    geom_rug() + xlab("Student to Room Ratio") +
    scale_x_log10(labels = reciprocal, breaks = 1/c(1,10,15,25,50,75,100,250,500)) +
    histtheme.tm,
  ncol = 3, main = textGrob("\nCapacity Metrics (Log 10)\n",
                            gp = gpar(fontfamily = "Raleway", fontface = "bold")))
dev.off()

# Summary of Capacity Metrics ---------------------------------------------

boxplot.tm <- theme_minimal() +
  theme(text = element_text(family = "Open Sans"),
        axis.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 12, face = "bold"),
        plot.background = element_rect(color = NA, fill = "gray98"))

metricplots.gg <-
  arrangeGrob(
    ggplot(schools.dt %>% select(`Students per \nTeacher\n(Full Capacity)` = all.teacher.ratio,
                                 `Students per \nRegular\nTeacher` = regular.teacher.ratio,
                                 school.classification) %>%
             melt(variable.name = "metric", value.name = "value"),
           aes(x = metric, y = value + min(value[value != min(value)]))) +
      geom_boxplot(alpha = 0.5, aes(fill = school.classification,
                                    color = school.classification)) +
      geom_text(data = schools.dt %>% select(`Students per \nTeacher\n(Full Capacity)` =
                                               all.teacher.ratio,
                                             `Students per \nRegular\nTeacher` =
                                               regular.teacher.ratio,
                                             school.classification) %>%
                  melt(variable.name = "metric", value.name = "value") %>%
                  group_by(metric, school.classification) %>%
                  summarise_each(funs(median)) %>% ungroup(),
                aes(label = format(1/value, digits = 3), y = value,
                    x = as.numeric(as.factor(metric))+0.4*
                      as.numeric(as.factor(school.classification))-0.6),
                family = "Open Sans", size = 4, vjust = 0.1) +
      scale_y_log10(labels = reciprocal, breaks = 1/c(2,10,15,25,50,75,100,250,1000)) +
      scale_color_manual(name = "School Classification",
                         values = c("darkgreen", "darkblue")) +
      scale_fill_manual(name = "School Classification",
                        values = c("darkgreen", "darkblue")) +
      ggtitle("Teacher\nCapacity\n") + boxplot.tm,
    ggplot(schools.dt %>% select(`Students per\nAcademic\nRoom` = academic.room.ratio,
                                 `Students per\nStandard\nRoom` = standard.room.ratio,
                                 `Students per\nRoom\n(Full Capacity)` = full.room.ratio,
                                 school.classification) %>%
             melt(variable.name = "metric", value.name = "value"),
           aes(x = metric, y = value + min(value[value != min(value)]))) +
      geom_boxplot(alpha = 0.5, aes(color = school.classification, fill = school.classification)) +
      geom_text(data = schools.dt %>% select(`Students per\nAcademic\nRoom` = academic.room.ratio,
                                             `Students per\nStandard\nRoom` = standard.room.ratio,
                                             `Students per\nRoom\n(Full Capacity)` = full.room.ratio,
                                             school.classification) %>%
                  melt(variable.name = "metric", value.name = "value") %>%
                  group_by(metric, school.classification) %>%
                  summarise_each(funs(median)) %>% ungroup(),
                aes(label = format(1/value, digits = 3), y = value,
                    x = as.numeric(as.factor(metric))+0.4*
                      as.numeric(as.factor(school.classification))-0.6),
                family = "Open Sans", size = 4, vjust = 0.1) +
      scale_y_log10(labels = reciprocal, breaks = 1/c(1,10,15,25,50,75,100,250,500)) +
      scale_color_manual(name = "School Classification",
                         values = c("darkgreen", "darkblue")) +
      scale_fill_manual(name = "School Classification",
                        values = c("darkgreen", "darkblue")) +
      ggtitle("Room\nCapacity\n") + boxplot.tm,
    ggplot(schools.dt %>% select(`MOOE per\nStudent` = mooe.ratio,
                                 school.classification) %>%
             melt(variable.name = "metric", value.name = "value"),
           aes(x = metric, y = value + min(value[value != min(value)]))) +
      geom_boxplot(alpha = 0.5, aes(color = school.classification, fill = school.classification)) +
      geom_text(data = schools.dt %>% select(`MOOE per\nStudent` = mooe.ratio,
                                             school.classification) %>%
                  melt(variable.name = "metric", value.name = "value") %>%
                  group_by(metric, school.classification) %>%
                  summarise_each(funs(median)) %>% ungroup(),
                aes(label = format(value, digits = 3), y = value + 200,
                    x = as.numeric(as.factor(metric))+0.4*
                      as.numeric(as.factor(school.classification))-0.6),
                family = "Open Sans", size = 4) +
      scale_y_log10(breaks = c(100, 500, 1000, 3000, 7000, 10000)) +
      scale_color_manual(name = "School Classification",
                         values = c("darkgreen", "darkblue")) +
      scale_fill_manual(name = "School Classification",
                        values = c("darkgreen", "darkblue")) +
      ggtitle("Budgetary\nCapacity\n") +
      boxplot.tm + theme(legend.position = "right"),
    ncol = 3,
    widths = c(0.275 - 0.025/2, 0.375 - 0.025/2, 0.375))

metrictitle.grob <-
  arrangeGrob(
    arrangeGrob(
      textGrob(
        label = paste("Capacities and Constraints"),
        gp = gpar(
          fontfamily = "Raleway",
          fontsize = 16,
          fontface = "bold"
        ),
        just = "left",
        x = unit(0.05, "npc"),
        y = unit(0.35, "npc")
      ),
      textGrob(
        label = paste("Capacity metrics for the Philippine educational system, 2013-2015"),
        gp = gpar(
          fontfamily = "Open Sans",
          fontsize = 10,
          fontface = "italic"
        ),
        just = "left",
        x = unit(0.05, "npc"),
        y = unit(0.75, "npc")
      ),
      ncol = 1,
      heights = c(0.6, 0.4)
    ),
    rasterGrob(readPNG("Data/JDT Watermark.png")),
    ncol = 2,
    widths = c(0.6, 0.4)
  )

metricfooter.gg <-
  textGrob(x = unit(0.03, "npc"), just = "left",
           label = paste(c(
             "Troy James R Palanca | www.jumbodumbothoughts.com",
             "Data Source: Department of Education",
             "Disclaimer: Content is provided for information purposes only."),
             collapse = "\n"),
           gp = gpar(fontfamily = "Open Sans",
                     fontsize = 7,
                     lineheight = 0.8))

png("Output/O5 - Capacity Metrics.png",
    width = 12*300, height = 6*300,
    bg = "gray98",
    res = 350)
print(
  arrangeGrob(
    metrictitle.grob,
    metricplots.gg,
    metricfooter.gg,
    heights = c(0.15, 0.8, 0.05),
    ncol = 1
  )
)
dev.off()

rm(boxplot.tm, histtheme.tm)

# 3D Plot for Clusters ----------------------------------------------------

schools_elem.dt <- schools.dt %>% filter(school.classification == "Elementary")
schools_seco.dt <- schools.dt %>% filter(school.classification == "Secondary")

par3d(windowRect  = c(100, 100, 600, 600), family = "Open Sans", cex = 0.8)
plot3d(x = log10(schools_elem.dt$all.teacher.ratio),
                y = log10(schools_elem.dt$full.room.ratio),
                z = log10(schools_elem.dt$mooe.ratio),
                size = 1, col = "darkgreen", box = F,
                xlab = "Teachers", ylab = "Rooms", zlab = "Budget")
view3d(75, 15)
snapshot3d("Output/O6A - Elementary Capacity Plot.png", fmt = "png")
movie3d(spin3d(axis = c(0,0,1), rpm = 5), duration = 12,
        dir = paste(getwd(),"/Output", sep = ""),
        movie = "O6_-_Elementary_Capacity_Plot")

file.rename("Output/O6_-_Elementary_Capacity_Plot.gif",
            "Output/O6 - Elementary Capacity Plot.gif")

par3d(windowRect  = c(100, 100, 600, 600), family = "Open Sans", cex = 0.8)
plot3d(x = log10(schools_seco.dt$all.teacher.ratio),
       y = log10(schools_seco.dt$full.room.ratio),
       z = log10(schools_seco.dt$mooe.ratio),
       size = 1, col = "darkblue", box = F,
       xlab = "Teachers", ylab = "Rooms", zlab = "Budget")
view3d(75, 15)
snapshot3d("Output/O6B - Secondary Capacity Plot.png", fmt = "png")
movie3d(spin3d(axis = c(0,0,1), rpm = 5), duration = 12,
        dir = paste(getwd(),"/Output", sep = ""),
        movie = "O6_-_Secondary_Capacity_Plot")

file.rename("Output/O6_-_Secondary_Capacity_Plot.gif",
            "Output/O6 - Secondary Capacity Plot.gif")

rm(schools_elem.dt, schools_seco.dt)
rm(reciprocal)

# Save Out ----------------------------------------------------------------
save.image("Data/D5 - Capacity Data.RData")
