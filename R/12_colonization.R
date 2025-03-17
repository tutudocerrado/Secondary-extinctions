
# Colonization ------------------------------------------------------------

# Function to calculate colonization

colonization <- function(x) {
  ifelse(x[1] == 1 & x[2] == 1, 0, 
         ifelse(x[1] == 1 & x[2] == 0, 0, 
                ifelse(x[1] == 0 & x[2] == 1, 1, 
                       ifelse(x[1] == 0 & x[2] == 0, 0, NA))))
}

# Directories -------------------------------------------------------------

dir.create("Colonization")
dir.create("Colonization/Plants")
dir.create("Colonization/Pollinators")

# Plants' colonization ----------------------------------------------------

plant_files <- list.files("./ENM/Plants/Current")

for (i in 1:length(plant_files)) {
  plant_current <- rast(paste("./ENM/Plants/Current", plant_files[i], sep = "/"))
  plant_future <- rast(paste("./ENM/Plants/Future", plant_files[i], sep = "/"))
  plant_colonization <- c(plant_current, plant_future)
  plant_colonization <- app(plant_colonization, fun = colonization)
  writeRaster(plant_colonization, 
              paste("./Colonization/Plants", 
                    plant_files[i], 
                    sep = "/"), 
              overwrite = TRUE)
  print(i)
}

for (i in 1:length(plant_files)) {
  colonization_dist <- rast(paste("./Colonization/Plants", plant_files[i], sep = "/"))
  if (i == 1) {
    plant_colonization <- colonization_dist
  } else {
    plant_colonization <- app(c(plant_colonization, colonization_dist), sum)
  }
  print(i)
}

writeRaster(plant_colonization, 
            "./Results/rasters/plant_colonization.tif", 
            overwrite = TRUE)

# Pollinators' colonization -----------------------------------------------

pollinator_files <- list.files("./ENM/Pollinators/Current")

for (i in 1:length(pollinator_files)) {
  pollinator_current <- rast(paste("./ENM/Pollinators/Current", pollinator_files[i], sep = "/"))
  pollinator_future <- rast(paste("./ENM/Pollinators/Future", pollinator_files[i], sep = "/"))
  pollinator_colonization <- c(pollinator_current, pollinator_future)
  pollinator_colonization <- app(pollinator_colonization, fun = colonization)
  writeRaster(pollinator_colonization, 
              paste("./Colonization/Pollinators", 
                    pollinator_files[i], 
                    sep = "/"), 
              overwrite = TRUE)
  print(i)
}

for (i in 1:length(pollinator_files)) {
  colonization_dist <- rast(paste("./Colonization/Pollinators", pollinator_files[i], sep = "/"))
  if (i == 1) {
    pollinator_colonization <- colonization_dist
  } else {
    pollinator_colonization <- app(c(pollinator_colonization, colonization_dist), sum)
  }
  print(i)
}

writeRaster(pollinator_colonization, 
            "./Results/rasters/pollinator_colonization.tif", 
            overwrite = TRUE)

# Total colonization ------------------------------------------------------

colonization <- c(plant_colonization, pollinator_colonization)
colonization <- app(colonization, sum)
writeRaster(colonization, 
            "./Results/rasters/colonization.tif", 
            overwrite = TRUE)

# Removing unused objects -------------------------------------------------

rm(colonization_dist, colonization, plant_current, plant_files, 
   plant_future, pollinator_current, pollinator_future, pollinator_files, i)

# Colonization maps by groups ---------------------------------------------

plant_colonization <- as.data.frame(plant_colonization, xy = TRUE)

plant_colonization_map <- ggplot(data = plant_colonization, 
                                 aes(x = x, 
                                     y = y, 
                                     fill = sum))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  ggtitle("Plants' colonization") +
  scale_fill_viridis_c(name = "Colonization") +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = "a", 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18))

pollinator_colonization <- as.data.frame(pollinator_colonization, xy = TRUE)

pollinator_colonization_map <- ggplot(data = pollinator_colonization, 
                                      aes(x = x, 
                                      y = y, 
                                      fill = sum))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  ggtitle("Pollinators' colonization") +
  scale_fill_viridis_c(name = "Colonization") +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = "b", 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18))


colonization_by_groups <- grid.arrange(plant_colonization_map, 
                                       pollinator_colonization_map, ncol = 2)



ggsave(plot = colonization_by_groups, 
       filename = "./Results/colonization_groups.tif",
       dpi = 300,
       width = 7.2,
       height = 4.5)

rm(colonization_by_groups, plant_colonization, plant_colonization_map, pollinator_colonization,
   pollinator_colonization_map)
