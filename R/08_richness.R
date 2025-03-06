# Richness maps -----------------------------------------------------------

# Creating paths to access plants models ----------------------------------

files_plant <- list.files("./ENM/Plants/Current", pattern = ".tif")

path_plants_present <- list()

for (i in 1:length(files_plant)) {
  path_plants_present[[i]] <- paste("./ENM/Plants/Current", 
                                    files_plant[i], 
                                    sep = "/")
}

path_plants_future <- list()

for (i in 1:length(files_plant)) {
  path_plants_future[[i]] <- paste("./ENM/Plants/Future", 
                                   files_plant[i], 
                                   sep = "/")
}

rm(files_plant, i)

# Creating paths to access pollinators models -----------------------------

files_pollinator <- list.files("./ENM/Pollinators/Current", pattern = ".tif")

path_pollinators_present <- list()

for (i in 1:length(files_pollinator)) {
  path_pollinators_present[[i]] <- paste("./ENM/Pollinators/Current", 
                                         files_pollinator[i], 
                                         sep = "/")
}

path_pollinators_future <- list()

for (i in 1:length(files_pollinator)) {
  path_pollinators_future[[i]] <- paste("./ENM/Pollinators/Future", 
                                        files_pollinator[i], 
                                        sep = "/")
}

rm(files_pollinator, i)

# Current (Plants)

for (i in 1:length(path_plants_present)) {
  ENM_plant <- rast(path_plants_present[[i]])
  if (i == 1) {
    Plant_richness <- ENM_plant
  } else {
    stack_plants <- c(Plant_richness, ENM_plant)
    Plant_richness <- app(stack_plants, fun = sum)
  }
  print(i)
}

Richness <- list()

names(Plant_richness) <- "Plant_richness_present"
Plant_richness_present <- Plant_richness
Richness[[1]] <- Plant_richness_present

dir.create("./Results")
dir.create("./Results/rasters")

writeRaster(Plant_richness_present, 
            filename = "./Results/rasters/Plant_richness_current.tif", 
            overwrite = TRUE)

rm(Plant_richness, ENM_plant, stack_plants, i, Plant_richness_present)

# Future (Plants)

for (i in 1:length(path_plants_future)) {
  ENM_plant <- rast(path_plants_future[[i]])
  if (i == 1) {
    Plant_richness <- ENM_plant
  } else {
    stack_plants <- c(Plant_richness, ENM_plant)
    Plant_richness <- app(stack_plants, fun = sum)
  }
  print(i)
}


names(Plant_richness) <- "Plant_richness_future"
Plant_richness_future <- Plant_richness
Richness[[2]] <- Plant_richness_future

writeRaster(Plant_richness_future, 
            filename = "./Results/rasters/Plant_richness_future.tif", 
            overwrite = TRUE)


rm(Plant_richness, ENM_plant, stack_plants, i, Plant_richness_future)

# Pollinator richness -----------------------------------------------------

# Present (Pollinators)

for (i in 1:length(path_pollinators_present)) {
  ENM_pollinator <- rast(path_pollinators_present[[i]])
  if (i == 1) {
    Pollinator_richness <- ENM_pollinator
  } else {
    stack_pollinator <- c(Pollinator_richness, ENM_pollinator)
    Pollinator_richness <- app(stack_pollinator, fun = sum)
  }
  print(i)
}

names(Pollinator_richness) <- "Pollinator_richness_present"
Pollinator_richness_present <- Pollinator_richness
Richness[[3]] <- Pollinator_richness_present

writeRaster(Pollinator_richness_present, 
            filename = "./Results/rasters/Pollinator_richness_current.tif", 
            overwrite = TRUE)


rm(Pollinator_richness, ENM_pollinator, stack_pollinator, i, Pollinator_richness_present)

# Future (Pollinators)

for (i in 1:length(path_pollinators_future)) {
  ENM_pollinator <- rast(path_pollinators_future[[i]])
  if (i == 1) {
    Pollinator_richness <- ENM_pollinator
  } else {
    stack_pollinator <- c(Pollinator_richness, ENM_pollinator)
    Pollinator_richness <- app(stack_pollinator, fun = sum)
  }
  print(i)
}

names(Pollinator_richness) <- "Pollinator_richness_future"
Pollinator_richness_future <- Pollinator_richness
Richness[[4]] <- Pollinator_richness_future

writeRaster(Pollinator_richness_future, 
            filename = "./Results/rasters/Pollinator_richness_future.tif", 
            overwrite = TRUE)


rm(Pollinator_richness, ENM_pollinator, stack_pollinator, i, Pollinator_richness_future)


# Adjusting raster dimensions ---------------------------------------------

richness_df <- list()

richness_df[[1]] <- as.data.frame(Richness[[1]], xy = TRUE)
richness_df[[2]] <- as.data.frame(Richness[[2]], xy = TRUE)
richness_df[[3]] <- as.data.frame(Richness[[3]], xy = TRUE)
richness_df[[4]] <- as.data.frame(Richness[[4]], xy = TRUE)

# Richness maps -----------------------------------------------------------

plant_present <- ggplot(data = richness_df[[1]], 
                        aes(x = x, 
                            y = y, 
                            fill = Plant_richness_present))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  ggtitle("Current scenario") +
  scale_fill_viridis_c(name = "Plant\nrichness") +
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


plant_future <- ggplot(data = richness_df[[2]], 
                       aes(x = x, 
                           y = y, 
                           fill = Plant_richness_future))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  ggtitle("Future scenario") +
  scale_fill_viridis_c(name = "Plant\nrichness") +
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
        plot.title = element_text(hjust = 0.5,size = 18))


pollinator_present <- ggplot(data = richness_df[[3]], 
                             aes(x = x, 
                                 y = y, 
                                 fill = Pollinator_richness_present))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  ggtitle("Current scenario") +
  scale_fill_viridis_c(name = "Pollinator\nrichness") +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = "c", 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, colour = "white", size = 18))


pollinator_future <- ggplot(data = richness_df[[4]], 
                            aes(x = x, 
                                y = y, 
                                fill = Pollinator_richness_future))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  ggtitle("Current scenario") +
  scale_fill_viridis_c(name = "Pollinator\nrichness") +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = "d", 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, colour = "white",size = 18))

riqueza <- grid.arrange(plant_present, 
                        plant_future,
                        pollinator_present,
                        pollinator_future, ncol = 2)


ggsave(plot = riqueza, 
       filename = "./Results/richness_by_groups.png",
       dpi = 300,
       width = 7,
       height = 9)

# Total richness ----------------------------------------------------------

present_stack <- c(Richness[[1]], Richness[[3]])
present_stack <- app(present_stack, fun = sum)
writeRaster(present_stack, 
            filename = "./Results/rasters/Current_total_richness.tif", 
            overwrite = TRUE)

present_stack <- as.data.frame(present_stack, xy = TRUE)

future_stack <- c(Richness[[2]], Richness[[4]])
future_stack <- app(future_stack, fun = sum)
writeRaster(future_stack, 
            filename = "./Results/rasters/Future_total_richness.tif", 
            overwrite = TRUE)
future_stack <- as.data.frame(future_stack, xy = TRUE)

current_richness <- ggplot(data = present_stack, 
                           aes(x = x, 
                               y = y, 
                               fill = sum))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  ggtitle("Current scenario") +
  scale_fill_viridis_c(name = "Richness") +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = "a", 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        #axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, colour = "black",size = 18))


future_richness <- ggplot(data = future_stack, 
                          aes(x = x, 
                              y = y, 
                              fill = sum))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  ggtitle("Future scenario") +
  scale_fill_viridis_c(name = "Richness") +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = "b", 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        #axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, colour = "black",size = 18))


riqueza <- grid.arrange(current_richness, 
                        future_richness, ncol = 2)


ggsave(plot = riqueza, 
       filename = "./Results/total_richness.tif",
       dpi = 300,
       width = 7,
       height = 4.5)



rm(riqueza, pollinator_future, pollinator_present, plant_future, plant_present, 
   Richness, richness_df, future_richness, current_richness,
   present_stack,future_stack)
rm(path_plants_future, path_plants_present, path_pollinators_future, path_pollinators_present)
