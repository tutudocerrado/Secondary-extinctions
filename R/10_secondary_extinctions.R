
# Mapping secondary extinctions -------------------------------------------

# Function to calculate secondary extinction ------------------------------

secondary_extinction <- function(x) {
  ifelse(x[1] == 0 & x[2] == 0 & x[3] == 0, 0,
         ifelse(x[1] == 0 & x[2] == 0 & x[3] >= 1, 0,
                ifelse(x[1] == 0 & x[2] == 1 & x[3] == 0, 0, 
                       ifelse(x[1] == 0 & x[2] == 1 & x[3] >= 1, 0,
                              ifelse(x[1] == 1 & x[2] == 0 & x[3] == 0, 0,
                                     ifelse(x[1] == 1 & x[2] == 0 & x[3] >= 1, 0,
                                            ifelse(x[1] == 1 & x[2] == 1 & x[3] == 0, 1,
                                                   ifelse(x[1] == 1 & x[2] == 1 & x[3] >= 1, 0, NA))))))))
}

# Loading files and species names -----------------------------------------

pollinator_files <- list.files("./ENM/Pollinators/Current", pattern = ".tif")
pollinator_names <- gsub("\\.tif$", "", pollinator_files)

plant_files <- list.files("./ENM/Plants/Current", pattern = ".tif")
plant_names <- gsub("\\.tif$", "", plant_files)

# Creating a mask to correct the raster dimension -------------------------

path_mask <- "./ENM/Pollinators/Current/Acamptopoeum_prinii.tif"
plant_mask <- as.data.frame(rast(path_mask), 
                            xy = TRUE)[,1:2]
plant_extant <- terra::ext(rast(path_mask))
plant_resolution <- res(rast(path_mask))
empty_raster <- rast(ext = plant_extant, 
                     resolution = plant_resolution, 
                     crs = "EPSG:4326")

# Creating directories to save secondary extinction data ------------------

dir.create("Secondary_extinction")
dir.create("Secondary_extinction/Pollinators")
dir.create("Secondary_extinction/Pollinators/Interaction_richness")
dir.create("Secondary_extinction/Plants")
dir.create("Secondary_extinction/Plants/Interaction_richness")

# Computing Pollinators' secondary extinction -----------------------------

for (i in 1:length(pollinator_files)) {
  filter_interactions <- pollination_database |> 
    filter(pollinator == pollinator_names[i]) #
  interactions <- as.numeric()
  for (j in 1:nrow(filter_interactions)) {
    if (filter_interactions[j,2] %in% plant_names) {
      interactions[j] <- filter_interactions[j,2]
    }
  }
  interactions <- unlist(interactions)
  
  # Stacking rasters of the interactions

  for (k in 1:length(interactions)) {
    interaction_pairs <- rast(paste("./ENM/Plants/Projection/Sorensen", 
                                    paste(interactions[k],
                                          ".tif", 
                                          sep = ""), 
                                    sep = "/"))
    if (k == 1) {
      interaction_stack <- interaction_pairs
    } else {
      interaction_stack <- c(interaction_stack, interaction_pairs)
    }
  }
  
  # Performing richness of interactions
  
  interaction_stack <- app(interaction_stack, fun = sum)
  writeRaster(interaction_stack, 
              paste("./Secondary_extinction/Pollinators/Interaction_richness", 
                    pollinator_files[i], 
                    sep = "/"), 
              overwrite = TRUE)
  
  # Loading current distribution of pollinator
  
  pollinator_current <- rast(paste("./ENM/Pollinators/Current", 
                                   pollinator_files[i], 
                                   sep = "/"))
  
  # Loading future distribution of the pollinator and correcting their extent
  
  pollinator_future <- rast(paste("./ENM/Pollinators/Projection/Sorensen", 
                                  pollinator_files[i], 
                                  sep = "/"))
  pollinator_future <- terra::extract(pollinator_future, 
                                      plant_mask, 
                                      xy = TRUE)[,-1]
  pollinator_future <- vect(pollinator_future, 
                            geom = c("x", "y"), 
                            crs = "EPSG:4326")
  pollinator_future <- terra::rasterize(pollinator_future, 
                                        empty_raster, 
                                        field = "mean")
  
  # Preparing data for applying secondary extinction function
 
  data <- c(pollinator_current, pollinator_future, interaction_stack)
  
  # Applying secondary extinction function
  
  pollinator_secondary_extinction <- app(data, fun = function(x) secondary_extinction(x))
  writeRaster(pollinator_secondary_extinction, 
              paste("./Secondary_extinction/Pollinators", 
                    pollinator_files[i], 
                    sep = "/"), 
              overwrite = TRUE)
  print(i)
}

# Computing Plants' secondary extinction ----------------------------------

for (i in 1:length(plant_files)) {
  filter_interactions <- pollination_database |> 
    filter(plant == plant_names[i]) #
  interactions <- as.numeric()
  for (j in 1:nrow(filter_interactions)) {
    if (filter_interactions[j,1] %in% pollinator_names) {
      interactions[j] <- filter_interactions[j,1]
    }
  }
  interactions <- unlist(interactions)
  
  # Stacking rasters of the interactions 
  
  for (k in 1:length(interactions)) {
    interaction_pairs <- rast(paste("./ENM/Pollinators/Projection/Sorensen", 
                                    paste(interactions[k],
                                          ".tif", 
                                          sep = ""), 
                                    sep = "/"))
    if (k == 1) {
      interaction_stack <- interaction_pairs
    } else {
      interaction_stack <- c(interaction_stack, interaction_pairs)
    }
  }
  
  # Performing richness of interactions and correcting their extent
  
  interaction_stack <- app(interaction_stack, fun = sum)
  interaction_stack <- terra::extract(interaction_stack, 
                                      plant_mask, 
                                      xy = TRUE)[,-1]
  interaction_stack <- vect(interaction_stack, 
                            geom = c("x", "y"), 
                            crs = "EPSG:4326")
  interaction_stack <- terra::rasterize(interaction_stack, 
                                        empty_raster, 
                                        field = "sum")
  
  writeRaster(interaction_stack, 
              paste("./Secondary_extinction/Plants/Interaction_richness", 
                    plant_files[i], 
                    sep = "/"), 
              overwrite = TRUE)
  
  # Loading current distribution of plant
  
  plant_current <- rast(paste("./ENM/Plants/Current", 
                              plant_files[i], 
                              sep = "/"))
  
  # Loading future distribution of the plants
  
  plant_future <- rast(paste("./ENM/Plants/Projection/Sorensen", 
                             plant_files[i], 
                             sep = "/"))
  
  # Preparing data for applying secondary extinction function
  
  data <- c(plant_current, plant_future, interaction_stack)
  
  # Applying secondary extinction function
  
  plant_secondary_extinction <- app(data, fun = function(x) secondary_extinction(x))
  writeRaster(plant_secondary_extinction, 
              paste("./Secondary_extinction/Plants", 
                    plant_files[i], 
                    sep = "/"), 
              overwrite = TRUE)
  print(i)
}

rm(data, empty_raster, filter_interactions,interaction_pairs, interaction_stack,
   plant_current, plant_extant, plant_future, plant_mask, plant_secondary_extinction,
   pollinator_current, pollinator_future, pollinator_secondary_extinction, i, 
   interactions, j, k, path_mask, plant_files, plant_names, plant_resolution,
   pollinator_files, pollinator_names, secondary_extinction)

# Computing secondary extinctions by group --------------------------------

# Plants

p_extinction_files <- list.files("./Secondary_extinction/plants", pattern = ".tif")

for (i in 1:length(p_extinction_files)) {
  p_extinction <- rast(paste("./Secondary_extinction/plants", 
                             p_extinction_files[i], 
                             sep = "/"))
  if (i == 1) {
    p_extinction_stack <- p_extinction
  } else {
    p_extinction_stack <- c(p_extinction_stack, p_extinction)
  }
  print(i)
}

p_extinction_plants <- terra::app(p_extinction_stack, fun = sum)

writeRaster(p_extinction_plants, 
            "./Results/rasters/secondary_extinction_plants.tif", 
            overwrite = TRUE)

# Pollinators

p_extinction_files <- list.files("./Secondary_extinction/pollinators", pattern = ".tif")

for (i in 1:length(p_extinction_files)) {
  p_extinction <- rast(paste("./Secondary_extinction/pollinators", 
                             p_extinction_files[i], 
                             sep = "/"))
  if (i == 1) {
    p_extinction_stack <- p_extinction
  } else {
    p_extinction_stack <- c(p_extinction_stack, p_extinction)
  }
  print(i)
}

p_extinction_pollinators <- terra::app(p_extinction_stack, fun = sum)

writeRaster(p_extinction_pollinators, 
            "./Results/rasters/secondary_extinction_pollinators.tif", 
            overwrite = TRUE)

# Computing total secondary extinction ------------------------------------

total_secondary_extinction <- c(p_extinction_plants, p_extinction_pollinators)
total_secondary_extinction <- app(total_secondary_extinction, fun = sum)
writeRaster(total_secondary_extinction, 
            "./Results/rasters/secondary_extinction.tif", 
            overwrite = TRUE)

# Secondary extinction maps -----------------------------------------------

p_extinction_plants <- as.data.frame(p_extinction_plants, xy = TRUE)

secondary_extinc_plant_map <- ggplot(data = p_extinction_plants, 
                                      aes(x = x, 
                                          y = y, 
                                          fill = sum))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  ggtitle("Plants' secondary extinction") +
  scale_fill_viridis_c(name = "Secondary\nextinction") +
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

p_extinction_pollinators <- as.data.frame(p_extinction_pollinators, xy = TRUE)

secondary_extinc_pollinator_map <- ggplot(data = p_extinction_pollinators, 
                                     aes(x = x, 
                                         y = y, 
                                         fill = sum))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  ggtitle("Pollinators' secondary extinction") +
  scale_fill_viridis_c(name = "Secondary\nextinction") +
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

secondary_extinction_groups <- grid.arrange(secondary_extinc_plant_map, 
                                            secondary_extinc_pollinator_map, ncol = 2)

ggsave(plot = secondary_extinction_groups, 
       filename = "./Results/secondary_extinction_groups.png",
       dpi = 300,
       width = 7,
       height = 4.5)

rm(p_extinction, p_extinction_plants, p_extinction_pollinators, p_extinction_stack,
   secondary_extinc_plant_map, secondary_extinc_pollinator_map, secondary_extinction_groups,
   total_secondary_extinction, i, p_extinction_files)
