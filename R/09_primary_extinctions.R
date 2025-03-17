# Primary extinction ------------------------------------------------------

# Function to calculate primary extinction

primary_extinction <- function(x) {
  ifelse(x[1] == 1 & x[2] == 1, 0, 
         ifelse(x[1] == 1 & x[2] == 0, 1, 
                ifelse(x[1] == 0 & x[2] == 1, 0, 
                       ifelse(x[1] == 0 & x[2] == 0, 0, NA))))
}

# Creating directories to save primary extinction maps --------------------

dir.create("./Primary_extinction")
dir.create("./Primary_extinction/plants")
dir.create("./Primary_extinction/pollinators")

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

# Creating primary extinction's maps --------------------------------------

# Plants

for (i in 1:length(plant_files)) {
  t1 <- rast(paste("./ENM/Plants/Current", plant_files[i], sep = "/"))
  t2 <- rast(paste("./ENM/Plants/Projection/Sorensen", plant_files[i], sep = "/"))
  t <- c(t1, t2)
  result <- app(t, primary_extinction)
  writeRaster(result, 
              paste("./Primary_extinction/plants", 
                    plant_files[i], 
                    sep = "/"), 
              overwrite = TRUE)
  print(i)
}

rm(result, t, t1, t2, i)

# Pollinators

for (i in 1:length(pollinator_files)) {
  t1 <- rast(paste("./ENM/Pollinators/Current", pollinator_files[i], sep = "/"))
  t2 <- rast(paste("./ENM/Pollinators/Projection/Sorensen", pollinator_files[i], sep = "/"))
  t2 <- terra::extract(t2, plant_mask, xy = TRUE)[,-1]
  t2 <- vect(t2, geom = c("x", "y"), crs = "EPSG:4326")
  t2 <- terra::rasterize(t2, empty_raster, field = "mean")
  t <- c(t1, t2)
  result <- app(t, primary_extinction)
  writeRaster(result, 
              paste("./Primary_extinction/pollinators", 
                    pollinator_files[i], 
                    sep = "/"), 
              overwrite = TRUE)
  print(i)
}; rm(result, t, t1, t2, i)

# Mapping primary extinctions by group ------------------------------------

# Plants

p_extinction_files <- list.files("./Primary_extinction/plants")

for (i in 1:length(p_extinction_files)) {
  p_extinction <- rast(paste("./Primary_extinction/plants", 
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
            "./Results/rasters/primary_extinction_plants.tif", 
            overwrite = TRUE)

# Pollinators

p_extinction_files <- list.files("./Primary_extinction/pollinators")

for (i in 1:length(p_extinction_files)) {
  p_extinction <- rast(paste("./Primary_extinction/pollinators", 
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
            "./Results/rasters/primary_extinction_pollinators.tif", 
            overwrite = TRUE)

rm(p_extinction_stack, p_extinction, i, p_extinction_files)

# Primary extinction maps by group ----------------------------------------

primary_extinc_pollinat <- rast("./Results/rasters/primary_extinction_pollinators.tif")
primary_extinc_pollinat <- as.data.frame(primary_extinc_pollinat, xy = TRUE)

primary_extinc_pollinat_map <- ggplot(data = primary_extinc_pollinat, 
                                      aes(x = x, 
                                          y = y, 
                                          fill = sum))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  ggtitle("Pollinators' primary extinction") +
  scale_fill_viridis_c(name = "Primary\nextinction") +
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


primary_extinc_plants <- rast("./Results/rasters/primary_extinction_plants.tif")
primary_extinc_plants <- as.data.frame(primary_extinc_plants, xy = TRUE)

primary_extinc_plants_map <- ggplot(data = primary_extinc_plants, 
                                    aes(x = x, 
                                        y = y, 
                                        fill = sum))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  ggtitle("Plants' primary extinction") +
  scale_fill_viridis_c(name = "Primary\nextinction") +
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
        plot.title = element_text(hjust = 0.5,size = 18))

primary_extinction_groups <- grid.arrange(primary_extinc_plants_map, 
                                          primary_extinc_pollinat_map, ncol = 2)



ggsave(plot = primary_extinction_groups, 
       filename = "./Results/primary_extinction_groups.tif",
       dpi = 300,
       width = 7.2,
       height = 4.5)


primary_extinc_pollinat <- rast("./Results/rasters/primary_extinction_pollinators.tif")
primary_extinc_plants <- rast("./Results/rasters/primary_extinction_plants.tif")

primary_extinctions <- app(c(primary_extinc_plants, primary_extinc_pollinat), fun = sum)
writeRaster(primary_extinctions, 
            "./Results/rasters/primary_extinction.tif", 
            overwrite = TRUE)


rm(primary_extinc_plants, primary_extinc_pollinat, primary_extinc_plants_map,
   primary_extinc_pollinat_map, primary_extinction_groups)

rm(primary_extinctions, primary_extinc_pollinat, primary_extinc_plants)
rm(p_extinction_plants, p_extinction_pollinators, path_plants_future,
   path_plants_present, path_pollinators_future, path_pollinators_present)
rm(primary_extinction, empty_raster, plant_extant, plant_mask, path_mask,
   plant_files, plant_names, plant_resolution, pollinator_files, pollinator_names)
