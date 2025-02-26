
# Correcting species distribution by their interactions -------------------

# Loading files and species names

pollinator_files <- list.files("./ENM/Pollinators/Ensemble", pattern = ".tif")
pollinator_names <- gsub("\\.tif$", "", pollinator_files)

plant_files <- list.files("./ENM/Plants/Ensemble", pattern = ".tif")
plant_names <- gsub("\\.tif$", "", plant_files)

# Changing species names in the pollination database ----------------------

pollination_database$pollinator <- gsub(" ", "_", pollination_database$pollinator)
pollination_database$plant <- gsub(" ", "_", pollination_database$plant)

# Creating a mask to correct the raster dimension -------------------------

plant_mask <- as.data.frame(rast("./ENM/Plants/Ensemble/Sorensen/Abutilon_amaenum.tif"), xy = TRUE)[,1:2]
plant_extant <- terra::ext(rast("./ENM/Plants/Ensemble/Sorensen/Abutilon_amaenum.tif"))
plant_resolution <- res(rast("./ENM/Plants/Ensemble/Sorensen/Abutilon_amaenum.tif"))
empty_raster <- rast(ext = plant_extant, 
                     resolution = plant_resolution, 
                     crs = "EPSG:4326")

# Creating directories to save pollinators' new distribution --------------

dir.create("./ENM/Pollinators/Current")
dir.create("./ENM/Pollinators/Future")

# Correcting pollinators' species distribution: current scenario ----------

for (i in 223:length(pollinator_files)) {
  filter_interactions <- pollination_database |> 
    filter(pollinator == pollinator_names[i]) #
  interactions <- unique(filter_interactions$plant)
  for (k in 1:length(interactions)) {
    name_tif <- paste(interactions[k],
                      ".tif", 
                      sep = "")
    interaction_pairs <- rast(paste("./ENM/Plants/Ensemble/Sorensen", 
                                    name_tif, 
                                    sep = "/"))
    if (k == 1) {
      interaction_stack <- interaction_pairs
    } else {
      interaction_stack <- c(interaction_stack, interaction_pairs)
    }
  }
  # Creating a mask to filter the species distribution
  
  interaction_stack <- app(interaction_stack, fun = sum)
  raster_bin <- (interaction_stack >= 1) * 1
  
  # Loading raster to correcting
  
  pollinator_name_tif <- paste(pollinator_names[i],
                               ".tif", 
                               sep = "")
  pollination_raster <- rast(paste("./ENM/Pollinators/Ensemble/Sorensen",
                                   pollinator_name_tif,
                                   sep = "/"))
  pollination_raster <- terra::extract(pollination_raster, plant_mask, xy = TRUE)[,-1]
  pollination_raster <- vect(pollination_raster, geom = c("x", "y"), crs = "EPSG:4326")
  pollination_raster <- terra::rasterize(pollination_raster, empty_raster, field = "layer")
  pollination_raster <- app(c(pollination_raster, raster_bin), fun = sum)
  pollination_raster <- (pollination_raster == 2) * 1
  writeRaster(pollination_raster, 
              paste("./ENM/Pollinators/Current", 
                    pollinator_name_tif, 
                    sep = "/"), 
              overwrite = TRUE);  print(i)
}

# Correcting pollinators' species distribution: future scenario -----------

for (i in 223:length(pollinator_files)) {
  filter_interactions <- pollination_database |> 
    filter(pollinator == pollinator_names[i]) #
  interactions <- unique(filter_interactions$plant)
  for (k in 1:length(interactions)) {
    name_tif <- paste(interactions[k],
                      ".tif", 
                      sep = "")
    interaction_pairs <- rast(paste("./ENM/Plants/Projection/Sorensen", 
                                    name_tif, 
                                    sep = "/"))
    if (k == 1) {
      interaction_stack <- interaction_pairs
    } else {
      interaction_stack <- c(interaction_stack, interaction_pairs)
    }
  }
  # Creating a mask to filter the species distribution
  
  interaction_stack <- app(interaction_stack, fun = sum)
  raster_bin <- (interaction_stack >= 1) * 1
  
  # Loading raster to correcting
  
  pollinator_name_tif <- paste(pollinator_names[i],
                               ".tif", 
                               sep = "")
  pollination_raster <- rast(paste("./ENM/Pollinators/Projection/Sorensen",
                                   pollinator_name_tif,
                                   sep = "/"))
  pollination_raster <- terra::extract(pollination_raster, plant_mask, xy = TRUE)[,-1]
  pollination_raster <- vect(pollination_raster, geom = c("x", "y"), crs = "EPSG:4326")
  pollination_raster <- terra::rasterize(pollination_raster, empty_raster, field = "mean")
  pollination_raster <- app(c(pollination_raster, raster_bin), fun = sum)
  pollination_raster <- (pollination_raster == 2) * 1
  writeRaster(pollination_raster, 
              paste("./ENM/Pollinators/Future", 
                    pollinator_name_tif, 
                    sep = "/"), 
              overwrite = TRUE)
  print(i)
}

# Creating directories to save plants' new distribution -------------------

dir.create("./ENM/Plants/Current")
dir.create("./ENM/Plants/Future")

# Correcting plants' species distribution: current scenario ---------------

for (i in 1901:length(plant_files)) {
  if (plant_names[i] %in% unique(pollination_database$plant)) {
    filter_interactions <- pollination_database |>
      filter(plant == plant_names[i])
    interactions <- unique(filter_interactions$pollinator)
    for (k in 1:length(interactions)) {
      name_tif <- paste(interactions[k],
                        ".tif", 
                        sep = "")
      interaction_pairs <- rast(paste("./ENM/Pollinators/Ensemble/Sorensen", 
                                      name_tif, 
                                      sep = "/"))
      if (k == 1) {
        interaction_stack <- interaction_pairs
      } else {
        interaction_stack <- c(interaction_stack, interaction_pairs)
      }
    }
    # Creating a mask to filter the species distribution
    
    interaction_stack <- app(interaction_stack, fun = sum)
    raster_bin <- (interaction_stack >= 1) * 1
    raster_bin <- terra::extract(raster_bin, plant_mask, xy = TRUE)[,-1]
    raster_bin <- vect(raster_bin, geom = c("x", "y"), crs = "EPSG:4326")
    raster_bin <- terra::rasterize(raster_bin, empty_raster, field = "sum")
    
    # Loading raster to correcting
    
    plant_name_tif <- paste(plant_names[i],
                            ".tif", 
                            sep = "")
    plant_raster <- rast(paste("./ENM/Plants/Ensemble/Sorensen",
                               plant_name_tif,
                               sep = "/"))
    
    plant_raster <- app(c(plant_raster, raster_bin), fun = sum)
    plant_raster <- (plant_raster == 2) * 1
    writeRaster(plant_raster, 
                paste("./ENM/Plants/Current", 
                      plant_name_tif, 
                      sep = "/"), 
                overwrite = TRUE)
  } else {
    print(paste(plant_names[i], "não pertence a base principal", sep = " "))
    next
  }

  print(i)
}

# Correcting plants' species distribution: future scenario ----------------

plant_files <- list.files("./ENM/Plants/Current", pattern = ".tif")
plant_names <- gsub("\\.tif$", "", plant_files)

for (i in 1:length(plant_files)) {
  filter_interactions <- pollination_database |>
    filter(plant == plant_names[i])
  interactions <- unique(filter_interactions$pollinator)
  for (k in 1:length(interactions)) {
    name_tif <- paste(interactions[k],
                      ".tif", 
                      sep = "")
    interaction_pairs <- rast(paste("./ENM/Pollinators/Projection/Sorensen", 
                                    name_tif, 
                                    sep = "/"))
    if (k == 1) {
      interaction_stack <- interaction_pairs
    } else {
      interaction_stack <- c(interaction_stack, interaction_pairs)
    }
  }
  # Creating a mask to filter the species distribution
  
  interaction_stack <- app(interaction_stack, fun = sum)
  raster_bin <- (interaction_stack >= 1) * 1
  raster_bin <- terra::extract(raster_bin, plant_mask, xy = TRUE)[,-1]
  raster_bin <- vect(raster_bin, geom = c("x", "y"), crs = "EPSG:4326")
  raster_bin <- terra::rasterize(raster_bin, empty_raster, field = "sum")
  
  # Loading raster to correcting
  
  plant_name_tif <- paste(plant_names[i],
                          ".tif", 
                          sep = "")
  plant_raster <- rast(paste("./ENM/Plants/Projection/Sorensen",
                             plant_name_tif,
                             sep = "/"))
  
  plant_raster <- app(c(plant_raster, raster_bin), fun = sum)
  plant_raster <- (plant_raster == 2) * 1
  writeRaster(plant_raster, 
              paste("./ENM/Plants/Future", 
                    plant_name_tif, 
                    sep = "/"), 
              overwrite = TRUE)
  print(i)
}

rm(filter_interactions, interaction_pairs, interaction_stack, path_plants_future,
   path_plants_present, path_pollinators_future, path_pollinators_present, plant_raster,
   pollination_raster, raster_bin,plant_files, plant_name_tif, pollinator_name_tif,
   i, interactions, j, k, name_tif, plant_names, pollinator_files, pollinator_names,
   )

# Cleaning pollination database -------------------------------------------


pollinator_files <- list.files("./ENM/Pollinators/Current", pattern = ".tif")
pollinator_names <- gsub("\\.tif$", "", pollinator_files)

plant_files <- list.files("./ENM/Plants/Current", pattern = ".tif")
plant_names <- gsub("\\.tif$", "", plant_files)

pollination_database2 <- as.data.frame(matrix(data = NA, ncol = 2))
colnames(pollination_database2) <- c("pollinator", "plant")

for (i in 1:length(pollinator_names)) {
  filter_interactions <- pollination_database |>
    filter(pollinator == pollinator_names[i])
  for (j in 1:nrow(filter_interactions)) {
    if (filter_interactions[j,2] %in% plant_names) {
      pollination_database2 <- bind_rows(pollination_database2,
                                         filter_interactions[j,])
    }
  }
  print(i)
}


pollination_database <- pollination_database2
pollination_database <- na.omit(pollination_database)

write_xlsx(pollination_database, "./data/clean/pollination_database2.xlsx")


rm(pollination_database2)
rm(plant_files, plant_names,pollinator_files, pollinator_names, filter_interactions, i, j)
rm(empty_raster,plant_extant,plant_mask, plant_resolution)
