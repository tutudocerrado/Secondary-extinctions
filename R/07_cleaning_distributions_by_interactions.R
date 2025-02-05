
# Creating paths to access plants models ----------------------------------

files_plant <- list.files("./ENM/Plants/Ensemble", pattern = ".tif")

path_plants_present <- list()

for (i in 1:length(files_plant)) {
  path_plants_present[[i]] <- paste("./ENM/Plants/Ensemble/Sorensen", 
                                    files_plant[i], 
                                    sep = "/")
}

path_plants_future <- list()

for (i in 1:length(files_plant)) {
  path_plants_future[[i]] <- paste("./ENM/Plants/Projection/Sorensen", 
                                    files_plant[i], 
                                    sep = "/")
}

rm(files_plant, i)

# Creating paths to access pollinators models -----------------------------

files_pollinator <- list.files("./ENM/Pollinators/Ensemble", pattern = ".tif")

path_pollinators_present <- list()

for (i in 1:length(files_pollinator)) {
  path_pollinators_present[[i]] <- paste("./ENM/Pollinators/Ensemble/Sorensen", 
                                         files_pollinator[i], 
                                         sep = "/")
}

path_pollinators_future <- list()

for (i in 1:length(files_pollinator)) {
  path_pollinators_future[[i]] <- paste("./ENM/Pollinators/Projection/Sorensen", 
                                        files_pollinator[i], 
                                        sep = "/")
}

rm(files_pollinator, i)

# Correcting species distribution by their interactions -------------------

# Loading files and species names

pollinator_files <- list.files("./ENM/Pollinators/Ensemble", pattern = ".tif")
pollinator_names <- gsub("\\.tif$", "", pollinator_files)

plant_files <- list.files("./ENM/Plants/Ensemble", pattern = ".tif")
plant_names <- gsub("\\.tif$", "", plant_files)

# Changing species names in the pollination database

pollination_database$pollinator <- gsub(" ", "_", pollination_database$pollinator)
pollination_database$plant <- gsub(" ", "_", pollination_database$plant)

# Pollinators
# Current scenario

# Creating directory to save new distribution files

dir.create("./ENM/Pollinators/Current")

# Creating a mask to correct the raster dimension

plant_mask <- as.data.frame(rast(path_plants_present[[1]]), xy = TRUE)[,1:2]
plant_extant <- terra::ext(rast(path_plants_present[[1]]))
plant_resolution <- res(rast(path_plants_present[[1]]))
empty_raster <- rast(ext = plant_extant, 
                     resolution = plant_resolution, 
                     crs = "EPSG:4326")

# Correcting species distribution

for (i in 233:length(pollinator_files)) {
  filter_interactions <- pollination_database |> 
    filter(pollinator == pollinator_names[i]) #
  interactions <- as.numeric()
  for (j in 1:nrow(filter_interactions)) {
    if (filter_interactions[j,2] %in% plant_names) {
      interactions[j] <- filter_interactions[j,2]
    }
  }
  interactions <- unlist(interactions)
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
              overwrite = TRUE)
  print(i)
}

# Future scenario

dir.create("./ENM/Pollinators/Future")

for (i in 233:length(pollinator_files)) {
  filter_interactions <- pollination_database |> 
    filter(pollinator == pollinator_names[i]) #
  interactions <- as.numeric()
  for (j in 1:nrow(filter_interactions)) {
    if (filter_interactions[j,2] %in% plant_names) {
      interactions[j] <- filter_interactions[j,2]
    }
  }
  interactions <- unlist(interactions)
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

# Plants

# Current scenario

# Correcting species name in the pollination database ---------------------

pollination_database2 <- pollination_database |> 
  mutate(pollinator = case_when(
    pollinator == "Bombus_atrattus" ~ "Bombus_atratus",
    TRUE ~ pollinator
  ))

pollination_database <- pollination_database2
rm(pollination_database2)

dir.create("./ENM/Plants/Current")






for (i in 2062:length(plant_files)) {
  filter_interactions <- pollination_database |>
    filter(plant == plant_names[i])
  interactions <- as.numeric()
  for (j in 1:nrow(filter_interactions)) {
    if (filter_interactions[j,1] %in% pollinator_names) {
      interactions[j] <- filter_interactions[j,1]
    }
  }
  interactions <- unlist(interactions)
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
  print(i)
}


# Future scenario

plant_files <- list.files("./ENM/Plants/Current", pattern = ".tif")
plant_names <- gsub("\\.tif$", "", plant_files)


dir.create("./ENM/Plants/Future")

for (i in 1:length(plant_files)) {
  filter_interactions <- pollination_database |>
    filter(plant == plant_names[i])
  interactions <- as.numeric()
  for (j in 1:nrow(filter_interactions)) {
    if (filter_interactions[j,1] %in% pollinator_names) {
      interactions[j] <- filter_interactions[j,1]
    }
  }
  interactions <- unlist(interactions)
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
