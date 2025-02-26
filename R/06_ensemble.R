
# Creating folders --------------------------------------------------------

dir.create("ENM")
dir.create("ENM/Pollinators")
dir.create("ENM/Pollinators/Projection")
dir.create("ENM/Pollinators/Projection/Sorensen")
dir.create("ENM/Pollinators/Ensemble")
dir.create("ENM/Pollinators/Ensemble/Sorensen")
dir.create("ENM/Plants")
dir.create("ENM/Plants/Projection")
dir.create("ENM/Plants/Projection/Sorensen")
dir.create("ENM/Plants/Ensemble")
dir.create("ENM/Plants/Ensemble/Sorensen")

# Future ensemble: pollinators --------------------------------------------

# Counting subfolders

rounds <- list.files("./ENM_pollinators")

folders <- list.files(paste("./ENM_pollinators", 
                            rounds[1], 
                            "Projection", 
                            sep = "/"))

paths <- list()

for (i in 1:length(rounds)) {
  rounds_list <- list()
  for (j in 1:length(folders)) {
    rounds_list[[j]] <- paste("./ENM_pollinators", 
                              rounds[i], 
                              "Projection", 
                              folders[j], 
                              "Ensemble",
                              "SUP",
                              sep = "/")
  }
  paths[[i]] <- rounds_list
}; rm(rounds_list, i, j)

paths_ensemble <- list()
group_list <- list()
save_list <- list()

# Creating paths to perform the ensemble by the average -------------------

for (i in 1:length(rounds)) {
  for (j in 1:length(folders)) {
    path_i <- paths[[i]][[j]]
    files <- list.files(path_i,
                        pattern = ".tif")
    for (k in 1:length(files)) {
      save_list[[k]] <- paste(path_i, 
                              files[k],
                              sep = "/")
    }
    group_list[[j]] <- save_list
    save_list <- list()
  }
  paths_ensemble[[i]] <- group_list
  group_list <- list()
}

# Removing unused objects

rm(i,j,k, group_list)

# Reading threshold files -------------------------------------------------

# Pollinators

thresholds <- list()

for (i in 1:length(rounds)) {
  thresholds[[i]] <- read_xlsx(paste("./ENM_pollinators",
                                rounds[i], 
                                "Thresholds_Ensemble.xlsx", 
                                sep = "/"),
                               sheet = 1)
}

# Reading evaluation table ------------------------------------------------

# Pollinators

evaluation <- list()

for (i in 1:length(rounds)) {
  evaluation[[i]] <- read_xlsx(paste("./ENM_pollinators",
                                     rounds[i], 
                                     "Evaluation_Table.xlsx", 
                                     sep = "/"),
                               sheet = 1)
}

# Performing ensemble -----------------------------------------------------

SA_bbox <- c(-82, -34, -56, 12.5)
GCMs <- list()

for (i in 1:length(rounds)) {
  for (j in 1:length(paths_ensemble[[i]][[1]])) {
      for (k in 1:length(folders)) {
        sp_name <- gsub("\\.tif", "", basename(paths_ensemble[[i]][[k]][[j]]))
        filtered_data <- subset(evaluation[[i]], Sp == sp_name & Algorithm == "SUP")
        
        # Validating models
        
        if (!filtered_data$AUC >= 0.7 & filtered_data$TSS >= 0.5) {
          next
        } else {
          GCMs[[k]] <- rast(paths_ensemble[[i]][[k]][[j]])
        }
        
      }
    if (exists("GCMs") && length(GCMs) >= 5) {
    stackGCM <- c(GCMs[[1]], GCMs[[2]], GCMs[[3]], GCMs[[4]], GCMs[[5]])
    meanGCM <- terra::app(stackGCM, fun = mean)
    cropGCM <- crop(meanGCM, SA_bbox)
    species_basename <- basename(paths_ensemble[[i]][[k]][[j]])
    writeRaster(cropGCM, 
                paste("ENM/Pollinators/Projection", 
                      species_basename, 
                      sep = "/"), 
                overwrite = TRUE)
    species_name <- gsub("\\.tif$", "", species_basename)
    threshold_file <- as.data.frame(thresholds[i])
    threshold <- threshold_file |> 
      filter(Sp == species_name & THR == "SORENSEN") |> 
      pull(THR_VALUE)
    threshold <- as.numeric(threshold)
    cropGCMbin <- cropGCM >= threshold
    writeRaster(cropGCMbin, 
                paste("ENM/Pollinators/Projection/Sorensen", 
                      species_basename, 
                      sep = "/"), 
                overwrite = TRUE)
    GCMs <- list()
    print(j)
    }
  }
  print(i); print(j)
}

# Future ensemble: plants -------------------------------------------------

# Counting subfolders

rounds <- list.files("./ENM_plants")

folders <- list.files(paste("./ENM_plants", 
                            rounds[1], 
                            "Projection", 
                            sep = "/"))

paths <- list()

for (i in 1:length(rounds)) {
  rounds_list <- list()
  for (j in 1:length(folders)) {
    rounds_list[[j]] <- paste("./ENM_plants", 
                              rounds[i], 
                              "Projection", 
                              folders[j], 
                              "Ensemble",
                              "SUP",
                              sep = "/")
  }
  paths[[i]] <- rounds_list
}

# Creating paths to perform the ensemble by the average -------------------

paths_ensemble <- list()
group_list <- list()
save_list <- list()

for (i in 1:length(rounds)) {
  for (j in 1:length(folders)) {
    path_i <- paths[[i]][[j]]
    files <- list.files(path_i,
                        pattern = ".tif")
    for (k in 1:length(files)) {
      save_list[[k]] <- paste(path_i, 
                              files[k],
                              sep = "/")
    }
    group_list[[j]] <- save_list
    save_list <- list()
  }
  paths_ensemble[[i]] <- group_list
  group_list <- list()
}

rm(i,j,k)

# Reading threshold files -------------------------------------------------

thresholds2 <- list()

for (i in 1:length(rounds)) {
  thresholds2[[i]] <- read_xlsx(paste("./ENM_plants",
                                      rounds[i], 
                                      "Thresholds_Ensemble.xlsx", 
                                      sep = "/"),
                                sheet = 1)
}

# Reading evaluation table ------------------------------------------------

# Pollinators

evaluation2 <- list()

for (i in 1:length(rounds)) {
  evaluation2[[i]] <- read_xlsx(paste("./ENM_plants",
                                      rounds[i], 
                                      "Evaluation_Table.xlsx", 
                                      sep = "/"),
                                sheet = 1)
}

# Performing ensemble -----------------------------------------------------

SA_bbox <- c(-82, -34, -56, 12.5)
GCMs <- list()

for (i in 1:length(rounds)) {
  for (j in 1:length(paths_ensemble[[i]][[1]])) {
    for (k in 1:length(folders)) {
      sp_name <- gsub("\\.tif", "", basename(paths_ensemble[[i]][[k]][[j]]))
      filtered_data <- subset(evaluation2[[i]], Sp == sp_name & Algorithm == "SUP")
      
      # Validating models
      
      if (!filtered_data$AUC >= 0.7 & filtered_data$TSS >= 0.5) {
        next
      } else {
        GCMs[[k]] <- rast(paths_ensemble[[i]][[k]][[j]])
      }
      
    }
    if (exists("GCMs") && length(GCMs) >= 5) {
    stackGCM <- c(GCMs[[1]], GCMs[[2]], GCMs[[3]], GCMs[[4]], GCMs[[5]])
    meanGCM <- terra::app(stackGCM, fun = mean)
    cropGCM <- crop(meanGCM, SA_bbox)
    species_basename <- basename(paths_ensemble[[i]][[k]][[j]])
    writeRaster(cropGCM, 
                paste("ENM/Plants/Projection", 
                      species_basename, 
                      sep = "/"), 
                overwrite = TRUE)
    species_name <- gsub("\\.tif$", "", species_basename)
    threshold_file <- as.data.frame(thresholds2[i])
    threshold <- threshold_file |> 
      filter(Sp == species_name & THR == "SORENSEN") |> 
      pull(THR_VALUE)
    threshold <- as.numeric(threshold)
    cropGCMbin <- cropGCM >= threshold
    writeRaster(cropGCMbin, 
                paste("ENM/Plants/Projection/Sorensen", 
                      species_basename, 
                      sep = "/"), 
                overwrite = TRUE)
    GCMs <- list()
    print(j)
    }
  }
  print(i); print(j)
}

# Removing unused objects -------------------------------------------------

rm(files, folders, i, j, k, neotropical_bbox, rounds, SA_bbox, species_basename, 
   species_name, threshold, threshold_file, thresholds, stackGCM, save_list, rounds_list,
   paths, paths_ensemble, meanGCM, group_list, GCMs, cropGCM, cropGCMbin, path_i)

# Cropping current niche models: pollinators ------------------------------

rounds <- list.files("./ENM_pollinators")

paths <- list()

for (i in 1:length(rounds)) {
  paths[[i]] <- paste("./ENM_pollinators", 
                            rounds[i], 
                            "Ensemble",
                            "SUP",
                            sep = "/")
}

files2 <- list.files("./ENM/pollinators/Projection", pattern = ".tif")

path_models <- list()

for (i in 1:length(rounds)) {
  files <- list.files(paths[[i]], pattern = ".tif")
  paths_current <- list()
  for (j in 1:length(files)) {
    paths_current[[j]] <- paste(paths[[i]], files[j], sep = "/")
  }
  path_models[[i]] <- paths_current
}

SA_bbox <- c(-82, -34, -56, 12.5)

for (i in 1:length(rounds)) {
  for (j in 1:length(path_models[[i]])) {
    if (basename(path_models[[i]][[j]]) %in% files2) {
      model <- rast(path_models[[i]][[j]])
      crop_model <- crop(model, SA_bbox)
      species_basename <- basename(path_models[[i]][[j]])
      writeRaster(crop_model, 
                  paste("ENM/Pollinators/Ensemble", 
                        species_basename, 
                        sep = "/"), 
                  overwrite = TRUE)
    } else {
      next
    }
    
  }
}

# Cropping pollinator models in the current scenario: binary --------------

paths <- list()

for (i in 1:length(rounds)) {
  paths[[i]] <- paste("./ENM_pollinators", 
                      rounds[i], 
                      "Ensemble",
                      "SUP",
                      "Sorensen",
                      sep = "/")
}


path_models <- list()

for (i in 1:length(rounds)) {
  files <- list.files(paths[[i]], pattern = ".tif")
  paths_current <- list()
  for (j in 1:length(files)) {
    paths_current[[j]] <- paste(paths[[i]], files[j], sep = "/")
  }
  path_models[[i]] <- paths_current
}

for (i in 1:length(rounds)) {
  for (j in 1:length(path_models[[i]])) {
    if (basename(path_models[[i]][[j]]) %in% files2) {
      model <- rast(path_models[[i]][[j]])
      crop_model <- crop(model, SA_bbox)
      species_basename <- basename(path_models[[i]][[j]])
      writeRaster(crop_model, 
                  paste("ENM/Pollinators/Ensemble/Sorensen", 
                        species_basename, 
                        sep = "/"), 
                  overwrite = TRUE)
    } else {
      next
    }
    
  }
}

# Cropping current niche models: plants -----------------------------------

rounds <- list.files("./ENM_plants")

files2 <- list.files("./ENM/plants/Projection", pattern = ".tif")

paths <- list()

for (i in 1:length(rounds)) {
  paths[[i]] <- paste("./ENM_plants", 
                      rounds[i], 
                      "Ensemble",
                      "SUP",
                      sep = "/")
}


path_models <- list()

for (i in 1:length(rounds)) {
  files <- list.files(paths[[i]], pattern = ".tif")
  paths_current <- list()
  for (j in 1:length(files)) {
    paths_current[[j]] <- paste(paths[[i]], files[j], sep = "/")
  }
  path_models[[i]] <- paths_current
}

SA_bbox <- c(-82, -34, -56, 12.5)

for (i in 1:length(rounds)) {
  for (j in 1:length(path_models[[i]])) {
    if (basename(path_models[[i]][[j]]) %in% files2) {
      model <- rast(path_models[[i]][[j]])
      crop_model <- crop(model, SA_bbox)
      species_basename <- basename(path_models[[i]][[j]])
      writeRaster(crop_model, 
                  paste("ENM/Plants/Ensemble", 
                        species_basename, 
                        sep = "/"), 
                  overwrite = TRUE)
    } else {
      next
    }
    print(j)
  }
  print(i); print(j)
}

# Binary files

paths <- list()

for (i in 1:length(rounds)) {
  paths[[i]] <- paste("./ENM_plants", 
                      rounds[i], 
                      "Ensemble",
                      "SUP",
                      "Sorensen",
                      sep = "/")
}


path_models <- list()

for (i in 1:length(rounds)) {
  files <- list.files(paths[[i]], pattern = ".tif")
  paths_current <- list()
  for (j in 1:length(files)) {
    paths_current[[j]] <- paste(paths[[i]], files[j], sep = "/")
  }
  path_models[[i]] <- paths_current
}

for (i in 1:length(rounds)) {
  for (j in 1:length(path_models[[i]])) {
    if (basename(path_models[[i]][[j]]) %in% files2) {
      model <- rast(path_models[[i]][[j]])
      crop_model <- crop(model, SA_bbox)
      species_basename <- basename(path_models[[i]][[j]])
      writeRaster(crop_model, 
                  paste("ENM/Plants/Ensemble/Sorensen", 
                        species_basename, 
                        sep = "/"), 
                  overwrite = TRUE)
    } else {
      next
    }
    
  }
  print(i)
}

# Removing unused objects -------------------------------------------------

rm(crop_model, model, path_models, paths, paths_current,  
   files, i, j, rounds, SA_bbox, species_basename, sp_name, files2, thresholds2,
   evaluation, evaluation2, filtered_data)





