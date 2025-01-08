
# Creating folders in the directory ---------------------------------------

dir.create("data/envi_data")
dir.create("data/envi_data/raw")
dir.create("data/envi_data/clean")
dir.create("data/envi_data/raw/future")
dir.create("data/envi_data/clean/future")
dir.create("data/envi_data/clean/pca_climate_future")
dir.create("data/envi_data/clean/pca_climate_current")
dir.create("data/envi_data/clean/pca_climate_soil_future")

# Pollinator environmental data -------------------------------------------

# Reading climate projections rasters of the future -----------------------

files <- list.files("./data/envi_data/raw/future")

# Saving in a list

raw_future <- list()

for (i in 1:length(files)) {
  raw_future[[i]] <- rast(paste("./data/envi_data/raw/future", 
                                files[i],
                                sep = "/"))
}

# Establishing Neotropical bounding box  ----------------------------------

neotropical_bbox <- c(-120, -30, -56, 30)

# Cropping Neotropical realm with total extent of South America

for (i in 1:length(raw_future)) {
  raw_future[[i]] <- crop(raw_future[[i]], neotropical_bbox)
}; rm(i)

# Creating folders in the directory for each future projection

GCM_dir <- as.numeric()

GCM_names <- c("ACCESS-CM2_ssp585_2081-2100",
               "CMCC-ESM2_ssp585_2081-2100",
               "EC-Earth3-Veg_ssp585_2081-2100",
               "FIO-ESM-2-0_ssp585_2081-2100",
               "GISS-E2-1-G_ssp585_2081-2100",
               "HadGEM3-GC31-LL_ssp585_2081-2100",
               "INM-CM5-0_ssp585_2081-2100",
               "IPSL-CM6A-LR_ssp585_2081-2100",
               "MIROC6_ssp585_2081-2100",
               "MPI-ESM1-2-HR_ssp585_2081-2100",
               "MRI-ESM2-0_ssp585_2081-2100",
               "UKESM1-0-LL_ssp585_2081-2100")

for (i in 1:length(GCM_names)) {
  GCM_dir[i] <- paste("./data/envi_data/clean/future",
                      GCM_names[i],
                      sep = "/")
  dir.create(GCM_dir[i])
}

# Saving climate projections of the future

n_layers <- nlyr(raw_future[[1]])
var_names <- c("bio01",
               "bio02",
               "bio03",
               "bio04",
               "bio05",
               "bio06",
               "bio07",
               "bio08",
               "bio09",
               "bio10",
               "bio11",
               "bio12",
               "bio13",
               "bio14",
               "bio15",
               "bio16",
               "bio17",
               "bio18",
               "bio19")

for (i in 1:length(raw_future)) {
  output_dir <- GCM_dir[i]
  for (j in 1:n_layers) {
    path <- paste(output_dir, var_names[j],sep = "/")
    writeRaster(raw_future[[i]][[j]], 
                filename = paste(path,".tif",sep = ""))
  }
}

# Removing unused objects hereafter

rm(raw_future, files, i, j, n_layers, output_dir, 
   output_path, path, var_names)

# Current climate data ----------------------------------------------------

raster_files <- list.files("./data/envi_data/raw/current/", 
                           pattern = "\\.tif$", 
                           full.names = TRUE)

raster_files <- lapply(raster_files, rast)

raster_files <- c(raster_files[[1]],
                  raster_files[[2]],
                  raster_files[[3]],
                  raster_files[[4]],
                  raster_files[[5]],
                  raster_files[[6]],
                  raster_files[[7]],
                  raster_files[[8]],
                  raster_files[[9]],
                  raster_files[[10]],
                  raster_files[[11]],
                  raster_files[[12]],
                  raster_files[[13]],
                  raster_files[[14]],
                  raster_files[[15]],
                  raster_files[[16]],
                  raster_files[[17]],
                  raster_files[[18]],
                  raster_files[[19]])


raster_files <- crop(raster_files, neotropical_bbox)

# Climate PCA -------------------------------------------------------------

climate_pca <- correct_colinvar(env_layer = raster_files,
                                method = c('pca'),
                                proj = "./data/envi_data/clean/future",
                                save_proj = "./data/envi_data/clean/pca_climate_future")

# Saving PCs of current climate scenario

writeRaster(climate_pca$env_layer$PC1, 
            filename = "./data/envi_data/clean/pca_climate_current/PC1.tif")

writeRaster(climate_pca$env_layer$PC2, 
            filename = "./data/envi_data/clean/pca_climate_current/PC2.tif")

writeRaster(climate_pca$env_layer$PC3, 
            filename = "./data/envi_data/clean/pca_climate_current/PC3.tif")

writeRaster(climate_pca$env_layer$PC4, 
            filename = "./data/envi_data/clean/pca_climate_current/PC4.tif")

writeRaster(climate_pca$env_layer$PC5, 
            filename = "./data/envi_data/clean/pca_climate_current/PC5.tif")

writeRaster(climate_pca$env_layer$PC6, 
            filename = "./data/envi_data/clean/pca_climate_current/PC6.tif")

# Saving PCs of future climate scenario

# Reading PCA climate projections rasters of the future -----------------------

filesPCA <- list.files("./data/envi_data/clean/pca_climate_future")

# Saving in a list

PCA_climate_future <- list()

for (i in 1:length(filesPCA)) {
  PCA_climate_future[[i]] <- rast(paste("./data/envi_data/clean/pca_climate_future", 
                                        filesPCA[i],
                                        "pcs.tif",
                                        sep = "/"))
}

# Exporting PCA climate projections

var_names <- c("PC1.tif",
               "PC2.tif",
               "PC3.tif",
               "PC4.tif",
               "PC5.tif",
               "PC6.tif")

for (i in 1:length(PCA_climate_future)) {
  for (j in 1:6) {
    writeRaster(PCA_climate_future[[i]][[j]], 
                filename = paste("./data/envi_data/clean/pca_climate_future",
                                 filesPCA[i],
                                 var_names[j],
                                 sep = "/"))
  }
}

# After run the code above, delete files "pcs" from the folder

# Removing unused objects hereafter

rm(filesPCA, i,j,var_names)

# Climate and edaphic data ------------------------------------------------

# Current climate data

raster_files <- list.files("./data/envi_data/raw/current/", 
                           pattern = "\\.tif$", 
                           full.names = TRUE)

raster_files <- lapply(raster_files, rast)

for (i in 1:length(raster_files)) {
  raster_files[[i]] <- terra::crop(raster_files[[i]], neotropical_bbox)
}

var_names <- c("bio01.tif",
               "bio02.tif",
               "bio03.tif",
               "bio04.tif",
               "bio05.tif",
               "bio06.tif",
               "bio07.tif",
               "bio08.tif",
               "bio09.tif",
               "bio10.tif",
               "bio11.tif",
               "bio12.tif",
               "bio13.tif",
               "bio14.tif",
               "bio15.tif",
               "bio16.tif",
               "bio17.tif",
               "bio18.tif",
               "bio19.tif")

for (i in 1:length(raster_files)) {
  writeRaster(raster_files[[i]], 
              filename = paste("./data/envi_data/clean/current_soil", 
                               var_names[i],
                               sep = "/"))
}


# Soil data

raster_soil_files <- list.files("./data/envi_data/raw/soil/", 
                           pattern = "\\.tif$", 
                           full.names = TRUE)

raster_soil_files <- lapply(raster_soil_files, rast)

# Resample of edaphic data

for (i in 1:length(raster_soil_files)) {
  raster_soil_files[[i]] <- resample(raster_soil_files[[i]], raster_files[[1]])
}


Current_soil_climate <- c(raster_files[[1]],
                          raster_files[[2]],
                          raster_files[[3]],
                          raster_files[[4]],
                          raster_files[[5]],
                          raster_files[[6]],
                          raster_files[[7]],
                          raster_files[[8]],
                          raster_files[[9]],
                          raster_files[[10]],
                          raster_files[[11]],
                          raster_files[[12]],
                          raster_files[[13]],
                          raster_files[[14]],
                          raster_files[[15]],
                          raster_files[[16]],
                          raster_files[[17]],
                          raster_files[[18]],
                          raster_files[[19]],
                          raster_soil_files[[1]],
                          raster_soil_files[[2]],
                          raster_soil_files[[3]],
                          raster_soil_files[[4]],
                          raster_soil_files[[5]],
                          raster_soil_files[[6]],
                          raster_soil_files[[7]],
                          raster_soil_files[[8]],
                          raster_soil_files[[9]],
                          raster_soil_files[[10]],
                          raster_soil_files[[11]])

# Exporting edaphic data into the future climate folders ------------------

var_names <- c("cbdod",
               "cec",
               "cfvo",
               "clay",
               "nitrogen",
               "ocd",
               "ocs",
               "phh2o",
               "sand",
               "silt",
               "soc")

for (i in 1:length(GCM_dir)) {
  output_dir <- GCM_dir[i]
  for (j in 1:length(raster_soil_files)) {
    path <- paste(output_dir, var_names[j],sep = "/")
    writeRaster(raster_soil_files[[j]], 
                filename = paste(path,".tif",sep = ""), overwrite = TRUE)
  }
}

# Climate and edaphic PCA -------------------------------------------------

names(Current_soil_climate) <- c("bio01","bio02","bio03","bio04","bio05",
                                 "bio06","bio07","bio08","bio09","bio10",
                                 "bio11","bio12","bio13","bio14","bio15",
                                 "bio16", "bio17","bio18","bio19", "cbdod",
                                 "cec","cfvo","clay","nitrogen","ocd",
                                 "ocs","phh2o","sand","silt","soc")

climate_soil_pca <- correct_colinvar(env_layer = Current_soil_climate,
                                     method = c('pca'),
                                     proj = "./data/envi_data/clean/future",
                                     save_proj = "./data/envi_data/clean/pca_climate_soil_future")


writeRaster(Current_soil_climate, 
            filename = "./data/envi_data/clean/current_soil")

rm(Current_soil_climate, climate_pca, output_dir, path, raster_files, raster_soil_files,
   PCA_climate_future, filesPCA, GCM_dir, GCM_names, var_names)

