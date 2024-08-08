# GBIF --------------------------------------------------------------------

# Collecting occurrence records from GBIF

# GBIF: pollinators -------------------------------------------------------

# Selecting only unique names

pollinator <- unique(pollination_database$pollinator)

# Creating a data frame to include species with no records

no_coordinates_pollinatorGBIF <- matrix(nrow = length(pollinator), ncol = 1)
no_coordinates_pollinatorGBIF <- as.data.frame(no_coordinates_pollinatorGBIF)
colnames(no_coordinates_pollinatorGBIF) <- "pollinator_scientfic_name"

# Getting occurrence records of the pollinators

for (i in 1:length(pollinator)) {
  data <- rgbif2(species = pollinator[i], save = FALSE, limit = 99500)
  if(nrow(data) == 0){
    no_coordinates_pollinatorGBIF[i,1] <- pollinator[i]
  }
  if(nrow(data) !=  0){
    data <- data[,c("species_search","decimalLongitude","decimalLatitude")]
    if(i == 1){
      gbif <- data
    } else{
      gbif <- rbind(gbif, data)
    }
  }
  print(paste(print(i), "of", length(pollinator)))
}

# Cleaning database with species name of pollinator which no records

no_coordinates_pollinatorGBIF <- na.omit(as.data.frame(no_coordinates_pollinatorGBIF))

# Saving database with species name of pollinator which no records

write_xlsx(no_coordinates_pollinatorGBIF, 
           path = "./data/raw/no_coordinates_pollinatorGBIF.xlsx")

# Saving occurrence records of pollinators from GBIF

gbif_data_pollinator <- gbif

write_xlsx(gbif_data_pollinator, 
           path = "./data/raw/gbif_data_pollinator.xlsx")

# Removing unused objects hereafter

rm(data,gbif,i)

# GBIF: plants ------------------------------------------------------------

# Selecting only unique names

plants <- unique(pollination_database$plant)

# Creating a data frame to include species with no records

no_coordinates_plantGBIF <- matrix(nrow = length(plants), ncol = 1)
no_coordinates_plantGBIF <- as.data.frame(no_coordinates_plantGBIF)
colnames(no_coordinates_plantGBIF) <- "plant_scientfic_name"

# Getting occurrence records of the pollinators

gbif <- matrix(data = NA, ncol = 3)
gbif <- as.data.frame(gbif)
colnames(gbif) <- c("species_search","decimalLongitude","decimalLatitude")

for (i in 1973:length(plants)) {
  data <- rgbif2(species = plants[i], save = FALSE, limit = 99500)
  if(nrow(data) == 0){
    no_coordinates_plantGBIF[i,1] <- plants[i]
  }
  if(nrow(data) !=  0){
    data <- data[,c("species_search","decimalLongitude","decimalLatitude")]
    if(i == 1){
      gbif <- data
    } else{
      gbif <- rbind(gbif, data)
    }
  }
  print(paste(print(i), "of", length(plants)))
}

# Cleaning database with species name of pollinator which no records

no_coordinates_plantGBIF <- na.omit(as.data.frame(no_coordinates_plantGBIF))

# Saving database with species name of pollinator which no records

write_xlsx(no_coordinates_plantGBIF, 
           path = "./data/raw/no_coordinates_plantsGBIF.xlsx")

# Saving occurrence records of pollinators from GBIF

gbif_data_plant <- gbif

write_xlsx(gbif_data_plant, 
           path = "./data/raw/gbif_data_plant.xlsx")

# Removing unused objects hereafter

rm(data,gbif,i)

# Specieslink -------------------------------------------------------------

# Exporting a list of pollinator names to obtain records from the SpeciesLink

pollinator_splink <- matrix(data = NA, nrow = length(pollinator), ncol = 1)
pollinator_splink <- as.data.frame(pollinator_splink)
colnames(pollinator_splink) <- "pollinators_species_name"

for (i in 1:nrow(pollinator_splink)) {
  pollinator_splink[i,1] <- pollinator[i]
}

write_xlsx(pollinator_splink, 
           path = "./data/raw/pollinator_splink_search.xlsx")

# Exporting a list of plants names to obtain records from the SpeciesLink

plants_splink <- matrix(data = NA, nrow = length(plants), ncol = 1)
plants_splink <- as.data.frame(plants_splink)
colnames(plants_splink) <- "plants_species_name"

for (i in 1:nrow(plants_splink)) {
  plants_splink[i,1] <- plants[i]
}

write_xlsx(plants_splink, 
           path = "./data/raw/plants_splink_search.xlsx")

# Renaming objects

plants <- plants_splink
pollinator <- pollinator_splink

# Removing unused objects hereafter

rm(plants_splink, pollinator_splink, i)

# Reading pollinator occurrence records from the Specieslink --------------

splink_data_pollinator <- read_excel("./data/raw/pollinator_splink.xlsx",
                                     sheet = 1)[,c(13,14,36,37)]

# Removing lines without information

splink_data_pollinator <- na.omit(splink_data_pollinator)

# Joining columns with species name data

splink_data_pollinator$genus <- paste(splink_data_pollinator$genus, 
                                      splink_data_pollinator$species, sep = " ")

# Selecting new columns

splink_data_pollinator <- splink_data_pollinator[,c(1,3,4)]

colnames(splink_data_pollinator) <- colnames(gbif_data_pollinator)

# Reading plant occurrence records from the Specieslink -------------------

splink_data_plants <- read_xlsx("./data/raw/plants_splink.xlsx",
                                sheet = 1)

# Joining columns with species name data

splink_data_plants$genus <- paste(splink_data_plants$genus, 
                                      splink_data_plants$species, sep = " ")

# Selecting new columns

splink_data_plants <- splink_data_plants[,c(1,3,4)]

colnames(splink_data_plants) <- colnames(gbif_data_pollinator)

# Joining databases (GBIF and SpeciesLink) --------------------------------

# Changing class of the coordinates (Pollinators)

splink_data_pollinator$decimalLongitude <- as.numeric(splink_data_pollinator$decimalLongitude)
splink_data_pollinator$decimalLatitude <- as.numeric(splink_data_pollinator$decimalLatitude)

# Joining 

data_pollinator <- bind_rows(gbif_data_pollinator, splink_data_pollinator)

# Removing unused objects hereafter

rm(gbif_data_pollinator, splink_data_pollinator)

# Changing class of the coordinates (Plants)

splink_data_plants$decimalLongitude <- as.numeric(splink_data_plants$decimalLongitude)
splink_data_plants$decimalLatitude <- as.numeric(splink_data_plants$decimalLatitude)

# Joining 

data_plant <- bind_rows(gbif_data_plant, splink_data_plants)

# Removing unused objects hereafter

rm(gbif_data_plant, splink_data_plants)

# Removing duplicates -----------------------------------------------------

data_plant <- na.omit(data_plant)
data_pollinator <- na.omit(data_pollinator)

data_plant <- unique(data_plant)
data_pollinator <- unique(data_pollinator)

# Treating synonyms -------------------------------------------------------

# Pollinators

ID <- as.numeric()

for (i in 1:nrow(data_pollinator)) {
  test <- data_pollinator[i,1] %in% pollinator[,1]
  if (sum(test) == 0) {
    ID[i] <- i
  }
  print(paste(i, "of", nrow(data_pollinator)))
}; rm(test,i)

ID <- na.omit(ID)

# Removing species unlisted in our database

data_pollinator <- data_pollinator[-ID,]
rm(ID)

# Plants

ID <- as.numeric()

for (i in 1:nrow(data_plant)) {
  test <- data_plant[i,1] %in% plants[,1]
  if (sum(test) == 0) {
    ID[i] <- i
  }
  print(paste(i, "of", nrow(data_plant)))
}; rm(test,i)

ID <- na.omit(ID)

# Removing species unlisted in our database

data_plant <- data_plant[-ID,]
rm(ID)

# To execute the following line first need to prepare the environmental variables

# Creating polygon of the Neotropical realm -------------------------------

mask <- rast("./data/envi_data/clean/future/ACCESS-CM2_ssp585_2081-2100/bio01.tif")

mask <- as.polygons(mask)

mask <- aggregate(mask)

# Cropping occurrence records ---------------------------------------------

# Cropping pollinators ----------------------------------------------------

# Transforming occurrence records in sf object

data_pollinator_polyg <- st_as_sf(data_pollinator,
                                  coords = c("decimalLongitude", "decimalLatitude"), 
                                  crs = 4326)

# Cropping occurrence records using mask of Neotropical region

data_pollinator_polyg <- sf::st_crop(data_pollinator_polyg, mask)

# Extracting species name from the cropped sf object 

names <- st_drop_geometry(data_pollinator_polyg)

# Extracting longitude and latitude from the cropped sf object

data_pollinator_polyg <- as.data.frame(st_coordinates(data_pollinator_polyg))

# Joining columns

data_pollinator2 <- bind_cols(names, data_pollinator_polyg)

# Correcting column names

colnames(data_pollinator2) <- colnames(data_pollinator)

data_pollinator <- data_pollinator2

# Removing unused objects hereafter

rm(data_pollinator_polyg, data_pollinator2, names)

# Cropping plants ---------------------------------------------------------

# Transforming occurrence records in sf object

data_plant_polyg <- st_as_sf(data_plant,
                             coords = c("decimalLongitude", "decimalLatitude"), 
                             crs = 4326)

# Cropping occurrence records using mask of Neotropical region

data_plant_polyg <- sf::st_crop(data_plant_polyg, mask)

# Extracting species name from the cropped sf object 

names <- st_drop_geometry(data_plant_polyg)

# Extracting longitude and latitude from the cropped sf object

data_plant_polyg <- as.data.frame(st_coordinates(data_plant_polyg))

# Joining columns

data_plant2 <- bind_cols(names, data_plant_polyg)

# Correcting column names

colnames(data_plant2) <- colnames(data_plant)

data_plant <- data_plant2

# Removing unused objects hereafter

rm(data_plant_polyg, data_plant2, names)


n_occ_pollinator <- data_pollinator |> 
  group_by(species_search) |> 
  summarise(length(species_search))

n_occ_plant <- data_plant |> 
  group_by(species_search) |> 
  summarise(length(species_search))




data_pollinator_sf <- st_as_sf(data_pollinator, 
                               coords = c("decimalLongitude", "decimalLatitude"), 
                               crs = 4326)
# Defina a extensão da grade

grid_extent <- st_bbox(data_pollinator_sf)

# Crie uma grade de hexágonos
grid <- st_make_grid(data_pollinator_sf, cellsize = 1, what = "polygons", square = TRUE)
grid_sf <- st_sf(geometry = grid)

# Encontre as interseções entre os pontos de dados e a grade

intersections <- st_intersects(grid_sf, data_pollinator_sf)

# Calcule a riqueza de espécies (número de espécies únicas) em cada célula da grade
grid_sf$richness <- sapply(intersections, function(ids) length(unique(data_pollinator_sf$species_search[ids])))

# Substituir valores de riqueza 0 por NA
grid_sf$richness <- ifelse(grid_sf$richness == 0, NA, grid_sf$richness)

ggplot() +
  geom_sf(data = grid_sf, aes(fill = richness), color = NA) +
  scale_fill_viridis_c(na.value = "white") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Mapa de Riqueza de Espécies", fill = "Riqueza")















