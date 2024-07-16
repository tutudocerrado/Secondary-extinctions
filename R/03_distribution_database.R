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

# Records of crop occurrence up to the Neotropical realm ------------------

# Defining the coordinates of the neotropical region

neotropical_bbox <- list(xmin = -120, xmax = -30, ymin = -60, ymax = 30)

# Filtering the data for the neotropical region

dados_neotropical <- data_pollinator |> 
  filter(data_pollinator$decimalLongitude >= neotropical_bbox$xmin & data_pollinator$decimalLongitude <= neotropical_bbox$xmax &
           data_pollinator$decimalLatitude >= neotropical_bbox$ymin & data_pollinator$decimalLatitude <= neotropical_bbox$ymax)























