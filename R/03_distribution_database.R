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

# Creating empty matrix to save pollinator names

pollinator_splink <- matrix(data = NA, nrow = length(pollinator), ncol = 1)
pollinator_splink <- as.data.frame(pollinator_splink)
colnames(pollinator_splink) <- "pollinators_species_name"

# Saving pollinator names in the dataframe

for (i in 1:nrow(pollinator_splink)) {
  pollinator_splink[i,1] <- pollinator[i]
}

write_xlsx(pollinator_splink, 
           path = "./data/raw/pollinator_splink_search.xlsx")

# Exporting a list of plants names to obtain records from the SpeciesLink

# Creating empty matrix to save pollinator names

plants_splink <- matrix(data = NA, nrow = length(plants), ncol = 1)
plants_splink <- as.data.frame(plants_splink)
colnames(plants_splink) <- "plants_species_name"

for (i in 1:nrow(plants_splink)) {
  plants_splink[i,1] <- plants[i]
}

# Saving plant names in the dataframe

write_xlsx(plants_splink, 
           path = "./data/raw/plants_splink_search.xlsx")

# Renaming objects

plants <- plants_splink
pollinator <- pollinator_splink

# Removing unused objects hereafter

rm(plants_splink, pollinator_splink, i)

# Note: the SpeciesLink data were obtained directly from the database

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

data_pollinator <- data_pollinator |> 
  mutate(species_search = gsub(" ", "_", species_search))

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

data_plant <- data_plant |> 
  mutate(species_search = gsub(" ", "_", species_search))

# Removing unused objects hereafter

rm(data_plant_polyg, data_plant2, names)

# Unique occurrence count by species

n_occ_pollinator <- data_pollinator |> 
  group_by(species_search) |> 
  summarise(n = length(species_search))

n_occ_plant <- data_plant |> 
  group_by(species_search) |> 
  summarise(n = length(species_search))

# Organizing occurrence data set in two groups ----------------------------

# Pollinators: >= 5 and < 20

filter_species_pollinator5 <- n_occ_pollinator |> 
  filter(n >= 5 & n < 20)

# Pollinators: > 20

filter_species_pollinator20 <- n_occ_pollinator |> 
  filter(n > 20)

# Filtering original data of pollinators

data_pollinators5 <- data_pollinator |> 
  filter(species_search %in% filter_species_pollinator5$species_search)
  
data_pollinators20 <- data_pollinator |> 
  filter(species_search %in% filter_species_pollinator20$species_search)

# Saving new occurrence data sets: pollinators

write.table(x = data_pollinators5, 
            file = "./data/clean/data_pollinators5.txt", 
            row.names = FALSE)

write.table(x = data_pollinators20, 
            file = "./data/clean/data_pollinators20.txt", 
            row.names = FALSE)

# After run the niche model using the checkerboard method for pollinators
# some species required a new modeling using boot method

# Species list

pollinator_sp <- c("Augochlora_semiramis","Augochloropsis_cleopatra","Callonychium_petuniae",
                   "Centris_burgdorfi","Ceratina_maculifrons","Coelioxoides_waltheriae",
                   "Compsomyiops_fulvicrura","Dialictus_opacus","Epanthidium_autumnale",
                   "Eufriesea_mussitans","Euglossa_melanotricha","Lanthanomelissa_betinae",
                   "Lanthanomelissa_clementis","Megachile_aetheria","Megachile_apicipennis",
                   "Megachile_brasiliensis","Megachile_fiebrigi","Megachile_lentifera",
                   "Megachile_orbiculata","Mesonychium_asteria","Mesonychium_jenseni",
                   "Paratetrapedia_fervida","Paratetrapedia_lineata","Paratrigona_glabrata",
                   "Parepeolus_aterrimus","Paroxystoglossa_andromache","Trichothurgus_laticeps",
                   "Protomeliturga_turnerae","Psaenythia_bergii","Pseudagapostemon_arenarius",
                   "Pseudagapostemon_pruinosus")

# Transforming in a data frame

pollinator_sp <- data.frame(species = pollinator_sp)

# Selecting coordinates in the main distribution database

data_pollinator5.1 <- data_pollinator |> 
  filter(species_search %in% pollinator_sp$species)

# Saving data

write.table(x = data_pollinator5.1, 
            file = "./data/clean/data_pollinators51.txt", 
            row.names = FALSE)

# Reducing the number of species to be modeled 200 records ----------------

# Plants: > 20

filter_species_plant20 <- n_occ_plant |> 
  filter(n > 20)

# > 20 records

# 1:200

filter_species_plant20.1 <- filter_species_plant20[c(1:200),]

data_plants20.1 <- data_plant |> 
  filter(species_search %in% filter_species_plant20.1$species_search)

# 201:400

filter_species_plant20.2 <- filter_species_plant20[c(201:400),]

data_plants20.2 <- data_plant |> 
  filter(species_search %in% filter_species_plant20.2$species_search)

# 401:600

filter_species_plant20.3 <- filter_species_plant20[c(401:600),]

data_plants20.3 <- data_plant |> 
  filter(species_search %in% filter_species_plant20.3$species_search)

# 601:800

filter_species_plant20.4 <- filter_species_plant20[c(601:800),]

data_plants20.4 <- data_plant |> 
  filter(species_search %in% filter_species_plant20.4$species_search)

# 801:1000

filter_species_plant20.5 <- filter_species_plant20[c(801:1000),]

data_plants20.5 <- data_plant |> 
  filter(species_search %in% filter_species_plant20.5$species_search)

# 1001:1200

filter_species_plant20.6 <- filter_species_plant20[c(1001:1200),]

data_plants20.6 <- data_plant |> 
  filter(species_search %in% filter_species_plant20.6$species_search)

# 1201:1400

filter_species_plant20.7 <- filter_species_plant20[c(1201:1400),]

data_plants20.7 <- data_plant |> 
  filter(species_search %in% filter_species_plant20.7$species_search)

# 1401:1600

filter_species_plant20.8 <- filter_species_plant20[c(1401:1600),]

data_plants20.8 <- data_plant |> 
  filter(species_search %in% filter_species_plant20.8$species_search)

# 1601:1800

filter_species_plant20.9 <- filter_species_plant20[c(1601:1800),]

data_plants20.9 <- data_plant |> 
  filter(species_search %in% filter_species_plant20.9$species_search)

# 1801:1996

filter_species_plant20.10 <- filter_species_plant20[c(1801:1996),]

data_plants20.10 <- data_plant |> 
  filter(species_search %in% filter_species_plant20.10$species_search)

# Plants: >= 5 and <= 20

filter_species_plant5 <- n_occ_plant |>
  filter(n >= 5 & n <= 20)

# Filtering original data of plants

data_plants5 <- data_plant |> 
  filter(species_search %in% filter_species_plant5$species_search)

# After run the niche model using the checkerboard method for plants
# some species required a new modeling using boot method

# Species list

plants_sp <- c("Abutilon_malachroides","Acacia_mearnsii","Acacia_plumosa",
               "Aleurites_fordii","Amaranthus_retroflexus","Archontophoenix_cunninghamia",
               "Archontophoenix_cunninghamiana","Baccharis_axilaris","Baccharis_concinna",
               "Baccharis_pseudovillosa","Banisteriopsis_cipoensis","Begonia_semperflorens",
               "Aptandra_liriosmoides","Baccharis_conyzoides","Barbacenia_flava",
               "Begonia_solananthera","Bernardia_crassifolia","Bombax_ceiba",
               "Brassica_campestris","Brassica_napus","Bredemeyera_microphylla",
               "Bulbine_frutescens","Caesalpinia_bonducella","Callistemon_citrinus",
               "Callistemon_lanceolatus","Camellia_japonica","Castanea_sativa",
               "Bidens_tinctoria","Calceolaria_crenatiflora","Cambessedesia_corymbosa",
               "Chamaechrista_ochnacea","Chamaechrista_vestita","Chrysanthemum_frutescens",
               "Chrysanthemum_myconis","Commelina_virginica","Cordia_lanata",
               "Coreopsis_grandiflora","Crinum_powellii","Croton_ericoides","Croton_priscus",
               "Cunila_microcephala","Diospyros_kaki","Dioclea_rufescens","Dipyrena_glaberrima",
               "Cuphea_pseudovaccinium","Diplusodon_orbicularis","Dyckia_maritima",
               "Eclipta_megapotamica","Eryngium_ciliatum","Eucalyptus_tereticornis",
               "Gardenia_jasminoides","Gazania_rigens","Geranium_robertianum",
               "Grazielia_gaudichaudiana","Grevillea_banskii","Hemerocallis_fulva",
               "Hiraea_cuneata","Hyptis_heterodon","Hyptis_lagenaria","Jacquemontia_grandiflora",
               "Jasminum_azoricum","Jasminum_nudiflorum","Jatropha_urens","Leucaena_glauca",
               "Lafoensia_nummularifolia","Inga_lenticellata","Ipomoea_nitida",
               "Jungia_sellowii","Kelissa_brasiliensis","Lavoisiera_cordata","Lilium_longiflorum",
               "Lippia_hirta","Lychnophora_rupestris","Mikania_conferta","Mikania_diversifolia",
               "Mikania_ulei","Mimosa_conyflora","Mimosa_per-dusenii","Mora_paraensis",
               "Myrcia_rotundifolia","Nothoscordum_inodorum","Oxalis_subvillosa",
               "Pamphalea_heterophylla","Paulownia_tomentosa","Peltodon_rugosus",
               "Petunia_hybrida","Luxemburgia_schwackeana","Menodora_decemfida",
               "Miconia_regnellii","Mimosa_maguirei","Oxalis_sellowiana","Paullinia_seminuda",
               "Pavonia_friesii","Petunia_ericifolia","Polygonum_capitatum",
               "Pontederia_lanceolata","Prumus_persica","Pyracantha_coccinea",
               "Quisqualis_indica","Schefflera_arboricola","Pterolepis_alpestris",
               "Rhabdocaulon_gracilis","Richterago_polyphylla","Senna_rostrata",
               "Sida_carpinifolia","Solanum_inaequale","Solanum_martii","Sorocea_ilicifolia",
               "Spondias_lutea","Stevia_cinerascens","Stilpnopappus_pantanalensis",
               "Struthanthus_confertus","Taraxacum_offcinale","Solanum_subsylvestris", 
               "Stachytarpheta_cayannensis","Tetrapanax_papyrifera","Tibouchina_regnellii",
               "Verbena_hybrida","Vernonia_tragiaefolia","Veronica_polita","Viguiera_trichophylla",
               "Wedelia_paludosa","Zanthedeschia_aetiopica","Tibouchina_chamissoana",
               "Tibouchina_debilis","Trembleya_laniflora","Vellozia_compacta","Verbenoxylum_reitzii",
               "Vernonia_hypochlora","Vochysia_pygmaea","Wahlenbergia_brasiliensis",
               "Weinmannia_discolor","Yucca_filamentosa")

# Transforming in a data frame

plants_sp <- data.frame(species = plants_sp)

# Selecting coordinates in the main distribution database

data_plants5.1 <- data_plant |> 
  filter(species_search %in% plants_sp$species)

data_plants5 <- bind_rows(data_plants5, data_plants5.1)

# Saving new occurrence data sets: plants

write.table(x = data_plants5, 
            file = "./data/clean/data_plants5.txt", 
            row.names = FALSE)

# > 20 occurrences

write.table(x = data_plants20.1, 
            file = "./data/clean/data_plants20_1.txt", 
            row.names = FALSE)
write.table(x = data_plants20.2, 
            file = "./data/clean/data_plants20_2.txt", 
            row.names = FALSE)
write.table(x = data_plants20.3, 
            file = "./data/clean/data_plants20_3.txt", 
            row.names = FALSE)
write.table(x = data_plants20.4, 
            file = "./data/clean/data_plants20_4.txt", 
            row.names = FALSE)
write.table(x = data_plants20.5, 
            file = "./data/clean/data_plants20_5.txt", 
            row.names = FALSE)
write.table(x = data_plants20.6, 
            file = "./data/clean/data_plants20_6.txt", 
            row.names = FALSE)
write.table(x = data_plants20.7, 
            file = "./data/clean/data_plants20_7.txt", 
            row.names = FALSE)
write.table(x = data_plants20.8, 
            file = "./data/clean/data_plants20_8.txt", 
            row.names = FALSE)
write.table(x = data_plants20.9, 
            file = "./data/clean/data_plants20_9.txt", 
            row.names = FALSE)
write.table(x = data_plants20.10, 
            file = "./data/clean/data_plants20_10.txt", 
            row.names = FALSE)

rm(pollinator, no_coordinates_plantGBIF, no_coordinates_pollinatorGBIF, 
   gbif_data_pollinator, plants, pollinator_splink, plants_splink, pollinator, 
   splink_data_pollinator, data_pollinator, data_plant, n_occ_plant, 
   n_occ_pollinator, filter_species_plant20, filter_species_plant20.1, 
   filter_species_plant20.10, filter_species_plant20.2, filter_species_plant20.3, 
   filter_species_plant20.4, filter_species_plant20.5, filter_species_plant20.6, 
   filter_species_plant20.7, filter_species_plant20.8, filter_species_plant20.9,
   filter_species_pollinator20, filter_species_pollinator5, data_pollinator5.1, 
   data_pollinators20, data_pollinators5, pollinator_sp, plants_sp, data_plant5.1, 
   data_plants20, data_plants20.1, data_plants20.10, data_plants20.2, data_plants20.3, 
   data_plants20.4, data_plants20.5, data_plants20.6, data_plants20.7, data_plants20.8, 
   data_plants20.9, data_plants5, data_plants5.1, filter_species_plant5)
