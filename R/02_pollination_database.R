
# Load interaction data from A.B.E.L.H.A database -------------------------

data_abelha <- read_xlsx("./data/raw/interaction_database_abelha.xlsx", 
                         sheet = 1)[,c(5,6,10,11)]

# joint genus and species name

data_abelha <- data_abelha |>  
  mutate(pollinator = paste(genero_abelha_trabalho, especie_abelha_trabalho, 
                            sep = " ")) |>  
  mutate(plant = paste(genero_planta_trabalho, especie_planta_trabalho, 
                       sep = " "))

data_abelha <- data_abelha[,c(5,6)]

# Load Natacha et al. 2022 ------------------------------------------------

Natacha <- read.csv("./data/raw/Natacha_et_al_2022/visits_2006-2011_data.csv",
                     sep = ",")

plant_names <- read_xlsx("./data/raw/Natacha_et_al_2022/plant_names.xlsx", 
                         sheet = 1)

pollination_names <- read.csv("./data/raw/Natacha_et_al_2022/insect_names.csv",
                              sep = ",")

# Merging databases

Natacha <- merge(Natacha, pollination_names, 
                 by.x = "codigo.i", 
                 by.y = "pol_code")

Natacha <- merge(Natacha, plant_names, 
                 by = "codigo.p")

Natacha <- Natacha[,c(12,13)]

colnames(Natacha) <- colnames(data_abelha)

# Verifying species with incomplete taxonomic identification

# Pollinations

contains_sp <- str_detect(Natacha$pollinator, "sp")

Natacha <- Natacha[!contains_sp,]

# Plants

contains_sp <- str_detect(Natacha$plant, "sp")

Natacha <- Natacha[!contains_sp,]

# Eliminating empty characters

Natacha <- subset(Natacha, pollinator != "")

# Correcting species names

Natacha$pollinator[Natacha$pollinator == "Megachile_pseudocentron2"] <- "Megachile_pseudocentron"
Natacha <- Natacha

Natacha$pollinator <- gsub("_", " ", Natacha$pollinator)
Natacha$plant <- gsub("_", " ", Natacha$plant)

# Joining to main database

data_abelha <- bind_rows(data_abelha, Natacha)

# Removing unused objects hereafter

rm(Natacha, plant_names, pollination_names, contains_sp)

# Vazquez_2022 ------------------------------------------------------------

# Reading complete network

Vazquez_network <- read_xlsx("./data/raw/vazquez_2002/vazquez_2002.xlsx", 
                             sheet = 3,
                             col_names = TRUE)

Vazquez_network <- Vazquez_network |> 
  pivot_longer(-pollinator,
               names_to = "plant",
               values_to = "n")

# Reading data with species names

# Pollinators

pollinator_name <- read_xlsx("./data/raw/vazquez_2002/vazquez_2002.xlsx", 
                         sheet = 2,
                         col_names = TRUE)[,c(1,4)]

# Plants

plants_name <- read_xlsx("./data/raw/vazquez_2002/vazquez_2002.xlsx", 
                         sheet = 1,
                         col_names = TRUE)[,c(1,3)]

# Attributing species name do the network

Vazquez_network <- merge(Vazquez_network, pollinator_name, 
                          by.x = "pollinator", 
                          by.y = "Species no.")

Vazquez_network <- merge(Vazquez_network, plants_name, 
                         by.x = "plant", 
                         by.y = "Species no.")

# Selecting columns

Vazquez_network <- Vazquez_network[,c(4,5)]

# Naming columns

colnames(Vazquez_network) <- colnames(data_abelha)
Vazquez_network <- Vazquez_network

# Removing NA data
  
contains_NA <- str_detect(Vazquez_network$pollinator, "NA")

Vazquez_network <- Vazquez_network[!contains_NA,]

# Verifying species with incomplete taxonomic identification

Vazquez_network <- Vazquez_network |> 
  filter(!str_detect(pollinator, " sp"),
         !str_detect(plant, " sp"))

# Joining to main database

data_abelha <- bind_rows(data_abelha, Vazquez_network)

# Removing unused objects hereafter

rm(plants_name, pollinator_name, contains_NA, Vazquez_network)

# Removing duplicates in the main dataset ---------------------------------

data_abelha <- unique(data_abelha)

# Taxonomic verification --------------------------------------------------

# Pollinators

pollinator <- bdc_clean_names(data_abelha$pollinator)

# Plants

plant <- bdc_clean_names(data_abelha$plant)

# Combining clean names

pollination_database <- bind_cols(pollinator[,4], plant[,4])

colnames(pollination_database) <- colnames(data_abelha)

# Removing unused objects hereafter

rm(plant, pollinator, data_abelha)

# Exporting data

write_xlsx(pollination_database, "./data/clean/pollination_database.xlsx")
