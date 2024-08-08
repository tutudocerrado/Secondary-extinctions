# Loading packages --------------------------------------------------------

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("terra", 
              "readxl", 
              "tidyverse", 
              "stringr",
              "bdc",
              "writexl",
              "Rocc",
              "flexsdm",
              "sf",
              "rgeos",
              "speciesgeocodeR",
              "rgdal",
              "ENMTML")

ipak(packages); rm(packages,ipak)

# Packages from GitHub ----------------------------------------------------

# Use the code bellow in case the Rocc package is not installed

remotes::install_github("liibre/Rocc")

# Use the code bellow in case the flexsdm package is not installed

remotes::install_github("sjevelazco/flexsdm")

# Use the code bellow in case the speciesgeocodeR package is not installed

remotes::install_github("azizka/speciesgeocodeR")

# Use the code bellow in case the rgdal package is not installed
# Before run this code, install RTools in the desktop

remotes::install_github("cran/rgdal")

# Use the code bellow in case the rgeos package is not installed
# Before run this code, install RTools in the desktop

remotes::install_github("cran/rgeos")

# Use the code bellow in case the ENMTML package is not installed
# Before run this code, install RTools in the desktop

remotes::install_github("andrefaa/ENMTML")
