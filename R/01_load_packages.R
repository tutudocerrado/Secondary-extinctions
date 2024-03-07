# Loading packages --------------------------------------------------------

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("reticulate")

# packages <- c("readxl","Rocc","terra","sdmpredictors","rgdal","sf","flexsdm", 
#               "tidyverse","ENMTML")

ipak(packages); rm(packages,ipak)

# Use the code bellow in case the Rocc package is not installed

remotes::install_github("liibre/Rocc")

# Use the code bellow in case the flexsdm package is not installed

remotes::install_github("sjevelazco/flexsdm")


remotes::install_github("envirometrix/landmap")

remotes::install_github('gearslaboratory/gdalUtils')