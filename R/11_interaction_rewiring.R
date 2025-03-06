
# Mapping interaction rewiring --------------------------------------------

# Function to calculate secondary extinction ------------------------------

interaction_rewiring <- function(x) {
  ifelse(x[1] == 1 & x[2] == 1, 0, 
         ifelse(x[1] == 1 & x[2] == 0, 0, 
                ifelse(x[1] == 0 & x[2] == 1, 1, 
                       ifelse(x[1] == 0 & x[2] == 0, 0, NA))))
}

# Creating directories ----------------------------------------------------

dir.create("Interaction_rewiring")
dir.create("Interaction_rewiring/Current")
dir.create("Interaction_rewiring/Future")

for (i in 1:nrow(pollination_database)) {
  
  # Loading species distributions corrected by their interactions
  
  poli_current <- rast(paste("./ENM/Pollinators/Current", 
                             paste(pollination_database[i,1], 
                                   ".tif", 
                                   sep = ""), sep = "/"))
  poli_future <- rast(paste("./ENM/Pollinators/Future", 
                            paste(pollination_database[i,1], 
                                  ".tif", 
                                  sep = ""), sep = "/"))
  plant_current <- rast(paste("./ENM/Plants/Current", 
                              paste(pollination_database[i,2], 
                                    ".tif", 
                                    sep = ""), sep = "/"))
  plant_future <- rast(paste("./ENM/Plants/Future", 
                             paste(pollination_database[i,2], 
                                   ".tif", 
                                   sep = ""), sep = "/"))
  
  # Stacking rasters
  
  current_interaction <- c(poli_current, plant_current)
  future_interaction <- c(poli_future, plant_future)
  
  # Computing distribution area of the interaction
  
  current_interaction <- (app(current_interaction, fun = sum) == 2) * 1
  writeRaster(current_interaction, 
              paste("./Interaction_rewiring/Current", 
                    paste(pollination_database[i,1], 
                          pollination_database[i,2], 
                          ".tif", 
                          sep = ""), 
                    sep = "/"), 
              overwrite = TRUE)
  
  future_interaction <- (app(future_interaction, fun = sum) == 2) * 1
  writeRaster(future_interaction, 
              paste("./Interaction_rewiring/Future", 
                    paste(pollination_database[i,1], 
                          pollination_database[i,2], 
                          ".tif", 
                          sep = ""), 
                    sep = "/"), 
              overwrite = TRUE)
  
  # Stacking interactions in the current and future scenario
  
  interaction_pp <- c(current_interaction, future_interaction)
  
  interaction_pp <- app(interaction_pp, interaction_rewiring)
  writeRaster(interaction_pp, 
              paste("./Interaction_rewiring", 
                    paste(pollination_database[i,1], 
                          pollination_database[i,2], 
                          ".tif", 
                          sep = ""), 
                    sep = "/"), 
              overwrite = TRUE)
  print(i)
}

rm(current_interaction, future_interaction, interaction_pp, plant_current, 
   plant_future, poli_current, poli_future, i, interaction_rewiring)

# Listing files -----------------------------------------------------------

files_interaction <- list.files("./Interaction_rewiring/Current", pattern = ".tif")

# Functions to calculate interaction rewiring -----------------------------

# The functions below require a stack file with absence-presence data of the interaction

a_calc <- function(x) {
  ifelse(x[1] == 1 & x[2] == 1, 1, 
         ifelse(x[1] == 1 & x[2] == 0, 0, 
                ifelse(x[1] == 0 & x[2] == 1, 0, 
                       ifelse(x[1] == 0 & x[2] == 0, 0, NA))))
}

b_calc <- function(x) {
  ifelse(x[1] == 1 & x[2] == 1, 0, 
         ifelse(x[1] == 1 & x[2] == 0, 1, 
                ifelse(x[1] == 0 & x[2] == 1, 0, 
                       ifelse(x[1] == 0 & x[2] == 0, 0, NA))))
}


c_calc <- function(x) {
  ifelse(x[1] == 1 & x[2] == 1, 0, 
         ifelse(x[1] == 1 & x[2] == 0, 0, 
                ifelse(x[1] == 0 & x[2] == 1, 1, 
                       ifelse(x[1] == 0 & x[2] == 0, 0, NA))))
}

# The functions below require a stack file with the data obtained by the functions above

calc_beta_w <- function(x) {
  return(((x[1] + x[2] + x[3]) / ((2 * x[1] + x[2] + x[3]) / 2)) - 1)
}

calc_jaccard <- function(x) {
  return((x[2] + x[3]) / ((x[1] + x[2] + x[3])))
}

calc_sorensen <- function(x) {
  return((x[2] + x[3]) / (2* x[1] + x[2] + x[3]))
}

calc_simpson <- function(x) {
  return((min(x[2], x[3])) / (x[1] + min(x[2], x[3])))
}

# Computing components of interaction rewiring ----------------------------

for (i in 1:length(files_interaction)) {
  
  # Loading interaction rasters
  
  current_interaction <- rast(paste("./Interaction_rewiring/Current",
                                    files_interaction[i], sep = "/"))
  future_interaction <- rast(paste("./Interaction_rewiring/Future",
                                   files_interaction[i], sep = "/"))
  
  # Stacking interaction rasters
  
  interaction_cf <- c(current_interaction, future_interaction)
  
  interaction_a <- app(interaction_cf, a_calc)
  interaction_b <- app(interaction_cf, b_calc)
  interaction_c <- app(interaction_cf, c_calc)
  
  if (i == 1) {
    int_a <- interaction_a
  } else {
    int_a <- c(int_a, interaction_a)
    int_a <- app(int_a, sum)
  }
  if (i == 1) {
    int_b <- interaction_b
  } else {
    int_b <- c(int_b, interaction_b)
    int_b <- app(int_b, sum)
  }
  if (i == 1) {
    int_c <- interaction_c
  } else {
    int_c <- c(int_c, interaction_c)
    int_c <- app(int_c, sum)
  }
  
  print(i)
}

writeRaster(int_a, 
            filename = "./Results/rasters/a_calc.tif", 
            overwrite = TRUE)

writeRaster(int_b, 
            filename = "./Results/rasters/b_calc.tif", 
            overwrite = TRUE)

writeRaster(int_c, 
            filename = "./Results/rasters/c_calc.tif", 
            overwrite = TRUE)

# Removing unused objects from environment --------------------------------

rm(current_interaction, future_interaction, int_a, int_b, int_c, interaction_a,
   interaction_b, interaction_c, interaction_cf, files_interaction,i,
   a_calc, b_calc, c_calc)

# Loading files of the interaction number ---------------------------------

int_a <- rast("./Results/rasters/a_calc.tif")
int_b <- rast("./Results/rasters/b_calc.tif")
int_c <- rast("./Results/rasters/c_calc.tif")

# Stacking rasters

int <- c(int_a, int_b, int_c)

# Mapping interactions ----------------------------------------------------

int_a <- as.data.frame(int_a, xy = TRUE)

int_a_map <- ggplot(data = int_a, 
                    aes(x = x, 
                    y = y, 
                    fill = sum))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  ggtitle("Interactions shared in the\ncurrent and future scenario") +
  scale_fill_viridis_c(name = "Number of\ninteractions") +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = "a", 
           color = "black", size = 7) +
  theme(legend.position = c(0.88, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18))

int_b <- as.data.frame(int_b, xy = TRUE)

int_b_map <- ggplot(data = int_b, 
                    aes(x = x, 
                        y = y, 
                        fill = sum))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  ggtitle("Unique interactions in the\ncurrent scenario") +
  scale_fill_viridis_c(name = "Number of\ninteractions") +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = "b", 
           color = "black", size = 7) +
  theme(legend.position = c(0.88, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18))


int_c <- as.data.frame(int_c, xy = TRUE)

int_c_map <- ggplot(data = int_c, 
                    aes(x = x, 
                        y = y, 
                        fill = sum))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  ggtitle("Unique interactions in the\nfuture scenario") +
  scale_fill_viridis_c(name = "Number of\ninteractions") +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = "c", 
           color = "black", size = 7) +
  theme(legend.position = c(0.88, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18))


interaction_components <- grid.arrange(int_a_map, int_b_map, int_c_map, ncol = 3)

ggsave(plot = interaction_components, 
       filename = "./Results/components_interaction_rewiring.tif",
       dpi = 300,
       width = 9.5,
       height = 4.5)

# Computing beta diversity and dissimilarity of interactions --------------

# Beta diversity

beta_w <- app(int, fun = function(x) calc_beta_w(x))
writeRaster(beta_w, 
            filename = "./Results/rasters/beta_w.tif", 
            overwrite = TRUE)

# Jaccard's dissimilarity

jaccard <- app(int, fun = function(x) calc_jaccard(x))
writeRaster(jaccard, 
            filename = "./Results/rasters/dissimilarity_jaccard.tif", 
            overwrite = TRUE)

# Sorensen's dissimilarity

sorensen <- app(int, fun = function(x) calc_sorensen(x))
writeRaster(sorensen, 
            filename = "./Results/rasters/dissimilarity_sorensen.tif", 
            overwrite = TRUE)

# Simpson's dissimilarity

simpson <- app(int, fun = function(x) calc_simpson(x))
writeRaster(simpson, 
            filename = "./Results/rasters/dissimilarity_simpson.tif", 
            overwrite = TRUE)

# Mapping dissimilarity ---------------------------------------------------


jaccard <- as.data.frame(jaccard, xy = TRUE)

jaccard_map <- ggplot(data = jaccard, 
                    aes(x = x, 
                        y = y, 
                        fill = lyr.1))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  scale_fill_viridis_c(name = "Jaccard's\ndissimilarity") +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = "a", 
           color = "black", size = 7) +
  theme(legend.position = c(0.88, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18))


sorensen <- as.data.frame(sorensen, xy = TRUE)

sorensen_map <- ggplot(data = sorensen, 
                    aes(x = x, 
                        y = y, 
                        fill = lyr.1))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  scale_fill_viridis_c(name = "Sorensen's\ndissimilarity") +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = "b", 
           color = "black", size = 7) +
  theme(legend.position = c(0.88, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18))


simpson <- as.data.frame(simpson, xy = TRUE)

simpson_map <- ggplot(data = simpson, 
                    aes(x = x, 
                        y = y, 
                        fill = lyr.1))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  scale_fill_viridis_c(name = "Simpson's\ndissimilarity") +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = "c", 
           color = "black", size = 7) +
  theme(legend.position = c(0.88, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18))


dissimilarity <- grid.arrange(jaccard_map, sorensen_map, simpson_map, ncol = 3)


ggsave(plot = dissimilarity, 
       filename = "./Results/dissimilarities.png",
       dpi = 300,
       width = 10.1,
       height = 4.5)

rm(beta_w, dissimilarity, int, int_a, int_b, int_c, int_a_map, int_b_map,
   int_c_map, interaction_components, jaccard, jaccard_map, simpson, simpson_map,
   sorensen, sorensen_map, calc_beta_w, calc_jaccard, calc_sorensen, calc_simpson)

# Mapping the number of new interactions in the future --------------------

files_int <- list.files("./Interaction_rewiring", pattern = ".tif")

for (i in 1:length(files_int)) {
  int <- rast(paste("./Interaction_rewiring", files_int[i], sep = "/"))
  if (i == 1) {
    int_stack <- int
  } else {
    int_stack <- app(c(int_stack, int), fun = sum)
  }
  print(i)
}

writeRaster(int_stack, 
            filename = "./Results/rasters/interactions_exclusive_in_future.tif", 
            overwrite = TRUE)

rm(int, int_stack, files_int, i)





