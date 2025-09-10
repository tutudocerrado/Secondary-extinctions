# Testing hypothesis ------------------------------------------------------

# Richness versus primary extinction  ------------------------------------

# Loading rasters

richness <- rast("./Results/rasters/Current_total_richness.tif")
primary_extinction <- rast("./Results/rasters/primary_extinction.tif")
secondary_extinction <- rast("./Results/rasters/secondary_extinction.tif")

# Converting in dataframe

richness <- as.data.frame(richness, xy = TRUE)
colnames(richness) <- c("x","y","richness")
primary_extinction <- as.data.frame(primary_extinction, xy = TRUE)
colnames(primary_extinction) <- c("x","y","primary_extinction")
secondary_extinction <- as.data.frame(secondary_extinction, xy = TRUE)
colnames(secondary_extinction) <- c("x","y","secondary_extinction")

# Merging dataframes

df_rich_prim_ext <- inner_join(richness, primary_extinction, by = c("x", "y"))

# Evaluating overdispersion

poisson_model <- glm(primary_extinction ~ richness, 
                     family = poisson, 
                     data = df_rich_prim_ext)

# Índice de superdispersão (deve ser próximo de 1 no Poisson)

dispersion <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
print(dispersion)

# Generalized Additive Models

modelo_bam <- bam(primary_extinction ~ s(richness, k = 15) + s(x, y, k = 2000),
                  family = nb(), 
                  data = df_rich_prim_ext, 
                  method = "fREML",
                  discrete = TRUE)

summary(modelo_bam)

gam.check(modelo_bam)

# predict 

df_rich_prim_ext$predito <- predict(modelo_bam, type = "response")

# residuals 

df_rich_prim_ext$residuos <- residuals(modelo_bam, type = "deviance")

# Maps (Supplementary material) -------------------------------------------

S1 <- ggplot(df_rich_prim_ext, aes(x = x, y = y, fill = predito)) +
  geom_tile() +
  scale_fill_viridis_c(name = "\nPredicted") +
  coord_equal() +
  ggtitle("Primary Extinction ~ Richness") +
  theme_nothing() +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = expression(bold("a")), 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 16))


S2 <- ggplot(df_rich_prim_ext, aes(x = x, y = y, fill = residuos)) +
  geom_tile() +
  scale_fill_viridis_c(name = "\nResiduals") +
  coord_equal() +
  ggtitle("") +
  theme_nothing() +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = expression(bold("b")), 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 16))

# Richness versus secondary extinction ------------------------------------

# Merging dataframes

df_rich_sec_ext <- inner_join(richness, secondary_extinction, by = c("x", "y"))

# Evaluating overdispersion

poisson_model <- glm(secondary_extinction ~ richness, 
                     family = poisson, 
                     data = df_rich_sec_ext)

# Índice de superdispersão (deve ser próximo de 1 no Poisson)

dispersion <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
print(dispersion)

# Generalized Additive Models

modelo_bam2 <- bam(secondary_extinction ~ s(richness, k=30) + s(x, y, k = 2000, bs = "ds"),
                  family = nb(), 
                  data = df_rich_sec_ext, 
                  method = "fREML",
                  discrete = TRUE)


summary(modelo_bam2)
gam.check(modelo_bam2)

# predict 

df_rich_sec_ext$predito <- predict(modelo_bam2, type = "response")

# residuals 

df_rich_sec_ext$residuos <- residuals(modelo_bam2, type = "deviance")

# Maps (Supplementary material) -------------------------------------------

S3 <- ggplot(df_rich_sec_ext, aes(x = x, y = y, fill = predito)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Predicted") +
  coord_equal() +
  ggtitle("Secondary Extinction ~ Richness") +
  theme_nothing() +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = expression(bold("c")), 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 16))


S4 <- ggplot(df_rich_sec_ext, aes(x = x, y = y, fill = residuos)) +
  geom_tile() +
  scale_fill_viridis_c(name = "\nResiduals") +
  coord_equal() +
  ggtitle("") +
  theme_nothing() +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = expression(bold("d")), 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 16))

# Richness versus colonization --------------------------------------------

colonization <- rast("./Results/rasters/colonization.tif")

colonization <- as.data.frame(colonization, xy = TRUE)
colnames(colonization) <- c("x","y","colonization")

# Merging dataframes

df_rich_col <- inner_join(richness, colonization, by = c("x", "y"))

# Evaluating overdispersion

poisson_model <- glm(colonization ~ richness, 
                     family = poisson, 
                     data = df_rich_col)

# Índice de superdispersão (deve ser próximo de 1 no Poisson)

dispersion <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
print(dispersion)


gam_model3 <- bam(colonization ~ s(richness, k = 50) + s(x, y, k = 2000, bs = "ds"),
                  family = nb(), 
                  data = df_rich_col, 
                  method = "fREML",
                  discrete = TRUE)

summary(gam_model3)

gam.check(gam_model3)

# predict 

df_rich_col$predito <- predict(gam_model3, type = "response")

# residuals 

df_rich_col$residuos <- residuals(gam_model3, type = "deviance")

# Maps (Supplementary material) -------------------------------------------

S5 <- ggplot(df_rich_col, aes(x = x, y = y, fill = predito)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Predicted") +
  coord_equal() +
  ggtitle("Colonization ~ Richness") +
  theme_nothing() +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = expression(bold("e")), 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 16))


S6 <- ggplot(df_rich_col, aes(x = x, y = y, fill = residuos)) +
  geom_tile() +
  scale_fill_viridis_c(name = "\nResiduals") +
  coord_equal() +
  ggtitle("") +
  theme_nothing() +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = expression(bold("f")), 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 16))
# Primary extinction versus Secondary extinction --------------------------

# Loading rasters

primary_extinction <- rast("./Results/rasters/primary_extinction.tif")
secondary_extinction <- rast("./Results/rasters/secondary_extinction.tif")

# Converting in dataframe

primary_extinction <- as.data.frame(primary_extinction, xy = TRUE)
colnames(primary_extinction) <- c("x","y","primary_extinction")
secondary_extinction <- as.data.frame(secondary_extinction, xy = TRUE)
colnames(secondary_extinction) <- c("x","y","secondary_extinction")

# Merging dataframes

df_extinctions <- cbind(primary_extinction, secondary_extinction$secondary_extinction)
colnames(df_extinctions) <- c("x","y","primary_extinction","secondary_extinction")

# Removing all lines that primary extinction is equal to zero

df_extinctions <- subset(df_extinctions, primary_extinction > 0)

# Evaluating overdispersion

poisson_model <- glm(secondary_extinction ~ primary_extinction, 
                     family = poisson, 
                     data = df_extinctions)

# Índice de superdispersão (deve ser próximo de 1 no Poisson)

dispersion <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
print(dispersion)

# Generalized Additive Models

gam_model <- bam(
  secondary_extinction ~ s(primary_extinction, k = 20) + s(x, y, k = 2000, bs = "ds"),
  family = nb(theta = 10),  # ou poisson
  data = df_extinctions,
  method = "fREML",
  discrete = TRUE
)

summary(gam_model)

gam.check(gam_model)

# predict 

df_extinctions$predito <- predict(gam_model, type = "response")

# residuals 

df_extinctions$residuos <- residuals(gam_model, type = "deviance")

# Maps (Supplementary material) -------------------------------------------

S7 <- ggplot(df_extinctions, aes(x = x, y = y, fill = predito)) +
  geom_tile() +
  scale_fill_viridis_c(name = "\nPredicted") +
  coord_equal() +
  ggtitle("Secondary Extinction ~ Primary Extinction") +
  theme_nothing() +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = expression(bold("g")), 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 16))


S8 <- ggplot(df_extinctions, aes(x = x, y = y, fill = residuos)) +
  geom_tile() +
  scale_fill_viridis_c(name = "\nResiduals") +
  coord_equal() +
  ggtitle("") +
  theme_nothing() +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = expression(bold("h")), 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 16))

# Plants' primary extinction versus pollinators' secondary extinct --------

plants_primary_extinction <- rast("./Results/rasters/primary_extinction_plants.tif")
pollinator_secondary_extinction <- rast("./Results/rasters/secondary_extinction_pollinators.tif")

# Converting in dataframe

plants_primary_extinction <- as.data.frame(plants_primary_extinction, xy = TRUE)
colnames(plants_primary_extinction) <- c("x","y","plants_primary_extinction")
pollinator_secondary_extinction <- as.data.frame(pollinator_secondary_extinction, xy = TRUE)
colnames(pollinator_secondary_extinction) <- c("x","y","pollinator_secondary_extinction")

# Merging dataframes

df_extinctions_group1 <- cbind(plants_primary_extinction, pollinator_secondary_extinction$pollinator_secondary_extinction)
colnames(df_extinctions_group1) <- c("x","y","plants_primary_extinction","pollinator_secondary_extinction")

# Removing all lines that primary extinction is equal to zero

df_extinctions_group1 <- subset(df_extinctions_group1, plants_primary_extinction > 0)

# Evaluating overdispersion

poisson_model <- glm(pollinator_secondary_extinction ~ plants_primary_extinction, 
                     family = poisson, 
                     data = df_extinctions_group1)

# Índice de superdispersão (deve ser próximo de 1 no Poisson)

dispersion <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
print(dispersion)

# Generalized Additive Models

gam_model1 <- bam(
  pollinator_secondary_extinction ~ s(plants_primary_extinction, k = 40) + s(x, y, k = 2000),
  family = poisson(),  # ou poisson
  data = df_extinctions_group1,
  method = "fREML",
  discrete = TRUE
)

summary(gam_model1)

gam.check(gam_model1)

# predict 

df_extinctions_group1$predito <- predict(gam_model1, type = "response")

# residuals 

df_extinctions_group1$residuos <- residuals(gam_model1, type = "deviance")

# Maps (Supplementary material) -------------------------------------------

S9 <- ggplot(df_extinctions_group1, aes(x = x, y = y, fill = predito)) +
  geom_tile() +
  scale_fill_viridis_c(name = "\nPredicted") +
  coord_equal() +
  ggtitle("Pollinators' secondary extinction ~") +
  theme_nothing() +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = expression(bold("i")), 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 16))


S10 <- ggplot(df_extinctions_group1, aes(x = x, y = y, fill = residuos)) +
  geom_tile() +
  scale_fill_viridis_c(name = "\nResiduals") +
  coord_equal() +
  ggtitle("Plants' primary extinction") +
  theme_nothing() +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = expression(bold("j")), 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 16))

# Pollinators' primary extinction versus plants' secondary extinct --------

pollinator_primary_extinction <- rast("./Results/rasters/primary_extinction_pollinators.tif")
plants_secondary_extinction <- rast("./Results/rasters/secondary_extinction_plants.tif")

# Converting in dataframe

pollinator_primary_extinction <- as.data.frame(pollinator_primary_extinction, xy = TRUE)
colnames(pollinator_primary_extinction) <- c("x","y","pollinator_primary_extinction")
plants_secondary_extinction <- as.data.frame(plants_secondary_extinction, xy = TRUE)
colnames(plants_secondary_extinction) <- c("x","y","plants_secondary_extinction")

# Merging dataframes

df_extinctions_group2 <- cbind(pollinator_primary_extinction, plants_secondary_extinction$plants_secondary_extinction)
colnames(df_extinctions_group2) <- c("x","y","pollinator_primary_extinction","plants_secondary_extinction")

# Removing all lines that primary extinction is equal to zero

df_extinctions_group2 <- subset(df_extinctions_group2, pollinator_primary_extinction > 0)

# Evaluating overdispersion

poisson_model <- glm(plants_secondary_extinction ~ pollinator_primary_extinction, 
                     family = poisson, 
                     data = df_extinctions_group2)

# Índice de superdispersão (deve ser próximo de 1 no Poisson)

dispersion <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
print(dispersion)

# Generalized Additive Models

gam_model2 <- bam(
  plants_secondary_extinction ~ s(pollinator_primary_extinction, k = 20) + s(x, y, k = 1000),
  family = nb(),  # ou poisson
  data = df_extinctions_group2,
  method = "fREML",
  discrete = TRUE
)

summary(gam_model2)

gam.check(gam_model2)

# predict 

df_extinctions_group2$predito <- predict(gam_model2, type = "response")

# residuals 

df_extinctions_group2$residuos <- residuals(gam_model2, type = "deviance")

# Maps (Supplementary material) -------------------------------------------

S11 <- ggplot(df_extinctions_group2, aes(x = x, y = y, fill = predito)) +
  geom_tile() +
  scale_fill_viridis_c(name = "\nPredicted") +
  coord_equal() +
  ggtitle("Plnts' secondary extinction ~") +
  theme_nothing() +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = expression(bold("k")), 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 16))


S12 <- ggplot(df_extinctions_group2, aes(x = x, y = y, fill = residuos)) +
  geom_tile() +
  scale_fill_viridis_c(name = "\nResiduals") +
  coord_equal() +
  ggtitle("Pollinators' primary extinction") +
  theme_nothing() +
  annotate("text", 
           x = -82, 
           y = 12, 
           label = expression(bold("l")), 
           color = "black", size = 7) +
  theme(legend.position = c(0.85, 0.25), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 16))

# Saving Supplementary Material (predict, residuals) ----------------------

Suppl_MR <- grid.arrange(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, ncol = 4)

ggsave(plot = Suppl_MR, 
       filename = "./Results/SM_results.tif",
       dpi = 300,
       width = 15.5,
       height = 13.5)




