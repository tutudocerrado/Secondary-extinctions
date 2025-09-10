
# Loading rasters to produce maps of results ------------------------------

primary_extinction <- rast("./Results/rasters/primary_extinction.tif")
secondary_extinction <- rast("./Results/rasters/secondary_extinction.tif")
turnover <- rast("./Results/rasters/dissimilarity_sorensen.tif")
colonization <- rast("./Results/rasters/colonization.tif")

# Converting rasters into dataframe ---------------------------------------

primary_extinction <- as.data.frame(primary_extinction, xy = TRUE)
primary_extinction <- primary_extinction |> 
  mutate(sum = ifelse(sum == 0, NA, sum))

secondary_extinction <- as.data.frame(secondary_extinction, xy = TRUE)
secondary_extinction <- secondary_extinction |> 
  mutate(sum = ifelse(sum == 0, NA, sum))

turnover <- as.data.frame(turnover, xy = TRUE)

colonization <- as.data.frame(colonization, xy = TRUE)
colonization <- colonization |> 
  mutate(sum = ifelse(sum == 0, NA, sum))

# Maps --------------------------------------------------------------------

# Primary extinction map --------------------------------------------------

primary_extinction_map <- ggplot(data = primary_extinction, 
                                 aes(x = x, 
                                 y = y, 
                                 fill = sum))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  scale_fill_viridis_c(name = "Primary\nextinction",
                       breaks = c(min(primary_extinction$sum, na.rm = TRUE), 
                                  max(primary_extinction$sum, na.rm = TRUE)),
                       na.value = "lightgray") +
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
        plot.title = element_text(hjust = 0.5, size = 18))

# Secondary extinction map ------------------------------------------------

secondary_extinction_map <- ggplot(data = secondary_extinction, 
                                 aes(x = x, 
                                     y = y, 
                                     fill = sum))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  scale_fill_viridis_c(name = "Secondary\nextinction",
                       breaks = c(min(secondary_extinction$sum, na.rm = TRUE), 
                                  max(secondary_extinction$sum, na.rm = TRUE)),
                       na.value = "lightgray") +
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
        plot.title = element_text(hjust = 0.5, size = 18))

# Turnover map ------------------------------------------------------------

turnover_map <- ggplot(data = turnover, 
                                   aes(x = x, 
                                       y = y, 
                                       fill = lyr.1))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  scale_fill_viridis_c(name = "Turnover",
                       breaks = c(min(turnover$lyr.1, na.rm = TRUE), 
                                  max(turnover$lyr.1, na.rm = TRUE))) +
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
        plot.title = element_text(hjust = 0.5, size = 18))

# Colonization map --------------------------------------------------------

colonization_map <- ggplot(data = colonization, 
                                   aes(x = x, 
                                       y = y, 
                                       fill = sum))+
  geom_tile() +
  theme_nothing() +
  coord_fixed() +
  scale_fill_viridis_c(name = "Colonization",
                       breaks = c(min(colonization$sum, na.rm = TRUE), 
                                  max(colonization$sum, na.rm = TRUE)),
                       na.value = "lightgray") +
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
        plot.title = element_text(hjust = 0.5, size = 18))

# Graphic: richness versus primary extinction -----------------------------

# All graphics were produced using data from macroecological relationships code

g1 <- ggplot(df_rich_prim_ext, aes(x = richness, y = primary_extinction)) +
  geom_bin2d(bins = 100) +
  scale_fill_viridis_c(trans = "log", 
                       name = "Density\n(log)", 
                       labels = comma) +
  stat_smooth(
    method = "gam",
    formula = y ~ s(x, k = 10),
    method.args = list(family = "nb"),
    color = "red",
    size = 1.2,
    se = FALSE
  ) +
  theme_classic() +
  annotate("text", 
           x = 15, 
           y = 1000, 
           label = expression(bold("e")), 
           color = "black", size = 7) +
  #ggtitle("b") +
  labs(
    x = "Species richness",
    y = "Primary extinction"
  ) +
  theme(
    text = element_text(color = "black", size = 14),
    axis.text = element_text(color = "black", size = 14))

# Graphic: richness versus secondary extinction ---------------------------

g2 <- ggplot(df_rich_sec_ext, aes(x = richness, y = secondary_extinction)) +
  geom_bin2d(bins = 100) +
  scale_fill_viridis_c(trans = "log", 
                       name = "Density\n(log)", 
                       labels = comma) +
  stat_smooth(
    method = "gam",
    formula = y ~ s(x, k = 10),
    method.args = list(family = "nb"),
    color = "red",
    size = 1.2,
    se = FALSE
  ) +
  theme_classic() +
  annotate("text",
           x = 15,
           y = 340,
           label = expression(bold("f")),
           color = "black", size = 7) +
  labs(
    x = "Species richness",
    y = "Secondary extinction"
  ) +
  theme(
    text = element_text(color = "black", size = 14),
    axis.text = element_text(color = "black", size = 14)
  )

# Graphic: richness versus colonization -----------------------------------

g3 <- ggplot(df_rich_col, aes(x = richness, y = colonization)) +
  geom_bin2d(bins = 100) +
  scale_fill_viridis_c(trans = "log", 
                       name = "Density\n(log)", 
                       labels = comma) +
  stat_smooth(
    method = "gam",
    formula = y ~ s(x, k = 10),
    method.args = list(family = "nb"),
    color = "red",
    size = 1.2,
    se = FALSE
  ) +
  theme_classic() +
  annotate("text",
           x = 15,
           y = 450,
           label = expression(bold("g")),
           color = "black", size = 7) +
  #ggtitle("b") +
  labs(
    x = "Species richness",
    y = "Colonization"
  ) +
  theme(
    text = element_text(color = "black", size = 14),
    axis.text = element_text(color = "black", size = 14)
  )

# Graphic: primary extinction versus secondary extinction -----------------

g4 <- ggplot(df_extinctions, aes(x = primary_extinction, y = secondary_extinction)) +
  geom_bin2d(bins = 100) +
  scale_fill_viridis_c( trans = "log", 
                        name = "Density\n(log)",
                        labels = comma) +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 10), 
              method.args = list(family = "nb"), 
              color = "red", size = 1.2, se = FALSE) +
  theme_classic() +
  annotate("text", 
           x = 15, 
           y = 390, 
           label = expression(bold("h")), 
           color = "black", size = 7) +
  labs(
    x = "Primary extinction",
    y = "Secondary extinction"
  ) +
  theme(text = element_text(color = "black", size = 14),
        axis.text = element_text(color = "black", size = 14))

# Saving figure 1 - Macroecological relationships -------------------------

gride1 <- grid.arrange(primary_extinction_map,
                       secondary_extinction_map, ncol = 2)

gride2 <- grid.arrange(colonization_map, 
                       turnover_map, ncol = 2)

figure1 <- grid.arrange(gride1, gride2, g1, g2, g3, g4, ncol = 2)

ggsave(plot = figure1, 
       filename = "./Results/Figure1.png",
       dpi = 300,
       width = 13.5,
       height = 13.5)


