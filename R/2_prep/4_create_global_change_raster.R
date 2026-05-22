# get combined change layer 

library(terra)
library(tidyverse)

# 1. Load ----------------

r_mask <- rast("data/spatial_data/martina_layers/lu_change_percent_2km.tif")
plot(r_mask)

r_n_depo <- rast("data/spatial_data/global_change_layers/mean_total_n_deposition_2k.tif")
#r_n_depo <- abs(r_n_depo)
plot(r_n_depo)

r_lc_change <- rast("data/spatial_data/global_change_layers/corine_landuse_change_2000_2018_2k.tif")
plot(r_lc_change)

r_lc_int <- rast("data/spatial_data/global_change_layers/corine_landuse_intensification_2000_2018_2k.tif")
plot(r_lc_int)

r_lc_mean <- rast("data/spatial_data/global_change_layers/corine_landuse_mean_2000_2018_2k.tif")
plot(r_lc_mean)

r_mat <- rast("data/spatial_data/global_change_layers/trend_mat_2k.tif")
plot(r_mat)
sum(values(r_mat > 0), na.rm = TRUE)/sum(values(!is.na(r_mat)))
sum(values(r_mat < 0), na.rm = TRUE)/sum(values(!is.na(r_mat)))


#all cells above 0

r_mat <- abs(r_mat)
plot(r_mat)

r_prec <- rast("data/spatial_data/global_change_layers/trend_prec_2k.tif")
plot(r_prec)
sum(values(r_prec > 0), na.rm = TRUE)/sum(values(!is.na(r_prec)))
sum(values(r_prec < 0), na.rm = TRUE)/sum(values(!is.na(r_prec)))
#22% decrease, 78 % increase
r_prec <- abs(r_prec)
plot(r_prec)


# 2. Mask layers ----------------------------

r_n_depo_masked <- mask(r_n_depo, r_mask)
plot(r_n_depo_masked)

r_mat_masked <- mask(r_mat, r_mask)
plot(r_mat_masked)

r_prec_masked <- mask(r_prec, r_mask)
plot(r_prec_masked)

r_lc_change_masked <- mask(r_lc_change, r_mask)
plot(r_lc_change_masked)

r_lc_int_masked <- mask(r_lc_int, r_mask)
plot(r_lc_int_masked)

r_lc_mean_masked <- mask(r_lc_mean, r_mask)
plot(r_lc_mean_masked)

# 3. Rescale between 1 and 0

r_n_depo_rescaled <- setValues(r_n_depo_masked, scales::rescale(values(r_n_depo_masked)))
plot(r_n_depo_rescaled)

r_lc_change_rescaled <- setValues(r_lc_change_masked, scales::rescale(values(r_lc_change_masked)))
plot(r_lc_change_rescaled)

r_lc_int_rescaled <- setValues(r_lc_int_masked, scales::rescale(values(r_lc_int_masked)))
plot(r_lc_int_rescaled)

r_lc_mean_rescaled <- setValues(r_lc_mean_masked, scales::rescale(values(r_lc_mean_masked)))
plot(r_lc_mean_rescaled)

r_mat_rescaled <- setValues(r_mat_masked, scales::rescale(values(r_mat_masked)))
plot(r_mat_rescaled)

r_prec_rescaled <- setValues(r_prec_masked, scales::rescale(values(r_prec_masked)))
plot(r_prec_rescaled)


#4. average -------------

r_gc_int <- mean(c(r_n_depo_rescaled, r_lc_int_rescaled, r_mat_rescaled, r_prec_rescaled))

plot(r_gc_int)
hist(values(r_gc_int))
writeRaster(r_gc_int, "data/spatial_data/global_change_layers/combined_global_change_pressure_using_lc_int_2k.tif", 
            overwrite = TRUE)

r_gc_change <- mean(c(r_n_depo_rescaled, r_lc_change_rescaled, r_mat_rescaled, r_prec_rescaled))

plot(r_gc_change)
hist(values(r_gc_change))
writeRaster(r_gc_change, "data/spatial_data/global_change_layers/combined_global_change_pressure_using_lc_change_2k.tif", 
            overwrite = TRUE)

r_gc_mean <- mean(c(r_n_depo_rescaled, r_lc_mean_rescaled, r_mat_rescaled, r_prec_rescaled))

plot(r_gc_mean)
hist(values(r_gc_mean))
writeRaster(r_gc_mean, "data/spatial_data/global_change_layers/combined_global_change_pressure_using_lc_mean_2k.tif", 
            overwrite = TRUE)


#5. Plots ---

dt_r_gc_int <- as.data.frame(r_gc_int, xy = T)
p_main_int <- ggplot() +
  geom_tile(data = dt_r_gc_int, aes(x = x, y = y, fill = mean)) +
  scico::scale_fill_scico(palette = "batlow") +
  labs(fill = "Global\nChange\nPressure", 
       title = "Using Land Use Intensification") +
  theme_void() +
  coord_fixed() +
  theme(legend.position = c(0.8, 0.7))
p_main_int

ggsave(plot = p_main_int, "builds/plots/global_change_pressure_map_int.png", dpi = 900)

dt_r_gc_change <- as.data.frame(r_gc_change, xy = T)
p_main_change <- ggplot() +
  geom_tile(data = dt_r_gc_change, aes(x = x, y = y, fill = mean)) +
  scico::scale_fill_scico(palette = "batlow") +
  labs(fill = "Global\nChange\nPressure", 
       title = "Using Land Use Change") +
  theme_void() +
  coord_fixed() +
  theme(legend.position = c(0.8, 0.7))
p_main_change

ggsave(plot = p_main_change, "builds/plots/global_change_pressure_map_change.png", dpi = 900)

dt_r_gc_mean <- as.data.frame(r_gc_mean, xy = T)
p_main_mean <- ggplot() +
  geom_tile(data = dt_r_gc_mean, aes(x = x, y = y, fill = mean)) +
  scico::scale_fill_scico(palette = "batlow") +
  labs(fill = "Global\nChange\nPressure", 
       title = "Using Land Use Mean Intensity") +
  theme_void() +
  coord_fixed() +
  theme(legend.position = c(0.8, 0.7))
p_main_mean

ggsave(plot = p_main_mean, "builds/plots/global_change_pressure_map_mean.png", dpi = 900)

library(patchwork)
(p_alt <- (p_main_int | (p_main_mean / p_main_change)) +
    plot_layout(widths = c(1.1, 0.9)) +
    theme(plot.margin = margin(5, 5, 5, 5)) +
    plot_annotation(tag_levels = "A"))

ggsave(plot = p_alt, "builds/plots/global_change_pressure_map_comb.png", dpi = 900, 
       height = 9, width = 11)


dt_r_n_depo <- as.data.frame(r_n_depo_rescaled, xy = T)
hist(dt_r_n_depo$mean)
p_n_depo <- ggplot() +
  geom_tile(data = dt_r_n_depo, aes(x = x, y = y, fill = mean)) +
  scale_fill_viridis_c() +
  labs(fill = "N\nDepo", title = "Nitrogen Deposition") +
  theme_void() +
  theme(legend.position = c(0.8, 0.7))
p_n_depo

dt_r_mat <- as.data.frame(r_mat_rescaled, xy = T)
hist(dt_r_mat$ar_coef)
p_mat <- ggplot() +
  geom_tile(data = dt_r_mat, aes(x = x, y = y, fill = ar_coef)) +
  scale_fill_viridis_c() +
  labs(fill = "Abs\nMAT\nChange", title = "MAT Change (Absolute)") +
  theme_void() +
  theme(legend.position = c(0.8, 0.7))
p_mat

dt_r_prec <- as.data.frame(r_prec_rescaled, xy = T)
hist(dt_r_prec$ar_coef)
p_prec <- ggplot() +
  geom_tile(data = dt_r_prec, aes(x = x, y = y, fill = ar_coef)) +
  scale_fill_viridis_c() +
  labs(fill = "Abs\nPrec.\nChange", title = "Precipitation Change (Absolute)") +
  theme_void() +
  theme(legend.position = c(0.8, 0.7))
p_prec


dt_r_lc_int <- as.data.frame(r_lc_int_rescaled, xy = T)
p_lc_int <- ggplot() +
  geom_tile(data = dt_r_lc_int, aes(x = x, y = y, fill = lu_change_percent_2km)) +
  scale_fill_viridis_c() +
  labs(fill = "LC\nIntensification", title = "Land Use Intensification") +
  theme_void() +
  theme(legend.position = c(0.8, 0.7))
p_lc_int

dt_r_lc_change <- as.data.frame(r_lc_change_rescaled, xy = T)
p_lc_change <- ggplot() +
  geom_tile(data = dt_r_lc_change, aes(x = x, y = y, fill = lu_change_percent_2km)) +
  scale_fill_viridis_c() +
  labs(fill = "LC\nChange", title = "Land Use Intensity Change") +
  theme_void() +
  theme(legend.position = c(0.8, 0.7))
p_lc_change

dt_r_lc_mean <- as.data.frame(r_lc_mean_rescaled, xy = T)
p_lc_mean <- ggplot() +
  geom_tile(data = dt_r_lc_mean, aes(x = x, y = y, fill = lu_mean_percent_2km)) +
  scale_fill_viridis_c() +
  labs(fill = "LC\nIntensity", title = "Mean Land Use Intensity") +
  theme_void() +
  theme(legend.position = c(0.8, 0.7))
p_lc_mean


library(patchwork)
p_dr <-  (p_mat | p_prec) / (p_n_depo |  p_lc_int) / (p_lc_mean |  p_lc_change)
p_dr
ggsave(plot = p_dr, "builds/plots/global_change_drivers.png", dpi = 900, height = 14, width = 10)
