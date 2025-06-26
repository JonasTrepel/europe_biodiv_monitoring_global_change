# get combined change layer 

library(terra)
library(tidyverse)

# 1. Load ----------------
r_n_depo <- rast("data/spatial_data/global_change_layers/mean_total_n_deposition_2k.tif")
#r_n_depo <- abs(r_n_depo)
plot(r_n_depo)

r_lc <- rast("data/spatial_data/martina_layers/lu_change_percent_2km.tif")
plot(r_lc)

r_mat <- rast("data/spatial_data/global_change_layers/trend_mat_2k.tif")
r_mat <- abs(r_mat)
plot(r_mat)

r_prec <- rast("data/spatial_data/global_change_layers/trend_prec_2k.tif")
r_prec <- abs(r_prec)
plot(r_prec)


# 2. Mask layers ----------------------------

r_n_depo_masked <- mask(r_n_depo, r_lc)
plot(r_n_depo_masked)

r_mat_masked <- mask(r_mat, r_lc)
plot(r_mat_masked)

r_prec_masked <- mask(r_prec, r_lc)
plot(r_prec_masked)

# 3. Rescale between 1 and 0

r_n_depo_rescaled <- setValues(r_n_depo_masked, scales::rescale(values(r_n_depo_masked)))
plot(r_n_depo_rescaled)

r_lc_rescaled <- setValues(r_lc, scales::rescale(values(r_lc)))
plot(r_lc_rescaled)

r_mat_rescaled <- setValues(r_mat_masked, scales::rescale(values(r_mat_masked)))
plot(r_mat_rescaled)

r_prec_rescaled <- setValues(r_prec_masked, scales::rescale(values(r_prec_masked)))
plot(r_prec_rescaled)


#4. average -------------

r_stack <- c(r_n_depo_rescaled, r_lc_rescaled, r_mat_rescaled, r_prec_rescaled)

# r_gc <- mean(r_n_depo_rescaled, 
#              r_lc_rescaled, 
#              r_mat_rescaled)
# plot(r_gc)

r_gc <- mean(r_stack)
plot(r_gc)
hist(values(r_gc))
writeRaster(r_gc, "data/spatial_data/global_change_layers/combined_global_change_pressure_2k.tif")

#5. Plot ---

dt_r_gc <- as.data.frame(r_gc, xy = T)
hist(dt_r_gc$mean)
p_main <- ggplot() +
  geom_tile(data = dt_r_gc, aes(x = x, y = y, fill = mean)) +
  scale_fill_viridis_c() +
  labs(fill = "Global\nChange\nPressure") +
  theme_void() +
  theme(legend.position = c(0.8, 0.7))
p_main

ggsave(plot = p_main, "builds/plots/global_change_pressure_map.png", dpi = 600)
