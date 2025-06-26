#### Prepare global change drivers 
library(terra)
library(tidyverse)
library(remotePARTS)

template_r <- rast("data/spatial_data/martina_layers/lu_change_percent_2km.tif")
plot(template_r)

# 1. Nitrogen Deposition -------------------------------------------
n_depo_files <- list.files("data/spatial_data/time_series_raster/n_deposition",
                           pattern = ".tif", 
                           full.names = T) 


r_n_stack <- rast(n_depo_files)
r_mean_n <- mean(r_n_stack)
plot(r_mean_n)
hist(values(r_mean_n))

#clamp
q_01_n <- quantile(values(r_mean_n), 0.01)
q_99_n <- quantile(values(r_mean_n), 0.99)

r_mean_n_clamped <- clamp(r_mean_n, 
                          lower = q_01_n, 
                          upper = q_99_n)

anyNA(values(r_mean_n_clamped))
plot(r_mean_n_clamped)
hist(values(r_mean_n_clamped))

#log 
r_mean_n_log <- log(r_mean_n)
plot(r_mean_n_log)
hist(values(r_mean_n_log))


#trend 
dt_n_depo <- NULL
for(file in unique(n_depo_files)){
  
  r_tmp <- rast(file)
  
  dt_r_tmp <- as.data.frame(r_tmp, xy = T)
  
  col_name <- gsub("data/spatial_data/time_series_raster/n_deposition/", "", file)
  col_name <- gsub(".tif", "", col_name)
  
  
  colnames(dt_r_tmp) <- c("x", "y", col_name)
  
  if(is.null(dt_n_depo)){
    dt_n_depo <- dt_r_tmp
  }else{
    dt_n_depo <- left_join(dt_n_depo, dt_r_tmp)
    
  }

  print(paste0(file, " done"))
  
}

n_depo_cols <- grep("n_depo", names(dt_n_depo), value = TRUE)
y_n_depo <- as.matrix(dt_n_depo[, n_depo_cols])

coords_n_depo <- as.matrix(dt_n_depo[, c("x", "y")])


ar_fit_n_depo <- fitAR_map(Y = y_n_depo, coords = coords_n_depo)
dt_n_depo$ar_coef <- coefficients(ar_fit_n_depo)[, "t"]
dt_n_depo$ar_p_value <- ar_fit_n_depo$pvals[, 2]

dt_n_depo %>% 
  ggplot(aes(x = x, y = y, col = ar_coef)) + 
  geom_point(size = .1) + 
  scale_color_gradient2(high = "red", low = "blue", 
                        mid = "grey90", midpoint = 0) + 
  guides(fill = "none") + 
  labs(y = "Latitude", x = "Longitude", col = expression(beta[1]))


r_n_depo_trend <- setValues(r_mean_n, dt_n_depo$ar_coef)
plot(r_n_depo_trend)

r_n_depo_trend_p <- setValues(r_mean_n, dt_n_depo$ar_p_value)
plot(r_n_depo_trend_p)

### Resample 

r_mean_n_proj <- project(r_mean_n, template_r)

r_mean_n_2k <- resample(x = r_mean_n_proj, y = template_r, 
                        filename = "data/spatial_data/global_change_layers/mean_total_n_deposition_2k.tif", 
                        overwrite = T)
plot(r_mean_n_2k)

r_mean_n_clamped_proj <- project(r_mean_n_clamped, template_r)
r_mean_n_clamped_2k <- resample(x = r_mean_n_clamped_proj, y = template_r, 
                        filename = "data/spatial_data/global_change_layers/mean_total_n_deposition_clamped_2k.tif", 
                        overwrite = T)
plot(r_mean_n_clamped_2k)

r_mean_n_log_proj <- project(r_mean_n_log, template_r)
r_mean_n_log_2k <- resample(x = r_mean_n_log_proj, y = template_r, 
                                filename = "data/spatial_data/global_change_layers/mean_total_n_deposition_log_2k.tif", 
                            overwrite = T)
plot(r_mean_n_log_2k)


r_n_depo_trend_proj <- project(r_n_depo_trend, template_r)
r_n_depo_trend_2k <- resample(x = r_n_depo_trend_proj, y = template_r, 
                            filename = "data/spatial_data/global_change_layers/trend_total_n_deposition_2k.tif", 
                            overwrite = T)
plot(r_n_depo_trend_2k)

r_n_depo_trend_p_proj <- project(r_n_depo_trend_p, template_r)
r_n_depo_trend_p_2k <- resample(x = r_n_depo_trend_p_proj, y = template_r, 
                              filename = "data/spatial_data/global_change_layers/trend_p_val_total_n_deposition_2k.tif", 
                              overwrite = T)
plot(r_n_depo_trend_p_2k)


## 2. MAT ----------------------------------------------
mat_files <- list.files("data/spatial_data/time_series_raster/era5_temperature",
                           pattern = ".tif", 
                           full.names = T) 

#trend 
dt_mat <- NULL
for(file in unique(mat_files)){
  
  r_tmp <- rast(file)
  
  dt_r_tmp <- as.data.frame(r_tmp, xy = T)
  
  col_name <- gsub("data/spatial_data/time_series_raster/era5_temperature/", "", file)
  col_name <- gsub(".tif", "", col_name)
  
  
  colnames(dt_r_tmp) <- c("x", "y", col_name)
  
  if(is.null(dt_mat)){
    dt_mat <- dt_r_tmp
  }else{
    dt_mat <- left_join(dt_mat, dt_r_tmp)
    
  }
  
  print(paste0(file, " done"))
  
}

mat_cols <- grep("mat", names(dt_mat), value = TRUE)
y_mat <- as.matrix(dt_mat[, mat_cols])

coords_mat <- as.matrix(dt_mat[, c("x", "y")])

ar_fit_mat <- fitAR_map(Y = y_mat, coords = coords_mat)
dt_mat$ar_coef <- coefficients(ar_fit_mat)[, "t"]
dt_mat$ar_p_value <- ar_fit_mat$pvals[, 2]

dt_mat %>% 
  ggplot(aes(x = x, y = y, col = ar_coef)) + 
  geom_point(size = .1) + 
  scale_color_gradient2(high = "red", low = "blue", 
                        mid = "grey90", midpoint = 0) + 
  guides(fill = "none") + 
  labs(y = "Latitude", x = "Longitude", col = expression(beta[1]))


tmp_mat <- rast(mat_files[13])
plot(tmp_mat)

r_mat_trend <- rast(dt_mat[, c("x", "y", "ar_coef")], type = "xyz") 
crs(r_mat_trend) <- crs(tmp_mat)
plot(r_mat_trend)

writeRaster(r_mat_trend, 
            "data/spatial_data/global_change_layers/trend_mat.tif",
            overwrite= T)


r_mat_trend_p <- rast(dt_mat[, c("x", "y", "ar_p_value")], type = "xyz") 
crs(r_mat_trend_p) <- crs(tmp_mat)
plot(r_mat_trend_p)

writeRaster(r_mat_trend_p, 
            "data/spatial_data/global_change_layers/trend_p_val_mat.tif",
            overwrite= T)


#Resample
r_mat_trend_proj <- project(r_mat_trend, template_r)
r_mat_trend_2k <- resample(x = r_mat_trend_proj, y = template_r, 
                              filename = "data/spatial_data/global_change_layers/trend_mat_2k.tif", 
                              overwrite = T)
plot(r_mat_trend_2k)

r_mat_trend_p_proj <- project(r_mat_trend_p, template_r)
r_mat_trend_p_2k <- resample(x = r_mat_trend_p_proj, y = template_r, 
                                filename = "data/spatial_data/global_change_layers/trend_p_val_mat_2k.tif", 
                                overwrite = T)
plot(r_mat_trend_p_2k)


## 3. Precipitation ----------------------------------------------
prec_files <- list.files("data/spatial_data/time_series_raster/era5_precipitation",
                        pattern = ".tif", 
                        full.names = T) 

#trend 
dt_prec <- NULL
for(file in unique(prec_files)){
  
  r_tmp <- rast(file)
  
  dt_r_tmp <- as.data.frame(r_tmp, xy = T)
  
  col_name <- gsub("data/spatial_data/time_series_raster/era5_temperature/", "", file)
  col_name <- gsub(".tif", "", col_name)
  
  
  colnames(dt_r_tmp) <- c("x", "y", col_name)
  
  if(is.null(dt_prec)){
    dt_prec <- dt_r_tmp
  }else{
    dt_prec <- left_join(dt_prec, dt_r_tmp)
    
  }
  
  print(paste0(file, " done"))
  
}

prec_cols <- grep("prec", names(dt_prec), value = TRUE)
y_prec <- as.matrix(dt_prec[, prec_cols])

coords_prec <- as.matrix(dt_prec[, c("x", "y")])


ar_fit_prec <- fitAR_map(Y = y_prec, coords = coords_prec)
dt_prec$ar_coef <- coefficients(ar_fit_prec)[, "t"]
dt_prec$ar_p_value <- ar_fit_prec$pvals[, 2]

dt_prec %>% 
  ggplot(aes(x = x, y = y, col = ar_coef)) + 
  geom_point(size = .1) + 
  scale_color_gradient2(high = "red", low = "blue", 
                        mid = "grey90", midpoint = 0) + 
  guides(fill = "none") + 
  labs(y = "Latitude", x = "Longitude", col = expression(beta[1]))

tmp_prec <- rast(prec_files[13])
plot(tmp_prec)

r_prec_trend <- rast(dt_prec[, c("x", "y", "ar_coef")], type = "xyz") 
crs(r_prec_trend) <- crs(tmp_prec)
plot(r_prec_trend)

writeRaster(r_prec_trend, 
            "data/spatial_data/global_change_layers/trend_precipitation.tif",
            overwrite= T)


r_prec_trend_p <- rast(dt_prec[, c("x", "y", "ar_p_value")], type = "xyz") 
crs(r_prec_trend_p) <- crs(tmp_prec)
plot(r_prec_trend_p)

writeRaster(r_prec_trend_p, 
            "data/spatial_data/global_change_layers/trend_p_val_precipitation.tif",
            overwrite= T)

#Resample
r_prec_trend_proj <- project(r_prec_trend, template_r)
r_prec_trend_2k <- resample(x = r_prec_trend_proj, y = template_r, 
                           filename = "data/spatial_data/global_change_layers/trend_prec_2k.tif", 
                           overwrite = T)
plot(r_prec_trend_2k)

r_prec_trend_p_proj <- project(r_prec_trend_p, template_r)
r_prec_trend_p_2k <- resample(x = r_prec_trend_p_proj, y = template_r, 
                             filename = "data/spatial_data/global_change_layers/trend_p_val_prec_2k.tif", 
                             overwrite = T)
plot(r_prec_trend_p_2k)
