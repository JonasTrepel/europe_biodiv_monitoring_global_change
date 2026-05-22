library(terra)
library(tidyverse)


r_2000 <- rast("data/spatial_data/corine_landcover/corine_landcover_2000_100m.tif")
r_2018 <- rast("data/spatial_data/corine_landcover/corine_landcover_2018_100m.tif")

r_stack <- c(r_2000, r_2018)

#group land use categories 

intensive <- c(
  # Artificial surfaces
  111,  # Continuous urban fabric
  112,  # Discontinuous urban fabric
  121,  # Industrial or commercial units
  122,  # Road and rail networks and associated land
  123,  # Port areas
  124,  # Airports
  131,  # Mineral extraction sites
  132,  # Dump sites
  133,  # Construction sites
  141,  # Green urban areas
  142,  # Sport and leisure facilities
  
  # Agricultural areas
  211,  # Non-irrigated arable land
  212,  # Permanently irrigated land
  213,  # Rice fields
  221,  # Vineyards
  222,  # Fruit trees and berry plantations
  223,  # Olive groves
  231,  # Pastures
  241,  # Annual crops associated with permanent crops
  242,  # Complex cultivation patterns
  243,  # Agriculture with natural vegetation
  244   # Agro-forestry areas
)

# Non-intensive / natural land use classes
non_intensive <- c(
  # Forests
  311,  # Broad-leaved forest
  312,  # Coniferous forest
  313,  # Mixed forest
  # Semi-natural vegetation
  321,  # Natural grasslands
  322,  # Moors and heathland
  323,  # Sclerophyllous vegetation
  324,  # Transitional woodland-shrub
  # Open natural areas
  331,  # Beaches, dunes, sands
  332,  # Bare rocks
  333,  # Sparsely vegetated areas
  334,  # Burnt areas
  335,  # Glaciers and perpetual snow
  # Wetlands
  411,  # Inland marshes
  412,  # Peat bogs
  421,  # Salt marshes
  422,  # Salines
  423,  # Intertidal flats
  # Water bodies
  511,  # Water courses
  512,  # Water bodies
  521,  # Coastal lagoons
  522,  # Estuaries
  523   # Sea and ocean
)


#builds reclassification table 
dt_reclass <- rbind(
  data.frame(
    is = intensive,
    becomes   = 1
  ),
  data.frame(
    is = non_intensive,
    becomes   = 0
  )
  # maybe also add
  # data.frame(
  #   is = semi_intensive,
  #   becomes   = 0.5
  # )
)

#reclassify raster

r_class <- classify(r_stack, rcl = dt_reclass)
r_class[r_class == 999] <- NA
names(r_class) <- c("intensive_2000", "intensive_2018")
plot(r_class)

#get template in here 
template_r <- rast("data/spatial_data/martina_layers/lu_change_percent_2km.tif")
plot(template_r)


r_class_proj <- project(r_class, template_r)
r_class_proj[[1]]
r_intensive_2000_2k <- resample(x = r_class_proj[[1]], y = template_r, 
                                method = "mean", 
                                filename = "data/spatial_data/global_change_layers/intensive_landuse_2000_2k.tif",
                                overwrite = T)
plot(r_intensive_2000_2k)


r_intensive_2018_2k <- resample(x = r_class_proj[[2]], y = template_r, 
                                method = "mean", 
                                filename = "data/spatial_data/global_change_layers/intensive_landuse_2018_2k.tif", 
                                overwrite = T)
plot(r_intensive_2018_2k)

r_land_use_change = r_intensive_2018_2k - r_intensive_2000_2k
names(r_land_use_change) <- "lu_change_percent_2km"
plot(r_land_use_change)
#r_land_use_change <- mask(r_land_use_change, template_r)

writeRaster(r_land_use_change,
            filename = "data/spatial_data/global_change_layers/corine_landuse_change_2000_2018_2k.tif", 
            overwrite = T)

r_land_use_intensification = ifel(r_land_use_change < 0 , 0, r_land_use_change)
plot(r_land_use_intensification)
writeRaster(r_land_use_intensification,
            filename = "data/spatial_data/global_change_layers/corine_landuse_intensification_2000_2018_2k.tif", 
            overwrite = T)


r_mean_landuse_intensity_2k <-  mean(c(r_intensive_2018_2k, r_intensive_2000_2k), na.rm = T)
names(r_mean_landuse_intensity_2k) <- "lu_mean_percent_2km"
plot(r_mean_landuse_intensity_2k)

writeRaster(r_mean_landuse_intensity_2k,
            filename = "data/spatial_data/global_change_layers/corine_landuse_mean_2000_2018_2k.tif", 
            overwrite = T)


dt_change <- as.data.frame(r_land_use_change, xy = TRUE, na.rm = TRUE)

# Symmetric limits around zero
max_abs <- max(abs(dt_change$lu_change_percent_2km))

# Plot
ggplot(dt_change) +
  geom_raster(aes(x = x,y = y, fill = lu_change_percent_2km)) +
  scale_fill_gradient2(
    low = "navy",
    mid = "white",
    high = "darkred",
    midpoint  = 0,
    limits = c(-max_abs, max_abs),
    name = "Change") +
  coord_equal() +
  theme_void() +
  labs(title = "Change in intensive land use (2000–2018)" )

