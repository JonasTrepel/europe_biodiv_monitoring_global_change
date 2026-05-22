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
