##### Load all rasters ##### 

library(rgee)
library(data.table)
library(tidyverse)
library(googledrive)
library(terra)

#  
rgee_env_dir <- c("C:\\Users\\au713983\\.conda\\envs\\rgee_env")
reticulate::use_python(rgee_env_dir, required=T)
ee_clean_user_credentials()
ee$Authenticate(auth_mode='notebook')
ee$Initialize(project = "ee-jonastrepel")
drive_auth(email = "jonas.trepel@bio.au.dk")
ee$String('Hello from the Earth Engine servers!')$getInfo()


monitor_gee_task <- function(pattern = NA, path = "rgee_backup", last_sleep_time = 600, 
                                       mail = "jonas.trepel@bio.au.dk") {
  
  drive_auth(email = mail)
  
  for (i in 1:10000) {
    drive_files <- drive_ls(path = path, pattern = pattern) %>%
      dplyr::select(name)
    
    # Check if the folder is empty
    if (n_distinct(drive_files) == 0) {
      Sys.sleep(10)
      print(paste0("Attempt ", i, ": Drive still empty"))
    } else {
      print("Files found:")
      print(drive_files)
      
      if (n_distinct(drive_files) < 8) {
        Sys.sleep(10) # to make sure all tiles are there
        drive_files <- drive_ls(path = path, pattern = pattern) %>%
          dplyr::select(name)
      }
      # check again
      if (n_distinct(drive_files) < 8) {
        Sys.sleep(last_sleep_time) # to make sure all tiles are there
      }
      drive_files <- drive_ls(path = path, pattern = pattern) %>% dplyr::select(name)
      print(drive_files)
      
      break #
    }
  }
}


years <- c(1950:2024)


for(year in years){
  
  print(paste0("Starting with: ", year))
  
  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year, "-12-31")
  
  annual_img <- ee$
    ImageCollection('ECMWF/ERA5_LAND/MONTHLY_AGGR')$
    select('temperature_2m')$
    filterDate(start_date, end_date)$
    mean()$subtract(273.15)
  
  Map$addLayer(annual_img)
  
  aoi <- ee$Geometry$Rectangle(
    coords = c(-30, 30, 90, 82),
    #coords = c(20.7, 41.3, 21, 41.5), 
    proj = "EPSG:4326",
    geodesic = FALSE
  )
  
  Map$addLayer(aoi)
  
  export_task <- ee_image_to_drive(image = annual_img,
                                   region = aoi,
                                   folder = "rgee_backup_mat",
                                   description = "annual_temp",
                                   scale = 11132, 
                                   timePrefix = FALSE, 
                                   maxPixels = 1e13
  )
  export_task$start()
  
  Sys.sleep(30)
  monitor_gee_task(pattern = "annual_temp", path = "rgee_backup_mat",
                   last_sleep_time = 10)
  
  (drive_files <- drive_ls(path = "rgee_backup_mat",
                                         pattern = "annual_temp") %>%
      dplyr::select(name) %>% 
      unique())
  
  
  path_name <- paste0("data/spatial_data/time_series_raster/era5_temperature/mat_", year, ".tif")
  

  drive_download(file = drive_files$name, path = path_name, overwrite = TRUE)
  
  
  googledrive::drive_rm(unique(drive_files$name))
  googledrive::drive_empty_trash()

  r <- rast(path_name)
  plot(r, main = paste0(year))
  
  print(paste0(year, " done"))
  
}