ee$Initialize(project = "jonas-trepel")
drive_auth(email = "jonas.trepel@gmail.com")
ee$String('Hello from the Earth Engine servers!')$getInfo()
source("R/functions/monitor_gee_task.R")




years <- c(2000, 2018)


img_cop_2000 <- ee$
  ImageCollection("COPERNICUS/CORINE/V20/100m")$
  select('landcover')$
  filterDate("1999-01-01", "2001-12-31")$
  first()


Map$addLayer(img_cop_2000)

export_task_2000 <- ee_image_to_drive(image = img_cop_2000,
                                 #region = aoi,
                                 folder = "rgee_backup_corine_2000",
                                 description = "corine_2000",
                                 scale = 100, 
                                 timePrefix = FALSE, 
                                 maxPixels = 1e13
)
export_task_2000$start()


img_cop_2018 <- ee$
  ImageCollection("COPERNICUS/CORINE/V20/100m")$
  select('landcover')$
  filterDate("2017-01-01", "2019-12-31")$
  first()


Map$addLayer(img_cop_2018)


export_task_2018 <- ee_image_to_drive(image = img_cop_2018,
                                      #region = aoi,
                                      folder = "rgee_backup_corine_2018",
                                      description = "corine_2018",
                                      scale = 100, 
                                      timePrefix = FALSE, 
                                      maxPixels = 1e13
)
export_task_2018$start()

Sys.sleep(120)
monitor_gee_task(pattern = "corine_2000", path = "rgee_backup_corine_2000",
                 last_sleep_time = 10)

monitor_gee_task(pattern = "corine_2018", path = "rgee_backup_corine_2018",
                 last_sleep_time = 300)


#### Download and merge corine 2000 


(drive_files_2000 <- drive_ls(path = "rgee_backup_corine_2000",
                         pattern = "corine_2000") %>%
    dplyr::select(name) %>% 
    unique())


for(filename in unique(drive_files_2000$name)){
  
  path_name = paste0("data/spatial_data/corine_landcover/tmp_tiles/", filename)
  drive_download(file = filename, path = path_name, overwrite = TRUE)
}


tmp_tiles_2000 <- list.files("data/spatial_data/corine_landcover/tmp_tiles",
                                  full.names = T, pattern = "corine_2000")

corine_2000_r_list <- lapply(tmp_tiles_2000, rast)

data_type_corine_2000 <- terra::datatype(corine_2000_r_list[[1]])

r_corine_2000 <- merge(sprc(corine_2000_r_list),
                         filename = "data/spatial_data/corine_landcover/corine_landcover_2000_100m.tif",
                         overwrite = TRUE,
                         datatype = data_type_corine_2000)
plot(r_corine_2000)

file.remove(tmp_tiles_2000)
googledrive::drive_rm(unique(drive_files_2000$name))
googledrive::drive_rm("rgee_backup_corine_2000")


#Download and merge corine 2018

(drive_files_2018 <- drive_ls(path = "rgee_backup_corine_2018",
                              pattern = "corine_2018") %>%
    dplyr::select(name) %>% 
    unique())


for(filename in unique(drive_files_2018$name)){
  
  path_name = paste0("data/spatial_data/corine_landcover/tmp_tiles/", filename)
  drive_download(file = filename, path = path_name, overwrite = TRUE)
}


tmp_tiles_2018 <- list.files("data/spatial_data/corine_landcover/tmp_tiles",
                             full.names = T, pattern = "corine_2018")

corine_2018_r_list <- lapply(tmp_tiles_2018, rast)

data_type_corine_2018 <- terra::datatype(corine_2018_r_list[[1]])

r_corine_2018 <- merge(sprc(corine_2018_r_list),
                       filename = "data/spatial_data/corine_landcover/corine_landcover_2018_100m.tif",
                       overwrite = TRUE,
                       datatype = data_type_corine_2018)
plot(r_corine_2018)

file.remove(tmp_tiles_2018)
googledrive::drive_rm(unique(drive_files_2018$name))
googledrive::drive_rm("rgee_backup_corine_2018")
