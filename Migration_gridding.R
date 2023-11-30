###############################################################################

library(geodata)
library(sf)
library(terra)
library(tmap)
library(geodata)
library(terra)
library(sf)


#Shapefiles of individual countries in the region
DRC <- geodata::gadm(country = 'Democratic Republic of the Congo', level = 0, path = tempdir())
Uganda <- geodata::gadm(country = 'Uganda', level = 0, path = tempdir())
Ethiopia <- geodata::gadm(country = 'Ethiopia', level = 0, path = tempdir())
South_Sudan <- geodata::gadm(country = 'SSD', level = 0, path = tempdir())
Tanzania <- geodata::gadm(country = 'Tanzania ', level = 0, path = tempdir())


region <- rbind(DRC, Uganda, Ethiopia, South_Sudan, Tanzania)

region <-st_as_sf(region)


# Create a grid covering the region
EA_region <- rast(ext = ext(region), res = 1.6)
EA_region <- terra::crop(EA_region, region)
values(EA_region) <- 1:ncell(EA_region)
EA_region <- terra::mask(EA_region, region)


# Define function to read and process geocoded data
read_and_process_geocode <- function(year) {
  file_path <- paste0("D:/OneDrive - CGIAR/SA_Team/korir/geo_", year, ".csv")
  origins_geocode <- read.csv(file_path)
  
  mig_points <- vect(origins_geocode, geom=c('lon', 'lat'), crs = crs(EA_region))
  
  # Remove unnecessary objects from memory
  rm(origins_geocode)
  
  mig_raster <- terra::rasterize(mig_points, EA_region, fun = 'count')
  
  # Remove unnecessary objects from memory
  rm(mig_points)
  
  return(mig_raster)
}

# Read and process geocoded data for each year
years <- 2018:2022
mig_rasters <- lapply(years, read_and_process_geocode)

# Create a list of raster names
mig_raster_names <- paste("mig_rasters_", years, sep = "")

# Combine the raster objects into a single list
mig_rasters <- setNames(mig_rasters, mig_raster_names)

# Optionally, you can assign the individual raster objects to the global environment
list2env(mig_rasters, envir = .GlobalEnv)





#Conflict data

# Define a function to read and filter conflict data
read_and_filter_conflict <- function(country_code) {
  file_path <- sprintf("Z:/1.Data/Palmira/CSO/data/%s/conflict/%s_conflict.csv", country_code, country_code)
  confl_data <- read.csv(file_path)
  confl_data <- confl_data[confl_data$YEAR >= 2018,]
  return(confl_data)
}

# List of country codes
country_codes <- c("ETH", "SSD", "TZA", "UGA")

# Read and filter conflict data for each country
conflict_data_list <- lapply(country_codes, read_and_filter_conflict)

# Combine the data into a single dataframe
merged_conf <- bind_rows(conflict_data_list)

# Group by YEAR and nest the data
conf_grouped <- merged_conf %>% group_by(YEAR) %>% nest()

# Define a function to rasterize the data
rasterize_data <- function(data) {
  vect_data <- vect(data, geom = c('LONGITUDE', 'LATITUDE'), crs = crs(EA_region))
  terra::rasterize(vect_data, EA_region, fun = 'count')
}

# Apply the rasterization function to each nested dataset
conf_rasters <- map(conf_grouped$data, rasterize_data)
names(conf_rasters) <- paste0("conf_raster_", years)

#Climate data
ETH_TR <- subset(rast("Z:/1.Data/Palmira/IOM/ETH/AT.tif"), c( "2018" ,"2019"))
SSD_TR <- subset(rast("Z:/1.Data/Palmira/IOM/SSD/AT.tif"), c( "2018" ,"2019"))
UGA_TR <- subset(rast("Z:/1.Data/Palmira/IOM/UGA/AT.tif"), c( "2018" ,"2019"))
TZA_TR <- subset(rast("Z:/1.Data/Palmira/IOM/TZA/AT.tif"), c( "2018" ,"2019"))
COD_TR <- subset(rast("Z:/1.Data/Palmira/IOM/COD/AT.tif"), c( "2018" ,"2019"))

TR <- terra::merge(ETH_TR, SSD_TR, UGA_TR, TZA_TR, COD_TR)
rm(ETH_TR, SSD_TR, UGA_TR, TZA_TR, COD_TR)

ETH_AT <- subset(rast("Z:/1.Data/Palmira/IOM/ETH/AT.tif"), c( "2018" ,"2019"))
SSD_AT <- subset(rast("Z:/1.Data/Palmira/IOM/SSD/AT.tif"), c( "2018" ,"2019"))
UGA_AT <- subset(rast("Z:/1.Data/Palmira/IOM/UGA/AT.tif"), c( "2018" ,"2019"))
TZA_AT <- subset(rast("Z:/1.Data/Palmira/IOM/TZA/AT.tif"), c( "2018" ,"2019"))
COD_AT <- subset(rast("Z:/1.Data/Palmira/IOM/COD/AT.tif"), c( "2018" ,"2019"))

AT <- terra::merge(ETH_AT, SSD_AT, UGA_AT, TZA_AT, COD_AT)
rm(ETH_AT, SSD_AT, UGA_AT, TZA_AT, COD_AT)

AT <- terra::resample(AT, EA_region, method = 'bilinear')
TR <- terra::resample(TR, EA_region, method = 'bilinear')


#Correlation between conflict and Migration

values_2018 <- data.frame(values(mig_rasters_2018), values(conf_rasters[["conf_raster_2018"]]), values(TR$`2018`), values(AT$`2018`))
values_2019 <- data.frame(values(mig_rasters_2019), values(conf_rasters[["conf_raster_2019"]]), values(TR$`2018`), values(AT$`2018`))
#values_2020 <- data.frame(values(mig_rasters_2020), values(conf_rasters[["conf_raster_2020"]]))
#values_2021 <- data.frame(values(mig_rasters_2021), values(conf_rasters[["conf_raster_2021"]]))
#values_2022 <- data.frame(values(mig_rasters_2022), values(conf_rasters[["conf_raster_2022"]]))

values <- rbind(values_2018, values_2019)
rm(conf_rasters)
rm(mig_rasters_2018, mig_rasters_2019, mig_rasters_2020, mig_rasters_2021, mig_rasters_2022)

names(values) <- c('Migrants', 'Conflicts', 'Rainfall', 'Temperature')
values <- na.omit(values)
#Pearson correlation
# correlation for all variables
round(cor(values),
      digits = 2 # rounded to 2 decimals
)

pairs(values[, c('Migrants', 'Conflicts', 'Rainfall', 'Temperature')])

# improved correlation matrix
library(corrplot)

corrplot(cor(values),
         method = "number",
         type = "upper" # show only upper side
)
# Linear regression
lm_model <- lm(Migrants ~ Conflicts+Rainfall+Temperature, data = values)

library(visreg)
visreg(lm_model, "Temperature", gg = T) 
r_squared <- summary(lm_model)$r.squared
rmse <- sqrt(mean(lm_model$residuals^2))




#loading Climate data
