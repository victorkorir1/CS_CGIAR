
#* TODO: Script to grid point data, perform pixelwise correlation and make plots

#* References: https://gist.github.com/hakimabdi/7308bbd6d9d94cf0a1b8
#* 
#* Author: Victor Korir
###############################################################################

library(geodata)
library(sf)
library(terra)
library(tmap)
library(geodata)
library(tidyverse)
library(raster)
#Shapefiles of individual countries in the region
DRC <- geodata::gadm(country = 'Democratic Republic of the Congo', level = 0, path = tempdir())
Uganda <- geodata::gadm(country = 'Uganda', level = 0, path = tempdir())
Ethiopia <- geodata::gadm(country = 'Ethiopia', level = 0, path = tempdir())
South_Sudan <- geodata::gadm(country = 'SSD', level = 0, path = tempdir())
Tanzania <- geodata::gadm(country = 'Tanzania ', level = 0, path = tempdir())

#merging the individual country boundaries
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
years <- 2018:2023
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
#, "SSD", "TZA", "UGA", 'COD'
country_codes <- c("ETH", "SSD", "TZA", "UGA", 'COD')

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

ETH_TR <- subset(rast("Z:/1.Data/Palmira/IOM/ETH/climatic_indexes/season_type_1/TR.tif"), c( "2018" ,"2019", '2020', '2021', '2022'))
SSD_TR <- subset(rast("Z:/1.Data/Palmira/IOM/SSD/climatic_indexes/season_type_1/TR.tif"), c( "2018" ,"2019", '2020', '2021', '2022'))
UGA_TR <- subset(rast("Z:/1.Data/Palmira/IOM/UGA/climatic_indexes/season_type_1/TR.tif"), c( "2018" ,"2019", '2020', '2021', '2022'))
TZA_TR <- subset(rast("Z:/1.Data/Palmira/IOM/TZA/climatic_indexes/season_type_1/TR.tif"), c( "2018" ,"2019", '2020', '2021', '2022'))
COD_TR <- subset(rast("Z:/1.Data/Palmira/IOM/COD/climatic_indexes/seanon_type_1/TR.tif"), c( "2018" ,"2019", '2020', '2021', '2022'))

TR <- terra::merge(ETH_TR, SSD_TR, UGA_TR, TZA_TR, COD_TR)
rm(ETH_TR, SSD_TR, UGA_TR, TZA_TR, COD_TR)


ETH_AT <- subset(rast('Z:/1.Data/Palmira/IOM/ETH/climatic_indexes/season_type_1/AT.tif'), c( "2018" ,"2019", '2020', '2021', '2022'))
SSD_AT <- subset(rast("Z:/1.Data/Palmira/IOM/SSD/climatic_indexes/season_type_1/AT.tif"), c( "2018" ,"2019", '2020', '2021', '2022'))
UGA_AT <- subset(rast("Z:/1.Data/Palmira/IOM/UGA/climatic_indexes/season_type_1/AT.tif"), c( "2018" ,"2019", '2020', '2021', '2022'))
TZA_AT <- subset(rast("Z:/1.Data/Palmira/IOM/TZA/climatic_indexes/season_type_1/AT.tif"), c( "2018" ,"2019", '2020', '2021', '2022'))
COD_AT <- subset(rast("Z:/1.Data/Palmira/IOM/COD/climatic_indexes/seanon_type_1/AT.tif"), c( "2018" ,"2019", '2020', '2021', '2022'))

AT <- terra::merge(ETH_AT, SSD_AT, UGA_AT, TZA_AT, COD_AT)
rm(ETH_AT, SSD_AT, UGA_AT, TZA_AT, COD_AT)

AT <- terra::resample(AT, EA_region, method = 'bilinear')
TR <- terra::resample(TR, EA_region, method = 'bilinear')

#Preparing data for correlation
conf_rasters <- rast(conf_rasters)
mig_rasters <- rast(mig_rasters)
mig_rasters1 <- brick(mig_rasters_2018, mig_rasters_2019, mig_rasters_2020, 
                 mig_rasters_2021, mig_rasters_2022, mig_rasters_2023)

AT_mig <- stack(as(AT, 'Raster'), as(migrasters1, 'Raster'))
TR_mig <- stack(as(TR, 'Raster'), as(migrasters1, 'Raster'))
conf_mig <- stack(as(conf_rasters, 'Raster'), as(migrasters1, 'Raster'))

#Pixelwise correlation fuction
gridcorts <- function(rasterstack, method, type=c("corel","pval","both")){
  # Values for (layers, ncell, ncol, nrow, method, crs, extent) come straight from the input raster stack
  # e.g. nlayers(rasterstack), ncell(rasterstack)... etc.
  print(paste("Start Gridcorts:",Sys.time()))
  print("Loading parameters")
  layers=nlayers(rasterstack);ncell=ncell(rasterstack);
  ncol=ncol(rasterstack);nrow=nrow(rasterstack);crs=crs(rasterstack);
  extent=extent(rasterstack);pb = txtProgressBar(min = 0, max = ncell, initial = 0)
  print("Done loading parameters")
  mtrx <- as.matrix(rasterstack,ncol=layers)
  empt <- matrix(nrow=ncell, ncol=2)
  print("Initiating loop operation")
  if (type == "corel"){
    for (i in 1:ncell){
      setTxtProgressBar(pb,i)
      if (all(is.na(mtrx[i,1:(layers/2)])) | all(is.na(mtrx[i,((layers/2)+1):layers]))){ 
        empt[i,1] <- NA 
      } else 
        if (sum(!is.na(mtrx[i,1:(layers/2)]/mtrx[i,((layers/2)+1):layers])) < 4 ){
          empt[i,1] <- NA 
        } else 
          empt[i,1] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$estimate)
    }
    print("Creating empty raster")
    corel <- raster(nrows=nrow,ncols=ncol,crs=crs)
    extent(corel) <- extent
    print("Populating correlation raster")
    values(corel) <- empt[,1]
    print(paste("Ending Gridcorts on",Sys.time()))
    corel
  } 
  else
    if (type == "pval"){
      for (i in 1:ncell){
        setTxtProgressBar(pb,i)
        if (all(is.na(mtrx[i,1:(layers/2)])) | all(is.na(mtrx[i,((layers/2)+1):layers]))){ 
          empt[i,2] <- NA 
        } else 
          if (sum(!is.na(mtrx[i,1:(layers/2)]/mtrx[i,((layers/2)+1):layers])) < 4 ){
            empt[i,2] <- NA 
          } else 
            empt[i,2] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$p.value)
      }
      pval <- raster(nrows=nrow,ncols=ncol,crs=crs)
      extent(pval) <- extent
      print("Populating significance raster")
      values(pval) <- empt[,2]
      print(paste("Ending Gridcorts on",Sys.time()))
      pval
    }
  else
    if (type == "both"){
      for (i in 1:ncell){
        setTxtProgressBar(pb,i)
        if (all(is.na(mtrx[i,1:(layers/2)])) | all(is.na(mtrx[i,((layers/2)+1):layers]))){ 
          empt[i,] <- NA 
        } else 
          if (sum(!is.na(mtrx[i,1:(layers/2)]/mtrx[i,((layers/2)+1):layers])) < 4 ){
            empt[i,] <- NA 
          } else {
            empt[i,1] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$estimate) 
            empt[i,2] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$p.value)
          }
      }
      c <- raster(nrows=nrow,ncols=ncol,crs=crs)
      p <- raster(nrows=nrow,ncols=ncol,crs=crs)
      print("Populating raster brick")
      values(c) <- empt[,1]
      values(p) <- empt[,2]
      brk <- brick(c,p)
      extent(brk) <- extent
      names(brk) <- c("Correlation","Pvalue")
      print(paste("Ending Gridcorts on",Sys.time()))
      brk
    }
}

#Correlations
mig_conf_corr <- gridcorts(conf_mig, method="pearson", type="both")
mig_AT_corr <- gridcorts(AT_mig, method="pearson", type="both")
mig_TR_corr <- gridcorts(TR_mig, method="pearson", type="both")



#Correlation between conflict and Migration
values_2018 <- data.frame(values(mig_rasters_2018), 
                          values(conf_rasters[["conf_raster_2018"]]), 
                          values(TR$`2018`), values(AT$`2018`))
values_2019 <- data.frame(values(mig_rasters_2019), 
                          values(conf_rasters[["conf_raster_2019"]]), 
                          values(TR$`2019`), values(AT$`2019`))
values_2020 <- data.frame(values(mig_rasters_2020), 
                          values(conf_rasters[["conf_raster_2020"]]), 
                          values(TR$`2020`), values(AT$`2020`))
values_2021 <- data.frame(values(mig_rasters_2021), 
                          values(conf_rasters[["conf_raster_2021"]]), 
                          values(TR$`2021`), values(AT$`2021`))
values_2022 <- data.frame(values(mig_rasters_2022), values(conf_rasters[["conf_raster_2022"]]), values(TR$`2022`), values(AT$`2022`))

names(values_2018) <- c('Migrants', 'Conflicts', 'Rainfall', 'Temperature')
names(values_2019) <- c('Migrants', 'Conflicts', 'Rainfall', 'Temperature')
names(values_2020) <- c('Migrants', 'Conflicts', 'Rainfall', 'Temperature')
names(values_2021) <- c('Migrants', 'Conflicts', 'Rainfall', 'Temperature')
names(values_2021) <- c('Migrants', 'Conflicts', 'Rainfall', 'Temperature')
values <- rbind(c(values_2018, values_2019, values_2020, values_2021, values_2022))
rm(conf_rasters)
rm(mig_rasters_2018, mig_rasters_2019, mig_rasters_2020, mig_rasters_2021, mig_rasters_2022)

names(values) <- c('Migrants', 'Conflicts', 'Rainfall', 'Temperature')
values <- na.omit(values)
values <- as.data.frame(values)
#Pearson correlation
# correlation for all variables
round(cor(as.numeric(values)),
      digits = 2 # rounded to 2 decimals
)

pair_plot <- pairs(values[, c('Migrants', 'Conflicts', 'Rainfall', 'Temperature')])

# improved correlation matrix
library(corrplot)
corrplot(cor(as.numeric(values)),
         method = "number",
         type = "upper" # show only upper side
)

###################################################
#plotting 
tm_shape(mig_TR_corr)+
  tm_raster(palette="RdYlGn", title = '', style="jenks", colorNA = 'grey', legend.show=T,legend.reverse = T)+
  tm_shape(region)+
  tm_borders()+
  tm_graticules(ticks = T, lines = F)+
  tm_compass(type="arrow", position=c('left', 'top'))+
  tm_facets( free.scales = TRUE) +
  tm_layout(legend.position= c("right", "bottom"),
            legend.outside = F,
            legend.width = 2, 
            legend.text.size = 0.6)


#plot the conf rasters and migration rasters(grided data)
migrasters1 <- stack(as(mig_rasters_2018, 'Raster'), 
                     as(mig_rasters_2019, 'Raster'),
                     as(mig_rasters_2020, 'Raster'),
                     as(mig_rasters_2021, 'Raster'),
                     as(mig_rasters_2022, 'Raster'),
                     as(mig_rasters_2023, 'Raster'))

names(mig_rasters) <- c("Migrants_2018", "Migrants_2019", "Migrants_2020", "Migrants_2021", "Migrants_2022", "Migrants_2023")
names(conf_rasters) <- c("Conflict_2018", "Conflict_2019", "Conflict_2020", "Conflict_2021", "Conflict_2022", "Conflict_2023")
mig_rasters <- mig_rasters/1000
migration_plot <- tm_shape(AT)+
  tm_raster(palette= 'YlOrBr',style = 'jenks', colorNA = 'white', title = 'Temperature(°C)')+
  tm_shape(region)+
  tm_borders(col = "black")+
  tm_compass( position=c('left', 'top'))+
  tm_graticules(ticks = T, lines = F)+
  tm_scale_bar(breaks = c(0, 250, 500), text.size = 0.5, 
               position = c("left", "bottom"))+
  tm_layout(legend.outside=T,
            legend.show = T,
            legend.outside.position = 'right',
            legend.outside.size = 0.15,
            #legend.title.size= 1,
            legend.frame=F,
            legend.just = c("left", "top"), 
            legend.position  = c("left", "bottom"),
            inner.margins = c(0.02, 0.02, 0.01, 0.02)
  )

out <- "C:/Users/vkorir/Documents/CS_CGIAR/"
tmap_save(migration_plot,  dpi= 600,  height=4, width=8, units="in",
          filename=paste0(out, "AT.png"))


#plot the correlation results()
plot_list <- list()
# Iterate over layers of the raster stack
for (i in 1:nlayers(mig_AT_corr)) {
 
  # Create the tm_shape object for the current layer
  current_plot <- tm_shape(mig_AT_corr[[i]]) +
    tm_raster(palette = "RdYlGn", style = "jenks", colorNA = 'grey', legend.show = TRUE, legend.reverse = TRUE) +
    tm_shape(region) +
    tm_borders() +
    tm_graticules(ticks = TRUE, lines = FALSE) +
    tm_compass(type = "arrow", position = c('left', 'top')) +
    tm_layout(legend.position = c("right", "bottom"),
              legend.outside = FALSE,
              legend.width = 1,
              legend.title.size = 0.6,
              legend.frame = T,
              legend.text.size = 0.5)
  
  # Add the current plot to the list
  plot_list[[i]] <- current_plot
}
coorp <- tmap_arrange(plot_list, ncol = 2)
tmap_save(coorp, dpi=600, height=3.11,width = 8,units = 'in', filename=paste0(out, "migATcorrplot.png"))

