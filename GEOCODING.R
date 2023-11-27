
#* TODO: Script to Geocode village/town names and visualize
#* Requires: Google maps API
#* Reference: https://guides.library.duke.edu/r-geospatial/geocode
#* 
#* Author: Victor Korir
################################################################################

library(ggmap)
# Enter the API key here
register_google(key = "AIzaSyAJaOkTHJcvjCvrUXbOQq4GmrBmqO4nq6I", write = TRUE)

#read IOM_FMS data
FMS_data <- readxl::read_xlsx('D:/OneDrive - CGIAR/SA_Team/korir/FCM/Somalia/New IOM Data/FMS_dataset_combined_clean_dept_and_transit.xlsx')

#Remove NAs and any character than cannot be Geocoded in village of departure
FMS_data <- FMS_data[-c(which(is.na(FMS_data$`City (Cleaned)`))),]
names <- FMS_data$`City (Cleaned)`
names <- names[-c(which(names=='Not Specified'))]
names <- as.data.frame(names)

#Geocode village admin levels
geo <- mutate_geocode(names, location = names, output = 'latlona')

#Download shapefiles for each country and merge
DRC <- geodata::gadm(country = 'Democratic Republic of the Congo', level = 0, path = tempdir())
Uganda <- geodata::gadm(country = 'Uganda', level = 0, path = tempdir())
Ethiopia <- geodata::gadm(country = 'Ethiopia', level = 0, path = tempdir())
South_Sudan <- geodata::gadm(country = 'SSD', level = 0, path = tempdir())
Tanzania <- geodata::gadm(country = 'Tanzania ', level = 0, path = tempdir())



# Create a grid covering the region
EA_region <- rast()
EA_region <- terra::crop(EA_region, region_shp)
EA_region <- terra::mask(EA_region, region_shp)

