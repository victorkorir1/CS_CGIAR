
#* TODO: Script to Geocode and visualize village/town names
#* Requires: Google maps API
#* Reference: https://guides.library.duke.edu/r-geospatial/geocode
#* 
#* Author: Victor Korir
################################################################################

library(ggmap)
library(tidyverse)
library(lubridate)
# Enter the API key here
register_google(key = "API KEY", write = TRUE)

#read IOM_FMS data
FMS_data <- readxl::read_xlsx('D:/OneDrive - CGIAR/SA_Team/korir/FCM/Somalia/New IOM Data/FMS_dataset_combined_clean_dept_and_transit.xlsx')

#Remove NAs and any character than cannot be Geocoded in village of departure
FMS_data <- FMS_data[-c(which(is.na(FMS_data$`City (Cleaned)`))),]
FMS_data <- FMS_data[,c("Date of Survey","What is the main reason for your Journey?" ,"District (Cleaned)" , "City (Cleaned)" )]
names <- FMS_data$`City (Cleaned)`
names <- names[-c(which(names=='Not Specified'))]
names <- as.data.frame(names)

#Geocode village admin levels
geo <- mutate_geocode(names, location = names, output = 'latlona')
write.csv(geo, file = 'D:/OneDrive - CGIAR/SA_Team/korir/geo_villages.csv')


######### GEOCODING METHOD 2 #################################################
library(tidygeocoder)
df <- tidygeocoder::geocode(names, address = names, method = "osm")
write.csv(df, file = 'D:/OneDrive - CGIAR/SA_Team/korir/tidygeo_villages.csv' )



gg_Geocoded <- read.csv('D:/OneDrive - CGIAR/SA_Team/korir/geo_villages.csv')
tidy_Geocoded <- read.csv('D:/OneDrive - CGIAR/SA_Team/korir/tidygeo_villages.csv')

Geocoded <- gg_Geocoded %>% mutate(lon = coalesce(tidy_Geocoded$long)) %>%
  mutate(lat = coalesce(tidy_Geocoded$lat))

FMS_geo <- left_join(FMS_data, Geocoded, by=c("City (Cleaned)"='names'))
FMS_geo <- FMS_geo[-c(which(is.na(FMS_geo$lat))),]

FMS_geo$`Date of Survey` <- lubridate::year(as.Date(FMS_geo$`Date of Survey`))

Fgeo_2018 <- FMS_geo[which(FMS_geo$`Date of Survey`==2018),]
Fgeo_2019 <- FMS_geo[which(FMS_geo$`Date of Survey`==2019),]
Fgeo_2020 <- FMS_geo[which(FMS_geo$`Date of Survey`==2020),]
Fgeo_2021 <- FMS_geo[which(FMS_geo$`Date of Survey`==2021),]
Fgeo_2022 <- FMS_geo[which(FMS_geo$`Date of Survey`==2022),]
Fgeo_2023 <- FMS_geo[which(FMS_geo$`Date of Survey`==2023),]

write.csv(Fgeo_2018, file = 'D:/OneDrive - CGIAR/SA_Team/korir/geo_2018.csv')
write.csv(Fgeo_2019, file = 'D:/OneDrive - CGIAR/SA_Team/korir/geo_2019.csv')
write.csv(Fgeo_2020, file = 'D:/OneDrive - CGIAR/SA_Team/korir/geo_2020.csv')
write.csv(Fgeo_2021, file = 'D:/OneDrive - CGIAR/SA_Team/korir/geo_2021.csv')
write.csv(Fgeo_2022, file = 'D:/OneDrive - CGIAR/SA_Team/korir/geo_2022.csv')
write.csv(Fgeo_2023, file = 'D:/OneDrive - CGIAR/SA_Team/korir/geo_2023.csv')


