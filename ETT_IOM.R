library(geodata)
library(sf)
library(tidyverse)
#Read ETT exported as CSV
ETT <- read.csv("D:/OneDrive - CGIAR/SA_Team/korir/FCM/Somalia/ETT/IOM_DTM_ETT_GedoDroughts_Tracker_R64_publish_0.csv", strip.white = TRUE)
as.numeric(ETT[[,13]])

#Cleaning
ETT <- ETT[-1,]

#Somalia admin 2 shapefile
SOM_adm2 <- st_as_sf(geodata::gadm(country = 'SOM', level = 2, version = 'latest' , path = tempdir()))
SOM_adm2_Gedo <- SOM_adm2[which(SOM_adm2$NAME_1 == 'Gedo'),]
SOM_adm2_Bay <- SOM_adm2[which(SOM_adm2$NAME_1 == 'Bay'),]

Gedo_Bay <- rbind(SOM_adm2_Bay, SOM_adm2_Gedo)

#renaming districts to match the districts and the ETT
ETT$District[ETT$District == 'Doolow'] <- 'Dolow'
ETT$District[ETT$District == 'Luuq'] <- 'Luuk'
ETT$District[ETT$District == 'Belet Xaawo'] <- 'Beled Xaawo'
ETT$District[ETT$District == 'Garbahaarey'] <- 'Garbahaaray'
ETT$District[ETT$District == 'Baardheere'] <- 'Baar-Dheere'



#Geocode the ditricts
ETT <- ETT %>% select(-contains('change'))
ETT <- ETT %>% select(1:10, ends_with('IDPS'))
ETT <- na.omit(ETT)
# Assuming 'ETT' is your data frame and columns 1:10 need cleaning
ETT <- ETT %>%
  mutate_at(vars(11:75), ~as.numeric(str_replace_all(., "[^0-9.-]", "")))



#Aggregate at admin levels
ETT_sum <- ETT %>% group_by(District) %>% summarise(across(13:74,sum))

#Plot the time trend

selected_data <- ETT_sum[ETT_sum$District == 'Baar-Dheere', ]

selected_data_long <- tidyr::gather(selected_data, key = "Column", value = "Sum", -District)

# Specify the start date, end date, and interval
start_date <- as.Date("2022-03-09")
end_date <- as.Date("2023-05-24")
interval <- "1 week"

# Create a sequence of dates
date_sequence <- seq(start_date, end_date, by = interval)

# Display the result
print(date_sequence)

selected_data_long$Index <- seq_along(selected_data_long$Column)
selected_data_long$Date_c <-date_sequence

ggplot(selected_data_long, aes(x = Index, y = Sum)) +
  geom_line() +
  labs(x = 'Columns', y = 'Sum', title = paste('Trend for District:', 'Baar-Dheere'))

#Load climate data

#Load conflict data

#Pearson correlation