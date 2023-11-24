library(ggmap)
register_google(key = "AIzaSyAJaOkTHJcvjCvrUXbOQq4GmrBmqO4", write = TRUE)

#read Fms data
FMS_data <- readxl::read_xlsx('D:/OneDrive - CGIAR/SA_Team/korir/FCM/Somalia/New IOM Data/FMS_dataset_combined_clean_dept_and_transit.xlsx')

#Remove NAs in village of departure
FMS_data <- FMS_data[-c(which(is.na(FMS_data$`City (Cleaned)`))),]

FMS_data$geo <- mutate_geocode(FMS_data, location = `City (Cleaned)`, output = 'latlona')
