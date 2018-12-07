#  Leaflet Week 2 project for devleoping data products
#  We are going to be utilizing zip code level data to create
#  a leaflet map that will then be shared to R pubs

library(dplyr)
library(leaflet)
library(zipcode)

#  Step 1:  Set the working directory and read in the data file
#           in order to limit the clusters that we will have, we are going to
#           filter for where the zip codes have a population greater than 50000 
#           people.  

setwd("C:/Users/Ryan/Documents/Data_Science_Studies/Developing Data Products/New_Assignment")

list.files()
Zip_Data <- read_csv("zip_code_database.csv")

Zip_Data <- Zip_Data %>% 
              filter(irs_estimated_population_2015 > 50000)

options(scipen=999)

# Step 2:  Here we are going to utilize the zipcodes package in order to look
#          complete formatting in order to insure that the zip codes are being
#          formatted similar to five numbers and that the zip code listed 
#          matches a true zip code

Zip_Data$zip   <- clean.zipcodes(Zip_Data$zip)

Zip_Map <- Zip_Data  %>% 
           leaflet() %>%
            addTiles() %>%
            addMarkers( lng = Zip_Data$longitude, 
                        lat = Zip_Data$latitude,
                        popup = paste("Population 2015:", Zip_Data$irs_estimated_population_2015, "<br>",
                                      "City:", Zip_Data$primary_city, "<br>",
                                      "State:", Zip_Data$state, "<br>",
                                      "Zip:", Zip_Data$zip, "<br>",
                                      "Area_Code:", Zip_Data$area_codes, "<br>",
                                      "Timezone:", Zip_Data$timezone), 
                        clusterOptions = markerClusterOptions())
Zip_Map
