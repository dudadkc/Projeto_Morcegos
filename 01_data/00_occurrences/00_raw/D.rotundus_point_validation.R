#Set working Directory 
#setwd("..../Data/Vampire Bat/Climate/vars/Points")

library(devtools)

#If you do not have CoordinateCleaneralready, install with line below
#install_github("ropensci/CoordinateCleaner")
1

library(countrycode)
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(rgbif)
library(sp)
library(rgeos)
library(maptools)
library(proj4)
library(data.table)
library(rgdal)
library(raster)
library(compareDF)

#If you have not already downloaded the occurence data, it is available 
#at https://doi.org/10.6084/m9.figshare.14055317
#You may also use the following code 
#To get data of organism of preference from GBIF via rgbif
#with limit set at 10mil you will get all available data records 
dat <- occ_search(scientificName = "Desmodus rotundus", limit = 10000000, hasCoordinate = T)
names(dat)

dat <- dat$data
names(dat) 
#This will get you a lot of columns in Darwin Core archive format. 

#Select variables of interest
dat <- dat %>%
  dplyr::select(scientificName,species, publishingCountry,decimalLongitude, 
                decimalLatitude, countryCode, stateProvince,locality, 
                individualCount,sex,gbifID, family, geodeticDatum,
                elevation,elevationAccuracy,taxonRank, 
                coordinateUncertaintyInMeters, year,month, day, 
                basisOfRecord,institutionCode, datasetName)
names(dat)

#If you have already consolidated records from the Desmodus rotunudus dataset, 
#use the following code to read in your data 

dat<-read.csv("Desmodus_rotundus_dataset.csv", head=T)

######Technical Validation Process of D. rotunudus record dataset##########

#plot occurrence data in geographic space to get a visual overview
wm <- borders("world", colour="gray50", fill="gray50")
ggplot()+ coord_fixed()+ wm +
  geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  theme_bw()

#For consistency, convert country code from ISO2c to ISO3c
dat$countryCode <-  countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c')

# If you want, remove records without coordinates use the following lines 
#dat <- dat%>%
  #filter(!is.na(decimalLongitude))%>%
  #filter(!is.na(decimalLatitude))

#Identify duplicate records

flags<-cc_dupl(dat, lon="decimalLongitude", lat="decimalLatitude", species="scientificName", additions=c("locality","elevationInMeters", "year", "institutionCode"), value="flagged")
flags
View(flags)
summary(flags)
#Add these flags into your original data frame for manual inspection and duplicate confirmation 
dat$flags<-flags
#write csv with flags 
write.csv(dat, "Desmodus_dataset_dup_flags.csv")

##############################################

#The following method of assessing duplicates can be used to validate previously identified duplicates 

dat_geo<- subset(dat, dat$decimalLatitude!="NA")

dat_geo$merged <- paste(dat_geo$decimalLatitude, dat_geo$decimalLongitude, sep=",")
unique_coord <- unique(dat_geo$merged)

changed <- 0
for(i in 1:length(unique_coord)){
  # Create subsets of the data using the merged.coord column
  row <- which(dat_geo$merged == unique_coord[i])
  coord_subset <- dat_geo[row, ]
  
  # Check length, if more than one there is a repeat
  if(nrow(coord_subset) > 1 && sum(coord_subset$status) >= 1) {
    for(j in 1:nrow(coord_subset)){
      if(coord_subset$status[j] == 0)  coord_subset[j, c("Lat", "Long")] <- NA
    }
    changed <- changed + nrow(coord_subset)-sum(coord_subset$status)
  }
  # Change value in data frame
  dat_geo[which(dat_geo$merged == unique_coord[i]), ][ ,c("decimalLatitude", "decimalLongitude")] <-  coord_subset[ ,c("decimalLatitude", "decimalLongitude")]
}
dat_geo1<- subset(dat_geo, dat_geo$decimalLatitude!="NA")

summary(dat_geo1)

duplicated(dat$decimalLongitude)

write.csv(dat_geo1, "Desmodus_rotundus_dataset_duplicate_validation.csv")


#####################################3

#Flag points with issues in geo-location  
dat <- data.frame(dat)

geoflags <- clean_coordinates(x = dat,
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", 
                                     "centroids", 
                                     "equal",
                                     "gbif", 
                                     "institutions", 
                                     "zeros")) 
summary(geoflags)
#plot to visualize where the flags are in geographic space 
plot(geoflags, lon = "decimalLongitude", lat = "decimalLatitude")

#Write these flags into the data set for manual confirmation 
dat$geo_flags<-geoflags
#write csv with flags 
write.csv(dat, "Desmodus_dataset_georaphic_flags.csv")

########Identify locations in the ocean 

#read in desired shp file. 
#For example this shp file has North, Central, and South American continents merged
shape<-readOGR("Americas_merged.shp")

#transform occurrence data frame into a spatial object 
occ.map<-dat
coordinates(occ.map)<-~decimalLongitude+decimalLatitude
#plot to visualize
plot(occ.map)

#search for occurrences within the limits of your shp file 
inside <- occ.map[apply(gIntersects(occ.map, shape, byid = TRUE), 2, any),]
plot(inside)

#Write these as their own data frame 
insidedf<-as.data.frame(inside@data)

#Now compare the two using compare_df
comparison<-compare_df(dat, insidedf)

#this will allow you to see the comparison data frame generated by compare_df
View(comparison$comparison_df)

#You may then write this as a csv for manual comparison 
comp_df<-as.data.frame(comparison$comparison_df)

#write.csv(comp_df, "Oceanpoints.csv")

