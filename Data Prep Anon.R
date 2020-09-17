library(tidyverse)
library(data.table)
library(geosphere)
setwd("C:/Users/Rebecca Sanfey/Desktop/MI/iVMS/Final_anon")

##--------Annonamise data ----##
model_output <- read.csv("C:/Users/Rebecca Sanfey/Desktop/MI/iVMS/additional_modelling/Results/FPO_vms_resultClassificationv1.2.csv")
model_output <- transform (model_output, id_num = as.numeric((factor(ID))))
model_output <- mutate(model_output, ID_anon = paste("Vessel",model_output$id_num, sep="_"))
model_output <- transform (model_output, trip_id_num = as.numeric((factor(ID_trip))))
model_output <- mutate(model_output, ID_trip_anon = paste("Trip",model_output$trip_id_num, sep="_"))

model_output <- 
  model_output %>%  
  select( -c(ID, ID_trip, id_num, trip_id_num))

saveRDS(model_output,file="Model_output_Anonymised.rds")

preppeddata <- model_output


# Distance ----------------------------------------------------------------
library(geosphere)
preppeddata <- mutate(preppeddata, 
                      Distance = distGeo(cbind(longitude, latitude), #uses ellipsoid geomodel, result in meters
                                         cbind(lag(longitude), lag(latitude))))
preppeddata$Distance[1] <- 0


# Duration ----------------------------------------------------------------
preppeddata$Duration <- NA
preppeddata$Date <-  as.character(preppeddata$Date)
preppeddata <- mutate(preppeddata, 
                      Duration = difftime(Date,
                                          lag(Date))) #duration in min
preppeddata$Duration[1] <- 0 
#preppeddata_long <- filter(preppeddata, Duration>40000)
preppeddata <- filter(preppeddata, Duration < 40000)

#  ----------------------------------------------------------------

fishing_events <- filter(preppeddata, HMM3 == "1")
fishing_events <- fishing_events %>% #removes 2
  filter(Duration > 0)
fishing_events$ID <- factor(fishing_events$ID_anon)
fishing_events$Activity_classification <- factor(fishing_events$Activity_classification)

# Inshore Grid ----------------------------------------------------------------
library(sf)
library(sp)
library(readr)
Inshore_Grid <- read_rds("Inshore_Grid.rds") #from inshore team and converted to .rds
ICES_rectangles <- read_rds("ICES_rectangles.rds") #downloaded from ICES
pnts_sf <- st_as_sf(fishing_events, coords = c('y'='longitude','x'='latitude'),remove = FALSE, crs = st_crs(Inshore_Grid))

fishing_events  <-  pnts_sf %>% 
  mutate(
    intersection = as.integer(st_intersects(geometry, Inshore_Grid)),
    RECT_intersection = as.integer(st_intersects(geometry, ICES_rectangles)),
    InshoreGrid_SQ = Inshore_Grid$Label[intersection],
    AREA = ICES_rectangles$Area_27[RECT_intersection], 
    RECT = ICES_rectangles$ICESNAME[RECT_intersection],
    Area_Rect = paste(AREA,RECT, sep=".")
  )

fishing_events$InshoreGrid_SQ <- factor(fishing_events$InshoreGrid_SQ)
fishing_events$Area_Rect <- factor(fishing_events$Area_Rect)
fishing_events_new <-  as.data.frame(fishing_events)
fishing_events <- select(fishing_events_new, -geometry)

saveRDS(fishing_events,file="Fishing_Anon.rds")
#saveRDS(fishing_events_new,file="Data/Fishing_with_geometry_Anon.rds")
saveRDS(preppeddata,file="Additional_modelled_data_prepped_Anon.rds")


#####---- Mapping prep ----#####

mapdata <- model_output
mapdata$Activity_classification <- factor(mapdata$Activity_classification)
levels(mapdata$Activity_classification)

fishing <- filter(preppeddata,Activity_classification == "FISHING")
mix_transit <-  filter(preppeddata,Activity_classification == "MIX_TRANSIT")
transit <-  filter(preppeddata,Activity_classification == "TRANSIT")
landing_sites <- read.csv("C:/Users/Rebecca Sanfey/Desktop/MI/iVMS/From Sophie/FPO_classification_function_v2/all_harbours_loc_v1.2.csv")

saveRDS(fishing,file="map_fishing.rds")
saveRDS(mix_transit,file="map_mixtransit.rds")
saveRDS(transit,file="map_transit.rds")
saveRDS(landing_sites,file="landing_sites.rds")


