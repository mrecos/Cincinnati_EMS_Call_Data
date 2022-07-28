library(sf)
library(tidycensus)
library(mapview)
library(spdep)
library(caret)
library(ckanr) 
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot)
library(jtools)   
library(stargazer) 
library(broom)
library(tufte)
library(rmarkdown)
library(kableExtra)
library(tidycensus)
library(RSocrata)
library(viridis)
library(spatstat)
library(raster)
library(knitr)
library(rgdal)
library(kableExtra)
library(tidyverse)

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")
## data ingest
# cinPD <- st_read("https://opendata.arcgis.com/datasets/9bc1afaff72e4f44a6d19280c159c951_4.geojson") %>%
#   st_transform('ESRI:102258')

cinBoundary <- 
  st_read("https://opendata.arcgis.com/datasets/ed78f4754b044ac5815d0a9efe9bb336_1.geojson") %>%
  st_transform('ESRI:102258') 

firestation <- st_read("https://opendata.arcgis.com/datasets/a6f043d181f94e37a274975a3718b7af_16.geojson") %>%
  filter(BND_NAME == "CINCINNATI")%>%
  st_transform(st_crs(cinBoundary))

#Opioid Overdose Data
opioid <- 
  st_read("./DATA/CINC_OD_DATA.geojson", crs = "epsg:4326") %>% 
  st_transform('ESRI:102258')

# Creating a fishnet grid
cin_fishnet <- 
  st_make_grid(cinBoundary,
               cellsize = 500, 
               square = FALSE) %>%
  .[cinBoundary] %>% 
  st_sf() %>%
  mutate(uniqueID = rownames(.))

cinNeigh <-
  st_read("https://opendata.arcgis.com/datasets/fff393f0112544b397838f9cf4d7765a_1.geojson") %>%
  st_transform('ESRI:102258') %>%
  st_transform(st_crs(cin_fishnet)) 

# import risk factors
request_311 <-
  read_csv("./DATA/Cincinnati_311__Non-Emergency__Service_Requests.csv") %>% 
  filter(grepl('2019|2020',REQUESTED_DATE))

# dead_animals <- request_311%>%
#   filter(grepl("DAPUB1",SERVICE_CODE))%>%
#   dplyr::select(Y = LATITUDE, X = LONGITUDE) %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
#   st_transform('ESRI:102258') %>%
#   st_transform(st_crs(cin_fishnet))%>%
#   mutate(Legend = "dead_animals")

abandon_cars <- request_311%>%
  filter(grepl("ABAN-VPR", SERVICE_CODE))%>%
  dplyr::select(Y = LATITUDE, X = LONGITUDE) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102258') %>%
  st_transform(st_crs(cin_fishnet))%>%
  mutate(Legend = "abandon_cars")  

pothole <- request_311%>%
  filter(grepl("PTHOLE",SERVICE_CODE))%>%
  dplyr::select(Y = LATITUDE, X = LONGITUDE) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102258') %>%
  st_transform(st_crs(cin_fishnet))%>%
  mutate(Legend = "pothole")

trash <- request_311%>%
  filter(grepl("RF-COLLT",SERVICE_CODE) |grepl("TRASH-I",SERVICE_CODE))%>%
  dplyr::select(Y = LATITUDE, X = LONGITUDE) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102258') %>%
  st_transform(st_crs(cin_fishnet))%>%
  mutate(Legend = "trash")

graffiti <- request_311%>%
  filter(grepl("GRFITI",SERVICE_CODE))%>%
  dplyr::select(Y = LATITUDE, X = LONGITUDE) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102258') %>%
  st_transform(st_crs(cin_fishnet))%>%
  mutate(Legend = "graffiti")

street_cleaning <- request_311%>%
  filter(grepl("SCLEN1",SERVICE_CODE))%>%
  dplyr::select(Y = LATITUDE, X = LONGITUDE) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102258') %>%
  st_transform(st_crs(cin_fishnet))%>%
  mutate(Legend = "street_cleaning")

# Census data
# View(load_variables(2018,'acs5',cache = TRUE))
# View(var20 <- load_variables(2020, "acs5"))
# # not getting poverty or unemployment using same codes for block groups
# 
vars <- c("B01001_001E", # total pop
          "B19013_001E", # med HHIC
          "B25002_001E", # Total Units
          "B06012_002E", # Pov
          "B27011_008E") # Unemployment
tracts20 <- 
  get_acs(geography = "tract", variables = vars, 
          year=2020, state="OH", county="Hamilton", geometry=T, output="wide") %>%
  st_transform('ESRI:102258') %>%
  rename(TotalPop = B01001_001E, 
         MedHHInc = B19013_001E,
         TotalUnit = B25002_001E,
         TotalPoverty = B06012_002E,
         TotalUnemployment = 	B27011_008E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>% #-starts_with("B") awesome!
  mutate(pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop *100, 0),
         pctUnemploy = ifelse(TotalPop > 0, TotalUnemployment / TotalPop *100, 0)
  ) %>%
  dplyr::select(-TotalPoverty ,-TotalUnemployment,-GEOID) %>%
  st_transform(st_crs(cin_fishnet)) 

tracts20.MedHHInc <- tracts20 %>%
  dplyr::select(MedHHInc) 
tracts20.TotalPop <- tracts20 %>%
  dplyr::select(TotalPop) 
tracts20.TotalUnit <- tracts20 %>%
  dplyr::select(TotalUnit) 
tracts20.pctPoverty <- tracts20 %>%
  dplyr::select(pctPoverty)
tracts20.pctUnemploy <- tracts20 %>%
  dplyr::select(pctUnemploy)

#### end data ingest

# Aggregate points to the fishnet
opioid_net <- 
  dplyr::select(opioid) %>% 
  mutate(countopioid = 1) %>% 
  aggregate(., cin_fishnet, sum) %>%
  mutate(countopioid = replace_na(countopioid, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(cin_fishnet) / 24), 
                       size=nrow(cin_fishnet), replace = TRUE))

## firestations
stationsBuffer_QtrMi <- 
  st_buffer(firestation, 1320)

#determine the count of overdoses within 1/4-mile of each fire station
DoseCentroids <- 
  st_centroid(opioid) %>%
  mutate(counter = 1) %>% 
  dplyr::select(counter)

buffersAndDose <- 
  aggregate(DoseCentroids, stationsBuffer_QtrMi, sum) %>%
  cbind(stationsBuffer_QtrMi) %>%
  mutate(counter = replace_na(counter, 0),
         Area = as.numeric(st_area(.))) 

summarizeFire <-
  buffersAndDose %>%
  group_by(ENGINE) %>%
  summarize(Building_Count = sum(counter))

#### Features to net

# All variables in fishnet 
vars_net <- 
  rbind(abandon_cars,pothole,trash,graffiti,street_cleaning) %>%
  st_join(., cin_fishnet, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(uniqueID, Legend) %>%
  summarize(count = n()) %>%
  full_join(cin_fishnet, by = "uniqueID") %>%
  spread(Legend, count, fill=0) %>%
  st_sf() %>%
  na.omit() %>% 
  dplyr::select(-`<NA>`) %>%
  ungroup()
### Multiple map for feature counts in fishnet
vars_net.long <- 
  gather(vars_net, Variable, value, -geometry, -uniqueID)
vars <- unique(vars_net.long$Variable)

### KNN
## Nearest Neighbor Feature
# convenience to reduce length of function names.
st_c <- st_coordinates
st_coid <- st_centroid
## create NN from abandoned cars, k = 3
'%!in%' <- function(x,y)!('%in%'(x,y))
vars_net$abandon_cars.nn <- 
  nn_function(st_c(st_coid(vars_net)),st_c(abandon_cars),k = 5)

vars_net$pothole.nn <-
  nn_function(st_c(st_coid(vars_net)),st_c(pothole),k = 10)

vars_net$trash.nn <-
  nn_function(st_c(st_coid(vars_net)),st_c(trash),k = 5) 

vars_net$graffiti.nn <-
  nn_function(st_c(st_coid(vars_net)),st_c(graffiti),k = 8) 

vars_net$street_cleaning.nn <-
  nn_function(st_c(st_coid(vars_net)),st_c(street_cleaning),k = 3)

#### need to extract census vars and join to net
#### 
pctUnemploy.net <- vars_net %>% 
  st_centroid() %>% 
  st_intersection(tracts20.pctUnemploy) %>% 
  dplyr::select(uniqueID, pctUnemploy)

MedHHInc.net <- vars_net %>% 
  st_centroid() %>% 
  st_intersection(tracts20.MedHHInc) %>% 
  dplyr::select(uniqueID, MedHHInc)

pctPoverty.net <- vars_net%>% 
  st_centroid() %>% 
  st_intersection(tracts20.pctPoverty) %>% 
  dplyr::select(uniqueID, pctPoverty)

TotalPop.net <- vars_net %>% 
  st_centroid() %>% 
  st_intersection(tracts20.TotalPop) %>% 
  dplyr::select(uniqueID, TotalPop)

TotalUnit.net <- vars_net %>% 
  st_centroid() %>% 
  st_intersection(tracts20.TotalUnit) %>% 
  dplyr::select(uniqueID, TotalUnit)

## join back to vars_net
vars_net <- vars_net %>% 
  left_join(st_drop_geometry(pctUnemploy.net),
            by = "uniqueID") %>% 
  left_join(st_drop_geometry(MedHHInc.net),
            by = "uniqueID") %>% 
  left_join(st_drop_geometry(pctPoverty.net),
            by = "uniqueID") %>% 
  left_join(st_drop_geometry(TotalPop.net),
            by = "uniqueID") %>% 
  left_join(st_drop_geometry(TotalUnit.net),
            by = "uniqueID") 

## join target
cin_final_net <-
  left_join(opioid_net, st_drop_geometry(vars_net), by="uniqueID") 
cin_final_net <-
  st_centroid(cin_final_net) %>%
  st_join(dplyr::select(cinNeigh, NEIGH), by = "uniqueID") %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(cin_final_net, geometry, uniqueID)) %>%
  st_sf() %>%
  na.omit() %>% 
  st_transform(crs="epsg:4326")

cin_final <- cbind(cin_final_net, 
                   st_coordinates(st_centroid(st_transform(cin_final_net, crs = "epsg:4326")))) %>% 
  st_drop_geometry() %>% 
  dplyr::select(-uniqueID, -cvID) %>% 
  rename("Longitude" = X, "Latitude" = Y)

write_sf(cin_final_net, "./DATA/cin_final_net.geojson")
# write.csv(cin_final, "Cinc_OD_pnts_ACS_KNN.csv", row.names = FALSE)
write.csv(cin_final %>% 
            mutate(id = 1:n()), "Cinc_OD_pnts_ACS_KNN_PREDICT.csv", row.names = FALSE)

### PLOT
mapview(cin_final_net, zcol = "countopioid", col.regions = viridis(60, option = "G"), 
        layer.name = "Case Counts", alpha.regions = 0.8) + 
  mapview(cin_final_net, zcol = "MedHHInc", col.regions = viridis(60, option = "A"), 
          layer.name = "Income", alpha.regions = 0.8)+ 
  mapview(cin_final_net, zcol = "graffiti.nn", col.regions = viridis(60, option = "A"), 
          layer.name = "Graffiti", alpha.regions = 0.8)+ 
  mapview(cin_final_net, zcol = "TotalUnit", col.regions = viridis(60, option = "A"), 
          layer.name = "Rental Density", alpha.regions = 0.8)

new_DR_Diverging_pal <- DataRobot_pal("DR_Diverging")
mapview(cin_final_net, zcol = "MedHHInc", col.regions = new_DR_Diverging_pal(16), 
        layer.name = "Income", alpha.regions = 0.4)

#library(DataRobotColors)
case_breaks = c(1,10,50,200)
ggplot(cin_final_net, aes(fill = countopioid+0.1)) +
  geom_sf(size=0.1) +
  scale_fill_DataRobot(discrete = FALSE, palette = "DR_Diverging", 
                       name = "Incident\nCount",trans = "log",
                       labels = case_breaks, breaks = case_breaks) +
  theme_void()
ggsave("countopioid.png", width = 5, height = 5)

ggplot(cin_final_net, aes(fill = graffiti  )) +
  geom_sf(size=0.1) +
  scale_fill_DataRobot(discrete = FALSE, palette = "DR_Diverging", 
                       name = "Median\nIncome") +
  theme_void()
ggsave("MedHHInc.png", width = 5, height = 5)





