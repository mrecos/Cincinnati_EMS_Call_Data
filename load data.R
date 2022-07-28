# original data: https://data.cincinnati-oh.gov/Safety/Cincinnati-Fire-Incidents-CAD-including-EMS-ALS-BL/vnsz-a3wp

library(readr)
library(tidyverse)
library(mapview)
library(sf)
mv <- mapview
g <- glimpse
dat <- read_csv("DATA/Cincinnati_Fire_Incidents__CAD___including_EMS__ALS_BLS_.csv")

# "HEROIN OVERDOSE"  
dat_od <- dat %>% 
  filter(CFD_INCIDENT_TYPE_GROUP == "HEROIN OVERDOSE") %>% 
  filter(LATITUDE_X > 0) %>% 
  st_as_sf(coords = c("LONGITUDE_X","LATITUDE_X"), crs = 4326)
mv(dat_od)  

# write_sf(dat_od, "DATA/CINC_OD_DATA.geojson")
# write.csv(dat_od, "DATA/CINC_OD_DATA.csv")
