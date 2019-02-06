#Road Resurfacing
#01/02/2019

#created by Sean O'Donnell
#Intended use for the Infrastructure Mapping Applicaiton
#Council's road resurfacing data in tabular form to spatial table 

library(dplyr)
library(magrittr)
library(tidyr)
library(sf)
library(readxl)
library(rmarkdown)
library(purrr, lib = "M:/R/R-3.4.3")
library(ggplot2, lib = "M:/R/R-3.4.3")
library(ggmap, lib = "M:/R/R-3.4.3")
library(leaflet, lib = "M:/R/R-3.4.3")


#Get the file
setwd("R:/K/Projects/Development/Planning/London_Infrastructure_Plan_2050/scripts/road_resurfacing")

source("westminster_roads.R")

westminster_road <- read_xlsx(westminster_road_drive_location)

boroughs <- st_read("W:/GISDataMapInfo/BaseMapping/Boundaries/AdminBoundaries/2018/ESRI/London/London_Borough.shp") %>%
  st_transform(27700)

#merge all roads as the base dataset

london_roads_folder <- "Q:/Teams/GIS&I/GIS/Processing/OrdnanceSurvey/OS Highways Network/output/London-wide streets/"

files <- dir(path = london_roads_folder, pattern = ".shp") 
files <- files[!grepl(pattern = "lock", files)]


london_roads <- list()

for(l in 1:length(files)){
  nested_sfs <- st_read(dsn = paste0(london_roads_folder,files[l]), options = "ENCODING=UTF-8") %>%
    st_transform(27700)
  name <- paste('item:',l,sep='')
  tmp <- list(table = nested_sfs)
  london_roads[[name]] <- tmp
}

london_roads <- do.call("rbind", purrr::flatten(london_roads)) %>%
  !is.na(designat03) %>%
  unique() %>%
  select(USRN,
         beginLifes,
         identifier,
         validFrom,
         operatio06,
         operatio08,
         responsi00,
         designat03,
         designat06,
         administ01,
         town1,
         gssCode1) %>%
  st_transform(27700)

london_roads <- st_transform(london_roads, 27700)
boroughs <- st_transform(boroughs, 27700)


#test#
#subset <- head(london_roads, n = 1000) %>%
#  st_transform(4326)
######

london_roads_boroughts <- left_join(london_roads, boroughs, by = ())

#agrep

roads_by_borough <- sapply(st_intersects(london_roads, boroughs[1,]),function(x){length(x)!=0}) %>%
  subset(london_roads, subset = .) %>%
  st_intersection(boroughs[1,]) %>% 
  st_join()
  dplyr::select(fid)


#plotting only#

#put into wgs for plotting
roads_by_borough_plot <- roads_by_borough %>%
  st_transform(4326)

boroughs_plot <- boroughs %>%
  st_transform(4326)

leaflet() %>%
  addMapPane(name = 'base', zIndex = 1) %>%
  addMapPane(name = 'themes', zIndex = 2) %>%
  clearBounds() %>%
  #setView(lng = -0.09, lat = 51.505, zoom = 12) %>% #create a default position
  addProviderTiles("Stamen.TonerLite",
                   options = leafletOptions(pane = 'base')) %>%
  addPolygons(data = boroughs_plot,
               color = "blue",
               opacity = 0.2,
               options = leafletOptions(pane='themes')) %>%
  addPolylines(data = roads_by_borough_plot,
                   color = "red",
                   options = leafletOptions(pane='themes'))

