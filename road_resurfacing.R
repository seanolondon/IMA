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

westminster_council_table <- read_xlsx(westminster_road_drive_location, na = c("TBC", "n/a", "#VALUE!"))
westminster_council_table$GSS_CODE<-"E09000033"

boroughs <- st_read("W:/GISDataMapInfo/BaseMapping/Boundaries/AdminBoundaries/2018/ESRI/London/London_Borough.shp") %>%
  st_transform(27700)

boroughs <- st_transform(boroughs, 27700)

borough_table <- st_set_geometry(boroughs, NULL)



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
  filter(!is.na(designat03)) %>%
  unique() %>%
  st_transform(27700)

london_roads <- st_transform(london_roads, 27700)

#test#
#subset <- head(london_roads, n = 1000) %>%
#  st_transform(4326)
######


london_roads_boroughs <- left_join(london_roads, borough_table, by = c("gssCode1" = "GSS_CODE")) 

#filter to westminster roads#
#THERE ARE DUPLICATE ROAD NAMES, WHICH ARE IN FACT DIFFERENT ROADS. THEY HAVE DIFFERENT AND DISCTINCT SPATIAL LOCATIONS#
london_roads_westminster <- london_roads_boroughs %>%
  filter(gssCode1 == "E09000033") %>%
  rename_all(tolower) %>%
  mutate(road_name_lwr = tolower(designat03)) %>%
  mutate(road_name_clean = stringr::str_replace_all(string=road_name_lwr, pattern=" ", repl="")) %>%
  mutate(road_name_clean = stringr::str_trim(road_name_clean)) %>%
  mutate(road_name_clean = gsub(pattern = "[^a-zA-Z0-9]", replacement = "", road_name_clean))


#reformat the council provided road resurfacing table
colnames(westminster_council_table) <- gsub(pattern = "[^a-zA-Z0-9]", replacement = "", colnames(westminster_council_table))

#clean the road name column
##dynamically select 'road name' in future development
westminster_council_table <- westminster_council_table %>%
  rename_all(tolower) %>%
  mutate(road_name_lwr = tolower(roadname)) %>%
  mutate(road_name_clean = stringr::str_replace_all(string=road_name_lwr, pattern=" ", repl="")) %>%
  mutate(road_name_clean = stringr::str_trim(road_name_clean)) %>%
  mutate(road_name_clean = gsub(pattern = "[^a-zA-Z0-9]", replacement = "", road_name_clean))

#testing the start and finish clumns#
westminster_council_table <- westminster_council_table %>%
  str_split_fixed(string, pattern, n)


#CLIP BY BOROUGH#
london_roads_westminster <- sapply(st_intersects(london_roads_westminster, boroughs),function(x){length(x)!=0}) %>%
  subset(london_roads_westminster, subset = .) %>%
  st_intersection(boroughs)
#TEST MATCHING#
#get ALL roads with a match - could be duplicated if roads with same name#
roads_with_resurfacing <- full_join(london_roads_westminster, westminster_council_table, by = "road_name_clean") %>%
  filter(!is.na(roadname))
#TESTFUZZYMATCH#
fuzzy_inner_join(y, by = c("string" = "seed"), match_fun = str_detect)
#FILTER JOIN BY ROADNAME MATCH#
#CLIP BY TOUCHING/INTERSECTING JUNCTIONS#
  


#agrep
#####clipper test######
roads_by_borough <- sapply(st_intersects(london_roads, boroughs[1,]),function(x){length(x)!=0}) %>%
  subset(london_roads, subset = .) %>%
  st_intersection(boroughs[1,]) %>% 
  st_join()
  dplyr::select(fid)


#plotting only#

#put into wgs for plotting
roads_by_borough_plot <- roads_with_resurfacing %>%
  st_transform(4326)

base_roads_by_borough_plot <- london_roads_westminster %>%
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
  addPolylines(data = base_roads_by_borough_plot,
              color = "green",
              weight = 3,
              opacity = 0.2,
              options = leafletOptions(pane='themes')) %>%
  addPolylines(data = roads_by_borough_plot,
             color = "red",
             weight = 4,
             options = leafletOptions(pane='themes'))

