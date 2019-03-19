#Road Resurfacing
#01/02/2019

#created by Sean O'Donnell
#Intended use for the Infrastructure Mapping Applicaiton
#Council's road resurfacing data in tabular form to spatial table 

#https://cran.r-project.org/web/packages/dodgr/dodgr.pdf
#https://rdrr.io/cran/stplanr/man/route_graphhopper.html

library(dplyr)
library(magrittr)
library(tidyr)
library(sp)
library(readxl)
library(rmarkdown)
library(purrr)
library(ggplot2)
library(ggmap)
library(leaflet)
library(sf, lib = .libPaths()[2])
library(lwgeom)
library(stplanr)
library(osmar)
library(stringdist)
library(RCurl)
library(RJSONIO)
library(revgeo)
library(httr)
library(rvest)
library(rjson)
library(jsonlite)

#require(devtools)  
#devtools::install_github(repo = 'rCarto/photon') 

#install.packages(c("sf", "lwgeom", "stplanr", "osmar", "stplanr","dplyr","magrittr","tidyr","sp","readxl","rmarkdown","purrr","ggplot2","ggmap","leaflet"), lib = .libPaths()[1], dependancies = TRUE)

#Get the file
setwd("R:/K/Projects/Development/Planning/London_Infrastructure_Plan_2050/scripts/road_resurfacing")

source("westminster_roads.R")
source("api_keys.R")

st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

westminster_council_table <- read_xlsx(westminster_road_drive_location, na = c("TBC", "n/a", "#VALUE!"))
westminster_council_table$GSS_CODE<-"E09000033"

boroughs <- st_read("W:/GISDataMapInfo/BaseMapping/Boundaries/AdminBoundaries/2018/ESRI/London/London_Borough.shp") %>%
  st_transform(27700)

boroughs <- st_transform(boroughs, 27700)

borough_table <- st_set_geometry(boroughs, NULL)

#merge all roads as the base dataset

london_roads_folder <- "R:/K/Projects/Development/Planning/London_Infrastructure_Plan_2050/scripts/road_resurfacing/os_highways_2019.gpkg"

london_roads <- st_read(dsn = london_roads_folder, layer = "Network_RoadLink_N", crs = 27700)

london_roads_nodes <- st_read(dsn = london_roads_folder, layer = "Network_Network_ND_Junctions", crs = 27700)

#But if node hits street with same name dont duplicate
#stplanr


#london_roads_boroughs <- left_join(london_roads, borough_table, by = c("gssCode1" = "GSS_CODE")) 
borough_of_interest <- boroughs %>%
  filter(GSS_CODE == "E09000033")

#filter to westminster roads#
#THERE ARE DUPLICATE ROAD NAMES, WHICH ARE IN FACT DIFFERENT ROADS. THEY HAVE DIFFERENT AND DISCTINCT SPATIAL LOCATIONS#
london_roads_westminster <- london_roads %>%
  st_intersection(borough_of_interest)%>%
  rename_all(tolower) %>%
  mutate(road_name_lwr = tolower(roadname1)) %>%
  mutate(road_name_clean = stringr::str_replace_all(string=road_name_lwr, pattern=" ", repl="")) %>%
  mutate(road_name_clean = stringr::str_trim(road_name_clean)) %>%
  mutate(road_name_clean = gsub(pattern = "[^a-zA-Z0-9]", replacement = "", road_name_clean))

london_roads_nodes_westminster <- london_roads_nodes %>%
  st_intersection(borough_of_interest)

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

#flag if a square
westminster_council_table_intersections <- westminster_council_table$roadname %>%
  stringr::str_detect("Square|square") %>%
  cbind(westminster_council_table) %>%
  mutate(square_1 = as.numeric(.)) %>%
  select(-.)  

#flag if a junction/roundabout
westminster_council_table_intersections <- westminster_council_table_intersections$locationextents %>%
  stringr::str_detect("Junction|junction|Roundabout|roundabout") %>%
  cbind(westminster_council_table_intersections) %>%
  mutate(junction_1 = as.numeric(.)) %>%
  select(-.) 

#flag if to the end
westminster_council_table_intersections <- westminster_council_table_intersections$locationextents %>%
  stringr::str_detect("to End|to end") %>%
  cbind(westminster_council_table_intersections) %>%
  mutate(end_1 = as.numeric(.)) %>%
  select(-.)

westminster_council_table_intersections <- westminster_council_table_intersections$locationextents %>%
  stringr::str_split_fixed(pattern = " to |&", n = Inf) %>%
  cbind(westminster_council_table_intersections) %>%
  rename(intersection_1 = `1`) %>%
  rename(intersection_2 = `2`) %>%
  rename(intersection_3 = `3`) %>%
  mutate(intersection_1_lwr = tolower(intersection_1)) %>%
  mutate(intersection_1_clean = stringr::str_replace_all(string=intersection_1_lwr, pattern=" ", repl="")) %>%
  mutate(intersection_1_clean = stringr::str_trim(intersection_1_clean)) %>%
  mutate(intersection_1_clean = gsub(pattern = "[^a-zA-Z0-9]", replacement = "", intersection_1_clean)) %>%
  mutate(intersection_2_lwr = tolower(intersection_2)) %>%
  mutate(intersection_2_clean = stringr::str_replace_all(string=intersection_2_lwr, pattern=" ", repl="")) %>%
  mutate(intersection_2_clean = stringr::str_trim(intersection_2_clean)) %>%
  mutate(intersection_2_clean = gsub(pattern = "[^a-zA-Z0-9]", replacement = "", intersection_2_clean)) %>%
  mutate(intersection_2_clean = gsub(pattern = "^end$", replacement = "", intersection_2_clean)) %>%
  mutate(intersection_3_lwr = tolower(intersection_3)) %>%
  mutate(intersection_3_clean = stringr::str_replace_all(string=intersection_3_lwr, pattern=" ", repl="")) %>%
  mutate(intersection_3_clean = stringr::str_trim(intersection_3_clean)) %>%
  mutate(intersection_3_clean = gsub(pattern = "[^a-zA-Z0-9]", replacement = "", intersection_3_clean)) %>%
  mutate(startname1 = paste0(road_name_clean, intersection_1_clean)) %>%
  mutate(startname2 = paste0(road_name_clean, intersection_2_clean)) %>%
  mutate(startname3 = paste0(road_name_clean, intersection_3_clean)) %>%
  mutate(startname1_alt = paste0(intersection_1_clean, road_name_clean)) %>%
  mutate(startname2_alt = paste0(intersection_2_clean, road_name_clean)) %>%
  mutate(startname3_alt = paste0(intersection_3_clean, road_name_clean)) %>%
  mutate(endname1 = paste0(road_name_clean, intersection_1_clean)) %>%
  mutate(endname2 = paste0(road_name_clean, intersection_2_clean)) %>%
  mutate(endname3 = paste0(road_name_clean, intersection_3_clean)) %>%
  mutate(endname1_alt = paste0(intersection_1_clean, road_name_clean)) %>%
  mutate(endname2_alt = paste0(intersection_2_clean, road_name_clean)) %>%
  mutate(endname3_alt = paste0(intersection_3_clean, road_name_clean))
  

london_roads_westminster <- london_roads_westminster %>%
  select(roadname1)
  
london_roads_nodes_westminster <- london_roads_nodes_westminster %>%
  st_join(london_roads_westminster) %>%
  select(OBJECTID, roadname1) %>%
  group_by(OBJECTID) %>% 
  mutate(touching = row_number()) %>%
  spread(touching, roadname1, sep = "_")
  
london_roads_nodes_westminster_df_clean <- st_set_geometry(london_roads_nodes_westminster, NULL) %>%
  lapply(tolower) %>%
  as_tibble() %>%
  lapply(FUN = function(x) gsub(" ", "", x)) %>%
  as_tibble() %>%
  lapply(FUN = function(x) gsub("[^a-zA-Z0-9]", "", x)) %>%
  as_tibble()

#replace duplicate road names in a row with NA
london_roads_nodes_westminster_df_clean[][t(apply(london_roads_nodes_westminster_df_clean[], 1, duplicated))] <- NA

#replace take all the intersections, remove NAs with blanks and join into all names merged
node_intersections <- london_roads_nodes_westminster_df_clean %>%
  select(starts_with("touching")) %>%
  replace(is.na(.), '') %>%
  unite(all_intersects, sep = "")

#bind the intersection names back to the nodes table
london_roads_nodes_westminster_df_clean <- cbind(london_roads_nodes_westminster_df_clean, node_intersections)


########################################################################
#set up intersection 1 using OSM geocoding
OSMCRS <- sp::CRS("+init=epsg:4326")

#########################

start_node <- geo_code("Intersection of Garway Road and Westbourne Grove, London, UK", service = "google", pat = google_api_key) %>% as.numeric()
end_node <- geo_code("Intersection of Garway Road and  o/s No 1 Garway Road, London, UK", service = "google", pat = google_api_key) %>% as.numeric()

#from <- c(-1.55, 53.80) # geo_code("leeds")
#to <- c(-1.76, 53.80) # geo_code("bradford uk")
r <- stplanr::route_osrm(start_node, end_node, alt = FALSE)

route <- stplanr::route_graphhopper(from = start_node, to = end_node, vehicle = "car", silent = FALSE, pat = graphhopper_api_key)  


##########
r %>%
  st_as_sf() %>%
  st_transform(27700) %>%
  st_write("M:/line.shp")


  
#OSM_geocode <- geo_code("No 1 Garway Road, Westminster, London, UK", service = "nominatim")

  #sp::SpatialPoints(matrix(point1, ncol = 2), proj4string = OSMCRS) %>% 
   # st_as_sf()
#set up intersection 2

#stringdist(westminster_council_table_intersections$startname1[1],london_roads_nodes_westminster_df_clean$all_intersects,method='jw')


#ifelse(jarowinkler(london_roads_nodes_westminster_df_clean$all_intersects, westminster_council_table_intersections$startname1) > 0.85, london_roads_nodes_westminster_df_clean$all_intersects, NA)

#westminster_council_table_intersections[westminster_council_table_intersections$startname1 %agrep% london_roads_nodes_westminster_df_clean$all_intersects,]

#fuzzyjoin::fuzzy_semi_join(westminster_council_table_intersections, london_roads_nodes_westminster_df_clean, by = startname1 )



start_node <- 


agrep(pattern, x, max.distance = 0.1, costs = NULL,
      ignore.case = FALSE, value = FALSE, fixed = TRUE,
      useBytes = FALSE)




#get ALL roads with a match - could be duplicated if roads with same name#
roads_with_resurfacing <- full_join(london_roads_westminster, westminster_council_table_intersections, by = "road_name_clean") %>%
  filter(!is.na(roadname))

########################################################

#CLIP BY BOROUGH#
london_roads_westminster <- sapply(st_intersects(london_roads_westminster, boroughs),function(x){length(x)!=0}) %>%
  subset(london_roads_westminster, subset = .) %>%
  st_intersection(boroughs)



#CLIP BY TOUCHING ROADS OF BOROUGH ROADS#
#evolve into a for loop, replace 4 with i#

#roads_with_resurfacing[4,]
#filter out squares and junctions! 
roads_with_resurfacing_simple <- roads_with_resurfacing %>%
  filter(square_1 < 1) %>%
  filter(junction_1 < 1) %>%
  filter(end_1 < 1)

roads_with_resurfacing_clipped <- list()

for(i in 1:nrow(roads_with_resurfacing_simple)) {
  
  resurfacing_road_unclipped <- roads_with_resurfacing_simple[1,] %>%
    st_cast("MULTILINESTRING") 
  
  resurfacing_road_unclipped_df <- st_set_geometry(resurfacing_road_unclipped, NULL)
  
  print(resurfacing_road_unclipped_df$road_name_clean[1])
  
}

#plotting only#
#put into wgs for plotting
intersecting_roads <- clipping_resurfaced_roads %>%
  st_transform(4326)  

roads_by_borough_plot <- roads_with_resurfacing %>%
  st_transform(4326)

base_roads_by_borough_plot <- london_roads_westminster %>%
  st_transform(4326)

boroughs_plot <- boroughs %>%
  st_transform(4326)

#road_with_resurfaceing_plot <- resurfacing_road_unclipped %>%
#  st_transform(4326)

#split_resurfaced_road_plot <- roads_with_resurfacing_clipped %>%
#  st_transform(4326)

leaflet() %>%
  addMapPane(name = 'base', zIndex = 1) %>%
  addMapPane(name = 'themes', zIndex = 2) %>%
  clearBounds() %>%
  #51.5199312,-0.2023115
  setView(lng = -0.2023115, lat = 51.5199312, zoom = 18) %>% #create a default position
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
               weight = 3,
               options = leafletOptions(pane='themes')) #%>%
#addPolylines(data = intersecting_roads,
#            color = "yellow",
#           weight = 4,
#         options = leafletOptions(pane='themes')) %>%
#addPolylines(data = split_resurfaced_road_plot,
#         color = "orange",
#        weight = 5,
#       options = leafletOptions(pane='themes'))
