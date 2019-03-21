#Road Resurfacing
#01/02/2019

#created by Sean O'Donnell
#Intended use for the Infrastructure Mapping Applicaiton
#Council's road resurfacing data in tabular form to spatial table 


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

#Get the file
setwd("R:/K/Projects/Development/Planning/London_Infrastructure_Plan_2050/scripts/road_resurfacing")

source("westminster_roads.R")
source("api_keys.R")
source("route_graphhopper_so_fix.R")

st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

westminster_council_table <- read_xlsx(westminster_road_drive_location, na = c("TBC", "n/a", "#VALUE!"))
westminster_council_table$GSS_CODE<-"E09000033"

boroughs <- st_read("W:/GISDataMapInfo/BaseMapping/Boundaries/AdminBoundaries/2018/ESRI/London/London_Borough.shp") %>%
  st_transform(27700)

boroughs_name <- "Westminster"

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

OSMCRS <- sp::CRS("+init=epsg:4326")

########################################################################

#filter out squares and ends and blank intersection 2 
#roadname plus intersection_1 = start_node
#roadname plus intersection_2 = end node_node
#intersection of road & intersec1 + road & intersect2 + borough + london + uk

westminster_council_table_intersections$start_string <- paste("Intersection of", 
                      westminster_council_table_intersections$roadname, 
                      "and", 
                      westminster_council_table_intersections$intersection_1,
                      ",",
                      boroughs_name,
                      ", London, UK")

westminster_council_table_intersections$end_string <- paste("Intersection of", 
                    westminster_council_table_intersections$roadname, 
                    "and", 
                    westminster_council_table_intersections$intersection_2,
                    ",",
                    boroughs_name,
                    ", London, UK")

westminster_council_table_intersections <- westminster_council_table_intersections %>%
  filter(square_1 < 1) %>%
  filter(junction_1 < 1) %>%
  filter(end_1 < 1)

starting_points <- list()
ending_points <- list()

starting_points_bng <- list()
ending_points_bng <- list()

#Function to build spatial lists of start points, end points, and road resurfacing
#often errors due to no response and overload from rousting server
#there is a function to catch this. 

for(k in 1:nrow(westminster_council_table_intersections)) {

  start_node <- geo_code(westminster_council_table_intersections$start_string[k], return_all = TRUE, service = "google", pat = google_api_key) %>% 
    as.data.frame()
  
  if (start_node$types == "intersection"){
    start_node <- filter(start_node, types == "intersection")
  } else {
    print("false")
  }
  
  start_node <- start_node %>%
    select(matches('geometry.location.lat|geometry.location.lng'))%>%
    filter(row_number() == 1)
  
  #tibble::rownames_to_column(var = "latlong") %>%
  #filter(latlong == "lon1"| latlong == "lat1" | latlong == "lon"| latlong == "lat")
  
  start_node <- c(start_node[1,2], start_node[1,1])
  
  starting_points[[k]] <- start_node
  
  start_node_bng <- sp::SpatialPoints(matrix(start_node, ncol = 2), proj4string = OSMCRS) %>% 
    st_as_sf() %>%
    st_transform(27700)
  
  starting_points_bng[[k]] <- start_node_bng
}


for(l in 1:nrow(westminster_council_table_intersections)) {
  
  end_node <- geo_code(westminster_council_table_intersections$end_string[l], return_all = TRUE, service = "google", pat = google_api_key) %>% 
    as.data.frame()
  
  if (end_node$types == "intersection"){
    end_node <- filter(end_node, types == "intersection")
  } else {
    print("false")
  }
  
  end_node <- end_node %>%
    select(matches('geometry.location.lat|geometry.location.lng'))%>%
    filter(row_number() == 1)
  
  #tibble::rownames_to_column(var = "latlong") %>%
  #filter(latlong == "lon1"| latlong == "lat1" | latlong == "lon"| latlong == "lat")

end_node <- c(end_node[1,2], end_node[1,1])

ending_points[[l]] <- end_node

end_node_bng <- sp::SpatialPoints(matrix(end_node, ncol = 2), proj4string = OSMCRS) %>% 
  st_as_sf() %>%
  st_transform(27700)

ending_points_bng[[l]] <- end_node_bng

}

starting_points_bng_sf <- do.call(rbind, starting_points_bng)
ending_points_bng_sf <- do.call(rbind, ending_points_bng)

####need midpoints so most effecient route isn't taken
#use startpoint and endpoint to create a bounding box
#use the bounding box to filter nodes
#find a node which intersects the road twice 
#or create a bounding box, clip the lines, select by street name, then get midpoint of that segment

bbox_roads <- list()
primary_road_path <- list()
primary_road_path_nested <- list()

for (z in 1:44) {
  a = st_sf(a = 1:2, geom = st_sfc(st_point(st_coordinates(starting_points_bng_sf[z,1])), st_point(st_coordinates(ending_points_bng_sf[z,1]))), crs = 27700)
  bbox <- st_bbox(a)
  bbox_roads[[z]] <- st_intersection(london_roads_westminster, st_as_sfc(bbox))
  
  primary_road <- westminster_council_table_intersections[z,] %>%
    select(roadname)
  
  primary_road_path[[z]] <- bbox_roads[[z]][bbox_roads[[z]]$roadname1 %agrep% primary_road,] %>%
    st_centroid() %>%
    st_transform(4326) %>%
    st_coordinates() %>%
    as.data.frame() #%>% 
    #mutate(point = paste(X, Y)) %>%
    #select(point)
  obj <- list()
  obj[[1]] <- starting_points[[z]]
  
  for (n in 1:nrow(primary_road_path[[z]])) {
    obj[[n+1]] <- c(primary_road_path[[z]][1][n,], primary_road_path[[z]][2][n,])
  }
  #number of path points plus two (to account for the start point)
  obj[[nrow(primary_road_path[[z]])+2]] <- ending_points[[z]]
  
  primary_road_path_nested[[z]] <- obj
  
}

bbox_roads_sf <- do.call(rbind, bbox_roads)

########
#test the number of items in each list match
ifelse(length(ending_points) == length(starting_points), print("same amount of start & end"), stop("mismatch in geocoding"))
########

roads_with_resurfacing <- list()

for(m in 1:length(starting_points)) {
#for(m in 1:8) {
  print(starting_points[[m]])
  print(ending_points[[m]])
  #route_graphhopper_so_fix(from = starting_points[[1]], to = ending_points[[1]], vehicle = "foot", silent = FALSE, pat = graphhopper_api_key) %>%
  
  ##for loop over this to build a line set then union e.g. primary_road_path_nested[[2]][[1]] is start primary_road_path_nested[[2]][[5]] is end
  roads_with_resurfacing[[m]] <- route_graphhopper_so_fix(from = starting_points[[m]], to = ending_points[[m]], vehicle = "foot", silent = FALSE, pat = graphhopper_api_key) %>%
    st_as_sf() %>%
    st_transform(27700)
}

#start and endpoint list of resurfacing routes

#intersection_points <- do.call(rbind, intersection_points)

roads_with_resurfacing_sf <- do.call(rbind, roads_with_resurfacing)

##########
st_write(roads_with_resurfacing_sf, "M:/route_testing/line9.shp")
st_write(primary_road_path, "M:/route_testing/path3.shp")
st_write(bbox_roads_sf, "M:/route_testing/near_roads2.shp")
st_write(starting_points_bng_sf, "M:/route_testing/start8.shp")
st_write(london_roads_westminster, "M:/route_testing/westminster_roads2.shp")
st_write(ending_points_bng_sf, "M:/route_testing/end8.shp")


##############using a local routes network of OS highways ############

network <- london_roads %>%
  st_transform(4326) %>%
  SpatialLinesNetwork(uselonglat = FALSE, tolerance = 0.001)

class(london_roads)
class(london_roads_westminster)

test_local <- route_local(sln = network, from = starting_points[[1]], to = ending_points[[1]])

  
#OSM_geocode <- geo_code("No 1 Garway Road, Westminster, London, UK", service = "nominatim")

  #sp::SpatialPoints(matrix(point1, ncol = 2), proj4string = OSMCRS) %>% 
   # st_as_sf()
#set up intersection 2

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
