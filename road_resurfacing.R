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
westminster_council_table_intersections <- westminster_council_table$roadname %>%
  stringr::str_detect("Square|square") %>%
  cbind(westminster_council_table) %>%
  mutate(square_1 = as.numeric(.)) %>%
  select(-.)  


westminster_council_table_intersections <- westminster_council_table_intersections$locationextents %>%
  stringr::str_detect("Junction|junction") %>%
  cbind(westminster_council_table_intersections) %>%
  mutate(junction_1 = as.numeric(.)) %>%
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
  mutate(intersection_3_lwr = tolower(intersection_3)) %>%
  mutate(intersection_3_clean = stringr::str_replace_all(string=intersection_3_lwr, pattern=" ", repl="")) %>%
  mutate(intersection_3_clean = stringr::str_trim(intersection_3_clean)) %>%
  mutate(intersection_3_clean = gsub(pattern = "[^a-zA-Z0-9]", replacement = "", intersection_3_clean))


#get ALL roads with a match - could be duplicated if roads with same name#
roads_with_resurfacing <- full_join(london_roads_westminster, westminster_council_table_intersections, by = "road_name_clean") %>%
  filter(!is.na(roadname))

########################################################

#CLIP BY BOROUGH#
london_roads_westminster <- sapply(st_intersects(london_roads_westminster, boroughs),function(x){length(x)!=0}) %>%
  subset(london_roads_westminster, subset = .) %>%
  st_intersection(boroughs)

#CLIP BY TOUCHING ROADS OF BOROUGH ROADS#
#evolve into a for loop, replace 1 with i#
touching_resurfaced_roads <- sapply(st_intersects(london_roads_westminster, roads_with_resurfacing[1,]),function(x){length(x)!=0}) %>%
  subset(london_roads_westminster, subset = .) 

#filter out the resurfaced road spatially
clipping_resurfaced_roads <- sapply(st_equals(touching_resurfaced_roads, roads_with_resurfacing[1,]),function(x){length(x)==0}) %>%
  subset(touching_resurfaced_roads, subset = .) %>%
  
#join the full road detail back to the clipping roads  
clip_road_1 <- roads_with_resurfacing[1,] %>%
  st_set_geometry(NULL) %>%
  inner_join(x = clipping_resurfaced_roads, by = c("road_name_clean" = "intersection_1_clean"))

clip_road_2 <- roads_with_resurfacing[1,] %>%
  st_set_geometry(NULL) %>%
  inner_join(x = clipping_resurfaced_roads, by = c("road_name_clean" = "intersection_2_clean"))

clip_road_3 <- roads_with_resurfacing[1,] %>%
  st_set_geometry(NULL) %>%
  inner_join(x = clipping_resurfaced_roads, by = c("road_name_clean" = "intersection_3_clean")) 
  
ifelse(nrow(clip_road_3) == 0, rm(clip_road_3))



#TESTFUZZYMATCH#
#fuzzy_inner_join(y, by = c("string" = "seed"), match_fun = str_detect)

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
intersecting_roads <- clipping_resurfaced_roads %>%
  st_transform(4326)  
  
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
             weight = 3,
             options = leafletOptions(pane='themes')) %>%
  addPolylines(data = intersecting_roads,
               color = "purple",
               weight = 4,
               options = leafletOptions(pane='themes'))


