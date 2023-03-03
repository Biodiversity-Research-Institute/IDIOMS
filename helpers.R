# Upload Packages ---------------------------------

library(shiny)
library(shinydashboard)
library(shinybusy)
library(profmem)
library(leaflet)
library(leaflet.esri)
library(rhandsontable)
library(plyr)
library(dplyr)
library(tidyverse)
library(magrittr)
library(shinyBS)
library(msm)
library(shinyjs)
library(V8)
library(shinyWidgets)
library(data.table)
library(DT)
library(zip)
library(RColorBrewer)
library(pracma)
library(devtools)
library(rcmdcheck)
library(sp)
library(sf)
library(rgdal)
library(geosphere)
library(maxcovr)
library(rootSolve)
library(nngeo)
library(tibble)
library(readr)
library(lpSolve)
library(Rglpk)
library(gtools)
library(DescTools)
library(rgeos)
library(SpatialPosition)
library(lutz)
library(rhandsontable)
library(htmltools)
library(knitr)
library(kableExtra)
library(basemaps)
library(oce)

# Rprofmem("idioms.memprof.1",threshold=400000)

row_id <- 0

# Function to create icons and labels with info about parameters
label.help <- function(label, id){
  HTML(paste0(label, actionLink(id,label=NULL,icon=icon('info-circle'))))
}


species <- sort(c("Roseate Tern", "Piping Plover", "Red Knot", "Northern Gannet", "Double-crested Cormorant"))
defaultSpecies<- "Roseate Tern"

# set theme for ggplot ------------------------------
theme_set(theme_bw())


# Set shiny options ---------------------------------
#options(shiny.error = browser)
#options(shiny.reactlog=TRUE)

# xi_min_dbm <- encoded_signal_to_actual_dbm(52.5)  #when recieved power = noise - Lotek receiver displays 52.5 = 4.348446e-11 watts (-73.62 dbm), must be above this

startUpValues <- list(
  stnPars = list(
    antNum = 4, antHt = 25, antLambda=1.8, antD0=9, antP0=4.89E-11, min_receiver_sensitivity=-73.62, stn_num=3
  ),

  studyareaPars = list(
    studyareaPars_nlocs = 100, studyareaPars_spacing_m = 1852 ,studyareaPars_Latitude = 41.5, studyareaPars_Longitude = -70.25,
    studyareaPars_width = 20, studyareaPars_length = 20
  ),
  thresholdsPars = list(min_coverage = 0.75)
)

load("data/shorebird_lines_sf.Rdata")
load("data/seabird_lines_sf.Rdata")

# usa <- sf::read_sf("data/statesp020.shp") %>% st_transform(4326)
BOEM_lease_outlines <- sf::read_sf("data/BOEMWindLeaseOutlines_6_1_2022.shp") %>% st_transform(3857)
BOEM_planning_area_outlines <- sf::read_sf("data/BOEMWindPlanningAreas_06_01_2022.shp") %>% st_transform(3857)

#https://stackoverflow.com/questions/21677489/fill-geospatial-polygons-with-pattern-r
# gridding function for filling polygons
pattern <- function(x, size, pattern) {
  ex = list(
    horizontal = c(1, 2),
    vertical = c(1, 4),
    left2right = c(2, 4),
    right2left = c(1, 3)
  )
  fillgrid = st_make_grid(x, cellsize = size)
  endsf = lapply(1:length(fillgrid), function(j)
    sf::st_linestring(sf::st_coordinates(fillgrid[j])[ex[[pattern]], 1:2]))
  endsf = sf::st_sfc(endsf, crs = sf::st_crs(x))
  endsf = sf::st_intersection(endsf, x)
  endsf = endsf[sf::st_geometry_type(endsf)
                %in% c("LINESTRING", "MULTILINESTRING")]
  endsf = sf::st_line_merge(sf::st_union(endsf))
  return(endsf)
} 

# https://stackoverflow.com/questions/6177629/how-to-silence-the-output-from-this-r-package
# small function that silences cat() and print() (but not message() or warning()) and returns whatever the expression returned:
shut_up = function(expr) {
  #temp file
  f = file()
  
  #write output to that file
  sink(file = f)
  
  #evaluate expr in original environment
  y = eval(expr, envir = parent.frame())
  
  #close sink
  sink()
  
  #get rid of file
  close(f)
  
  # y
}

# generate continuous Spectral pallete
Spectral_pal_cont <- colorRampPalette(rev(brewer.pal(11,"Spectral")))
YlOrRd_pal_cont <- colorRampPalette(c("white", brewer.pal(9,"YlOrRd")))
PuBuGn_pal_cont <- colorRampPalette(brewer.pal(9,"PuBuGn"))
YlOrBr_pal_cont <- colorRampPalette(c("white",brewer.pal(9,"YlOrBr")))
manual1_pal_cont <- colorRampPalette(rev(c("#8E063B", "#AB4147", "#C56551", "#DA8459", "#E99F61", "#F2B669", "#F6C971", "#F4D97B", "#EDE388", "#E2E6BD", "#e0e2cc", "#f1f2e6")))
manual2_pal_cont <- colorRampPalette(rev(c("#7D0112", "#8E2C19", "#9E4723", "#AD5F30", "#BC763E", "#C88C4F", "#D4A261", "#DEB675", "#E6C98A", "#ECDAA0", "#F1E9B8", "#F2F1E4")))

turbine_icon <- makeIcon(
  # iconUrl = "P:/NYSERDA Nanotag tools/nanotag_guidance_Shiny_tool/auto_radio_telemetry_Shiny_tool/www/baseline_toys_black_18dp.png",
  iconUrl = "www/baseline_toys_black_18dp.png",
  iconWidth = 10, iconHeight = 10
  # iconAnchorX = 0, iconAnchorY = 0
)

turbine_icon_red <- makeIcon(
  # iconUrl = "P:/NYSERDA Nanotag tools/nanotag_guidance_Shiny_tool/auto_radio_telemetry_Shiny_tool/www/baseline_toys_black_18dp.png",
  iconUrl = "www/turbine_icon_red.png",
  iconWidth = 12, iconHeight = 12
  # iconAnchorX = 0, iconAnchorY = 0
)

WGS84 <-  CRS("+init=epsg:4326")
WebMerc <- CRS("+init=epsg:3857")
# stn_grid <- NULL

cardinal_start_coords <- function(boundary, buff_dist_km){
  require(sf) 
  boundary_buff <- st_buffer(boundary, buff_dist_km*1000)
  boundary_buff_bbox <- st_bbox(boundary_buff)
  center_x_dist <- (boundary_buff_bbox[["xmax"]] - boundary_buff_bbox[["xmin"]])/2
  center_y_dist <- (boundary_buff_bbox[["ymax"]] - boundary_buff_bbox[["ymin"]])/2
  
  cardinals <- data.frame() #cardinals = c("SW", "S", "SE", "E", "NE", "N", "NW", "W"))
  cardinals<- rbind(cardinals, cbind(dir ="SW", x = boundary_buff_bbox[["xmin"]],y = boundary_buff_bbox[["ymin"]]))
  cardinals <- rbind(cardinals, cbind(dir ="S", x = boundary_buff_bbox[["xmin"]] + center_x_dist, y = boundary_buff_bbox[["ymin"]]))
  cardinals<- rbind(cardinals, cbind(dir ="SE", x = boundary_buff_bbox[["xmax"]], y = boundary_buff_bbox[["ymin"]]))
  cardinals <- rbind(cardinals, cbind(dir ="E", x = boundary_buff_bbox[["xmax"]], y = boundary_buff_bbox[["ymin"]] + center_y_dist))
  cardinals <- rbind(cardinals, cbind(dir ="NE", x = boundary_buff_bbox[["xmax"]], y = boundary_buff_bbox[["ymax"]]))
  cardinals <- rbind(cardinals, cbind(dir ="N", x = boundary_buff_bbox[["xmin"]] + center_x_dist, y = boundary_buff_bbox[["ymax"]]))
  cardinals <- rbind(cardinals, cbind(dir ="NW", x = boundary_buff_bbox[["xmin"]], y = boundary_buff_bbox[["ymax"]]))
  cardinals <- rbind(cardinals, cbind(dir ="W" ,x = boundary_buff_bbox[["xmin"]], y = boundary_buff_bbox[["ymin"]] + center_y_dist))
  cardinals <- cardinals %>% 
    mutate_at(c('x', 'y'), as.numeric)
  cardinals_sf <- st_as_sf(sp::SpatialPointsDataFrame(cbind(cardinals$x, cardinals$y),cardinals), crs = 3857) 
  return(cardinals_sf)
}

create_grid <- function(study, resolution_m){
  #create a regularly spaced grid of stations in webMercator
  require(rgeos)
  require(sp)
  require(SpatialPosition)

  study_area_WebMerc <- spTransform(study, CRSobj = WebMerc)
  grid_array <- SpatialPosition::CreateGrid(w = study_area_WebMerc, resolution = resolution_m, returnclass = "sf")  #creates a regularly spaced grid
  grid_array <- st_sf(grid_array, crs=WebMerc)
  # with rows corresponds to argument 1 (points) and 
  # columns to argument 2 (polygons)
  grid_array <- grid_array[which(st_intersects(st_as_sf(study_area_WebMerc), grid_array, sparse = FALSE)), ] 
  return(grid_array)
}


#create spatial polylines
points_to_line_sp <- function(data, x, y, id_field = NULL, sort_field = NULL, proj = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(x, y)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])),  names(paths[1]))), proj4string = proj)
    # Check if only one path if more than 1 then proceed
    if (length(paths) > 1){
      for (p in 2:length(paths)) {
        # id <- paste0("line", as.character(p))
        id <- names(paths[p])
        l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)), proj4string = proj)
        sp_lines <- spRbind(sp_lines, l)
      }}
    
    return(sp_lines)
  }
}

######################EXTRA####################
# BOEM_lease_area <- geojsonio::geojson_read("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/ArcGIS/rest/services/BOEM_Wind_Planning_and_Lease_Areas/FeatureServer/0", what = "sp")


# 
# # javascript code for extendShinyjs to highlight NAs in input fields
# jsCode <- '
# shinyjs.backgroundCol = function(params) {
# var defaultParams = {
# id : null,
# col : "red"
# };
# params = shinyjs.getParams(params, defaultParams);
# var el = $("#" + params.id);
# el.css("background-color", params.col);
# }'
# 
# NAsHighlightColor <- "#FDC0C0"


