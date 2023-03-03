#create spatial polylines
points_to_line_sp <- function(data, x, y, id_field = NULL, sort_field = NULL, proj = NULL) {
  require(sp)
  require(maptools)
  
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

WGS84 <-  CRS("+init=epsg:4326")
WebMerc <- CRS("+init=epsg:3857")

#load modeleded movement data for shorebirds and convert to spatial lines
# load("P:/NYSERDA Nanotag tools/Bird movement simulation/shorebird_tracks.Rdata")

shorebird_points_sp <- SpatialPointsDataFrame(coords = cbind(shorebird.tracks$x, shorebird.tracks$y), data = shorebird.tracks, proj4string = WebMerc)
shorebird_points_sf <- st_as_sf(shorebird_points_sp)
save(shorebird_points_sf, file = "data/shorebird_points_sf.Rdata")

shorebird_lines_sp <- points_to_line_sp(shorebird.tracks, x = "x", y = "y",id_field = "id", proj = WebMerc)
shorebird_lines_sf <- st_as_sf(shorebird_lines_sp)
shorebird_lines_sf$ID <- 1:nrow(shorebird_lines_sf)
st_geometry(shorebird_lines_sf) <- shorebird_lines_sf$geometry

save(shorebird_lines_sf, file = "data/shorebird_lines_sf.Rdata")

#load modeleded movement data for seabirds and convert to spatial lines

# load("P:/NYSERDA Nanotag tools/Bird movement simulation/seabird_tracks.Rdata")
seabird_points_sp <- SpatialPointsDataFrame(coords = cbind(simDat$x, simDat$y), data = as.data.frame(simDat)[,c("x", "y", "ID")], proj4string = WebMerc)
seabird_points_sf <- st_as_sf(seabird_points_sp)
save(seabird_points_sf, file = "data/seabird_points_sf.Rdata")

seabird_lines_sp <- points_to_line_sp(as.data.frame(simDat)[,c("x", "y", "ID")], x = "x", y = "y",id_field = "ID", proj = WebMerc)
seabird_lines_sf <- st_as_sf(seabird_lines_sp)
seabird_lines_sf$ID <- 1:nrow(seabird_lines_sf)
st_geometry(seabird_lines_sf) <- seabird_lines_sf$geometry
save(seabird_lines_sf, file = "data/seabird_lines_sf.Rdata")

#Trial bootstrap mean detection functions
# 
# load(file = "data/seabird_points_sf.Rdata")
# load(file = "P:/NYSERDA Nanotag tools/motus_tag_guidance_Shiny_tool/not uploaded to Shiny server/selected_stn_data_angles.RData")
# data_sf <-  st_sf(data_sf, crs = 3857)
# antenna_pattern_25 <-data_sf[which(data_sf$flt_ht==25),]
# 
# test_mean_detect <- boot_mean_detection_CI(antenna_pattern_25, seabird_lines_sf, n_birds = 20, R = 100)

