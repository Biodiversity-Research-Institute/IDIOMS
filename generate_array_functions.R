
##################function(s) for optimizing antenna receiver performance############################
manual_coverage <- function(selected_stns_sp, study_area_sf, min_ht, max_ht, interval_m=50, stn_HT, tag_freq, ant_type, 
                            xi_min_dbm = NULL, num_ant = NULL, lambda = NULL, D0 = NULL, p0 = NULL, local_tz){ #updateProgress = NULL, 
  require(sp)
  
  ht_seq <- seq(min_ht, max_ht, interval_m)
  # selected_stns_at_multi_ht <- parallel::parLapply(clust, ht_seq, function(flt_ht) {
  i=1  
  n_iter=1
  loop_length <- length(ht_seq)*n_iter
  inc_amt <- 1/loop_length
  
  start_time <- Sys.time()

  if (is.null(local_tz)) {local_tz=""}
  study_area_sf <- st_transform(study_area_sf, crs=3857)
  
  # selected_stns_at_multi_ht_df <- data.frame()
  # for (flt_ht in ht_seq){
  
  station_array_at_multi_ht <- lapply(ht_seq, function(flt_ht) {
    
    if (grepl("omni",ant_type, fixed = T)){
      single_antenna_optim <- omni_antenna_detect_pattern(z = flt_ht, referenceIsotropicRange = 1000)
    } else {
      #create single antenna pattern depending on if 166 or 434 MHz
      if (tag_freq=="166 MHZ"){
        #166 MHz
        single_antenna_optim <- yagi_antenna_detect_pattern_166(z = flt_ht, ant_HT = stn_HT, xi_min_dbm = xi_min_dbm, lambda, D0, p0, maxit = 1000)
      } else { 
        #434 MHZ
        # browser()
        # single_antenna_optim <- antenna_detection_pattern_434(z = flt_ht, r_2 = 1500, max_range = 15000)
        # single_antenna_optim <- antenna_detection_pattern_434_horizon(z = flt_ht, r_2 = 1500, stn_height = stn_HT)
        single_antenna_optim <- antenna_detection_pattern_434_horizon_FSPL(z = flt_ht, stn_height=stn_HT, transmitter_gain = 2, receiver_gain = D0)
      }
    } #grepl
    
    max_r <- st_bbox(single_antenna_optim)[["xmax"]] 
    
    WebMerc <- CRS("+init=epsg:3857")
    selected_stns_sp <- spTransform(selected_stns_sp, CRSobj = WebMerc)
    
    #build antenna stations based on angle set 
    #step through each station to generate different pattern and combine
    # angle_set = c(0, 90, 180, 270)
    stn_list <- list()
    for (j in 1:nrow(selected_stns_sp)){
      stn <- selected_stns_sp[j, ]
      ID <- as.character(stn[["ID"]])
      # ant_starting_angle <- stn[["ant_angle_start"]]
      ant_starting_angle <- unlist(stn[["ant_angle_list"]])[[1]]
      
      if (grepl("omni",ant_type, fixed = T)){
        #if omni don't generate multiple antenna arrangements
        #affine shift to location of antenna
        # browser()
        single_antenna_optim_shifted <- single_antenna_optim + stn@coords[,1:2]
        stn_list[[ID]] <- st_as_sf(data.frame("station" = ID, "theta"=0, "geometry"=single_antenna_optim_shifted), crs=3857)
      } else {
        #cycle through all selected stations to build antenna patterns at each station and height combo
        stn_list[[ID]] <- suppressWarnings(multi_antenna_pattern(ant_stations = stn, stn_id = ID, ant_angle_st = ant_starting_angle, 
                                antenna_num = num_ant, single_antenna_detect_poly = single_antenna_optim))
      }
    } 
    
    station_array_set <- data.table::rbindlist(stn_list)
    #added below to create regular df from lists
    station_array_set$station <- unlist(station_array_set$station)
    station_array_set$theta <- unlist(station_array_set$theta)

    #determine coverage of study area
    study_area_covered <- as.numeric(st_area(st_intersection(st_union(station_array_set$geometry),study_area_sf))/st_area(study_area_sf))
    antenna_coverage_overlap <- 1-as.numeric(sum(st_area(st_union(station_array_set$geometry)))/sum(st_area(station_array_set$geometry)))
    station_array_set_df <- data.frame(flt_ht = flt_ht)
    station_array_set_df$max_r <- max_r
    station_array_set_df$stns <- station_array_set$ID
    station_array_set_df$study_area_covered <- study_area_covered
    station_array_set_df$antenna_coverage_overlap <- antenna_coverage_overlap
    station_array_set_df$angle_df <- list(station_array_set)
    station_array_set_df$angles <- paste(unlist(station_array_set[,c("station", "theta")]), collapse=",")
    
    prog_txt <- paste0("completed ", flt_ht, " m step ", i, " of ", loop_length, " flight heights, at: ", 
                       strftime(Sys.time(), tz=local_tz), ", elapsed time: ", round(as.numeric(difftime(Sys.time(), start_time, units = "min")), 1), " minutes")
    # updateProgress(value = i, detail = prog_txt)
    incProgress(amount = inc_amt, detail = prog_txt)
    i <<- i + 1
    return(station_array_set_df)
    })
  
    station_array_at_multi_ht_df <- data.table::rbindlist(station_array_at_multi_ht)
    return(station_array_at_multi_ht_df)
  }

# grid_expand <- function(ref_area, maxr, buff_prop) {
# 
#   #Try buffering based on max detection distance instead of just increment based on size of area.
#   
#   buff <- maxr*buff_prop
#   #expand ref buffer distance as needed to optimize
#   
#   ref_area <<- sf::st_buffer(ref_area, dist=buff)
#   grid <- create_grid(sf::as_Spatial(ref_area), 1000)
#   #get reference grid for optimization routine
#   grid_WGS84 <- st_transform(grid, WGS84)
#   
#   #need to get lat/longs so transform to WGS84
#   grid_df <<- cbind(grid_WGS84[,c("ID")], st_coordinates(grid_WGS84)) %>% st_set_geometry(NULL)
#   colnames(grid_df) <- c("ID", "long", "lat")
#   # debug.out$debug <- renderPrint(str(grid_sf))
#   # sf::write_sf(grid_sf, paste0("P:/NYSERDA Nanotag tools/nanotag_guidance_Shiny_tool/auto_radio_telemetry_Shiny_tool/grid_sf_", flt_ht,".shp"))
#   return(grid_df)
# }

#optimize stations to maximize antenna angles and coverage area
optimize_study_area_covg <- function(study_area_sf, min_ht, max_ht, interval_m=50, stn_HT, xi_min_dbm, all_stns_df, grid_df, grid_sf, tag_freq,
                                     ant_type, n_stations, num_ant, lambda, D0, p0, local_tz, max_rbuff_prop){ #updateProgress = NULL, , debug.out
  require(sp)
  ht_seq <- seq(min_ht, max_ht, interval_m)
  # selected_stns_at_multi_ht <- parallel::parLapply(clust, ht_seq, function(flt_ht) {
  i=1  
  n_iter=1
  loop_length <- length(ht_seq)*n_iter
  inc_amt <- 1/loop_length
  start_time <- Sys.time()
  ref_area_sf_covered <- 0 #initialize
  #add below to help with CRS issues in shinyapps.io
  study_area_sf <- st_transform(study_area_sf, crs=3857)
  ref_area_sf <- study_area_sf  #initialize
  
  if (is.null(local_tz)) {local_tz=""}
    
  if (tag_freq=="434 MHZ"){
    #need to expand for 434 for initial estimation even at low altitudes, clumping
    # grid_df <- grid_expand(ref_area = ref_area_sf, maxr = 30000, buff_prop = 0.33)
    #Try buffering based on max detection distance instead of just increment based on size of area.
    buff_dist <- 15000
    #expand ref buffer distance as needed to optimize

    ref_area_sf <- sf::st_buffer(ref_area_sf, dist=buff_dist)
    grid_sf <- create_grid(sf::as_Spatial(ref_area_sf), 500)
    #get reference grid for optimization routine
    grid_sf_WGS84 <- st_transform(grid_sf, WGS84)

    #need to get lat/longs so transform to WGS84
    grid_df <- cbind(grid_sf_WGS84[,c("ID")], st_coordinates(grid_sf_WGS84)) %>%  st_set_geometry(NULL)
    colnames(grid_df) <- c("ID", "long", "lat")
  }
  # stn <- selected_stns_sp[j, ]
  # ID <- as.character(stn[["ID"]])
  
  selected_stns_at_multi_ht_df <- data.frame()
  # selected_stns_at_multi_ht <- lapply(ht_seq, function(flt_ht) {
  for (flt_ht in ht_seq){
    if (grepl("omni",ant_type, fixed = T)){
      #if omni don't generate multiple antenna arrangements
      #affine shift to location of antenna
      # browser()
      # single_antenna_optim_shifted <- single_antenna_optim + stn@coords[,1:2]
      # stn_list[[ID]] <- st_as_sf(data.frame("station" = ID, "theta"=0, "geometry"= single_antenna_optim_shifted), crs=3857)
      single_antenna_optim <- omni_antenna_detect_pattern(z = flt_ht, referenceIsotropicRange = 1000)
      
    } else {
      #create single antenna pattern depending on if 166 or 434 MHz
      if (tag_freq=="166 MHZ"){
        #166 MHz
        single_antenna_optim <- yagi_antenna_detect_pattern_166(z = flt_ht, ant_HT = stn_HT, xi_min_dbm = xi_min_dbm, lambda, D0, p0, maxit = 1000)

      } else { 
        #434 MHz
        # single_antenna_optim <- antenna_detection_pattern_434(z = flt_ht, r_2 = 1500, max_range = 15000)
        single_antenna_optim <- antenna_detection_pattern_434_horizon_FSPL(z = flt_ht, stn_height=stn_HT, transmitter_gain = 2, receiver_gain = D0)
      }
    }
    max_r <- st_bbox(single_antenna_optim)[["xmax"]]
    
    # stn_placement_maxcovr_dist_mat_grid_beams <- max_coverage_motustag(proposed_facility = all_stns_df,
    #                                                                   user = grid_df,
    #                                                                   n_added = n_stations,
    #                                                                   distance_cutoff = max_r*0.75)
    
    stn_placement_maxcovr_dist_mat_grid_beams <- max_coverage_motustag(proposed_facility = all_stns_df,
                                                                       user = grid_df,
                                                                       n_added = n_stations,
                                                                       distance_cutoff = max_r ) #, solver = "lpSolve")

    stn_placement_maxcovr_grid_detect <- SpatialPointsDataFrame(coords=stn_placement_maxcovr_dist_mat_grid_beams$facility_selected[[1]][,c("long", "lat")], data = stn_placement_maxcovr_dist_mat_grid_beams$facility_selected[[1]], proj4string=WGS84)

    all_proposed_stns_grid_detect_WebMerc <- spTransform(stn_placement_maxcovr_grid_detect, CRSobj = WebMerc)
    # debug.out$debug <- renderPrint(str(all_proposed_stns_grid_detect_WebMerc))
    
    colnames(all_proposed_stns_grid_detect_WebMerc@data) <- c("ID", "x", "y")  #set to lat long, needs x, y
    #optimize angles at flight height
    # optimized_stn_angles_internal <- antenna_angle_optim_effecient(proposed_stn_points = all_proposed_stns_grid_detect_WebMerc, n_antennas = num_ant, ant_ang_inc = 15,
    #                                                                single_antenna_optim = single_antenna_optim, detect_locs = all_stns_sf)

    optimized_stn_angles_internal <- suppressWarnings(antenna_angle_optim_effecient(proposed_stn_points = all_proposed_stns_grid_detect_WebMerc, n_antennas = num_ant, ant_ang_inc = 15,
                                                                   single_antenna_optim = single_antenna_optim, detect_locs = grid_sf, debug.out=debug.out))

    #determine coverage of study area
    study_area_covered <- as.numeric(st_area(st_intersection(st_union(optimized_stn_angles_internal$geometry),study_area_sf))/st_area(study_area_sf))
    ref_area_sf_covered <- as.numeric(st_area(st_intersection(st_union(optimized_stn_angles_internal$geometry),ref_area_sf))/st_area(ref_area_sf))
    
    #in optimization scheme, stations being pushed together at higher altitudes as detection ranges get bigger. 
    #Increase the area of optimization as height get higher if coverage gets >80%. 
    #Suggest buffering study area by 10% each time you approach 100% coverage - this may need to be fine-tuned
    #tried 75% covg, 20% buf and 75/10, seems 80/10 a good compromise
    #With addition of 434 - use lower cuttof of 50%
    print(paste("grid_sf num pts: ", nrow(grid_sf)))
    print(paste("ref_area_sf_covered: ",ref_area_sf_covered))
    # browser()
    
    buff_dist=0
    if (ref_area_sf_covered > 0.5) { #expand if only 50% detected or less
      #Try buffering based on max detection distance instead of just increment based on size of area.
      buff_dist <- max_r*max_rbuff_prop
      #expand ref buffer distance as needed to optimize

      ref_area_sf <- sf::st_buffer(ref_area_sf, dist=buff_dist)
      grid_sf <- create_grid(sf::as_Spatial(ref_area_sf), 500)
      #get reference grid for optimization routine
      grid_sf_WGS84 <- st_transform(grid_sf, WGS84)

      #need to get lat/longs so transform to WGS84
      grid_df <- cbind(grid_sf_WGS84[,c("ID")], st_coordinates(grid_sf_WGS84)) %>%  st_set_geometry(NULL)
      colnames(grid_df) <- c("ID", "long", "lat")
      print(paste("grid_expanded for: ", flt_ht))
      # grid_df <- grid_expand(ref_area = ref_area_sf, maxr = max_r, buff_prop = max_rbuff_prop)
      
    }

    antenna_coverage_overlap <- 1-as.numeric(sum(st_area(st_union(optimized_stn_angles_internal$geometry)))/sum(st_area(optimized_stn_angles_internal$geometry)))
    selected_locs <- data.frame(flt_ht = flt_ht)
    # selected_locs$max_r_detect <- max_r_detect
    selected_locs$max_r <- max_r
    selected_locs$stns <- list(stn_placement_maxcovr_dist_mat_grid_beams$facility_selected[[1]])
    # selected_locs$flt_ht=rep(flt_ht, n_stations)
    selected_locs$study_area_covered <- study_area_covered
    selected_locs$ref_area_covered <- ref_area_sf_covered
    # selected_locs$buff_dist <- buff_dist
    selected_locs$antenna_coverage_overlap <- antenna_coverage_overlap
    # selected_locs$study_area_covered=rep(study_area_covered, n_stations)
    selected_locs$angle_df <- list(optimized_stn_angles_internal)
    selected_locs$angles <- paste(unlist(optimized_stn_angles_internal[,c("station", "theta")]), collapse=",")
    
    #", round(i/loop_length*100, 1), "% complete 
    prog_txt <- paste0("completed ", flt_ht, " m step ", i, " of ", loop_length, " flight heights, at: ", 
                 strftime(Sys.time(), tz=local_tz), ", elapsed time: ", round(as.numeric(difftime(Sys.time(), start_time, units = "min")), 1), " minutes")
    # updateProgress(value = i, detail = prog_txt)
    incProgress(amount = inc_amt, detail = prog_txt)
    
    # i <<- i + 1
    i <- i + 1
    
    selected_stns_at_multi_ht_df <- rbind(selected_stns_at_multi_ht_df, selected_locs)
    
    #clean up memory
    rm(single_antenna_optim)
    rm(all_proposed_stns_grid_detect_WebMerc)
    rm(stn_placement_maxcovr_grid_detect)
    rm(stn_placement_maxcovr_dist_mat_grid_beams)
    rm(optimized_stn_angles_internal)
    rm(selected_locs)
    # gc()
    # return(selected_locs)
  } 
  #)
  # selected_stns_at_multi_ht_df <- data.table::rbindlist(selected_stns_at_multi_ht)
  return(selected_stns_at_multi_ht_df)
 }
  
  