
##########Generic radio telemetry functions#############################

watts_to_dbm <- function(power_watts){
  return(10*log10(power_watts * 1000))
}

dbm_to_watts <- function(power_dbm){
  return((10^(power_dbm/10))/1000)
}

dbm_to_mw <- function(power_dbm){
  return(10^(power_dbm/10))
}


antenna_angle_seq <- function(input_angle, num_antennas){
  ant_ang_inc <- 360/num_antennas
  start_angle <- input_angle %% ant_ang_inc
  return(seq(from = start_angle, by = ant_ang_inc, length.out = num_antennas))
}


psi_calc <- function(x, y, XT, YT, theta){
  # function to calculate psi for use in antenna strength modeling
  require(pracma)
  degree <- atan2d((y-YT),(x-XT)) # the degrees of the x,y point
  if (degree < 0) {degree <- degree+360}
  psi <- degree - theta  #subtract the antenna angle (theta)
  return(deg2rad(psi)) # radians
}

earth_radius_m = 6371000

dist_to_horizon_at_z_m <- function(stn_height, z) sqrt(2 * earth_radius_m * stn_height + stn_height^2) + sqrt(2 * earth_radius_m * z + z^2)


detection_limit_distance <- function(z_in, ant_HT, min_xi_dbm, x_max_end = 10000, inc = 10000, seq_inc = 10) { #, x_min_end = -10000
  #determine the distance necessary to estimate along the main antenna lobe 
  #problem is nodes of low antenna strength I need to get across and also issues with rear lobes being at angles extending backwards past 0 axis
  #just calc main lobe at front since this is better behaved. 
  #z is bird height
  #ant_X, ant_Y, ant_HT, ant_theta are antenna params
  #min_xi_dbm is min power for detection
  #x max direction
  x_max_start = 0
  continue_proc = T
  while(continue_proc){
    power_vals <- data.frame()
    for (x_val in seq(x_max_start,x_max_end, seq_inc)){
      power_dbm = xi_square_calc_dbm(x=x_val, y=0, z=z_in, XT=0, YT=0, HT=ant_HT, theta=0)
      power_vals <- rbind(power_vals, cbind("x_val"=x_val,"power_dbm"=power_dbm))
    }
    # print(power_vals)
    x_max_detect_vals <- power_vals[which(power_vals$power_dbm >= min_xi_dbm), ]
    x_max_detect <- max(x_max_detect_vals$x_val) #determine max dist detected
    # print(x_max_detect)
    if (x_max_detect < x_max_end) {
      continue_proc = F
    } else {
      x_max_start <- x_max_end  #reset start
      x_max_end <- x_max_end + inc  #increment end in case we haven't reached max
    }
  }
}


rotation = function(a){
  r = a * pi / 180 #degrees to radians
  # matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
  #rotation about axis believe require inverse
  matrix(c(cos(r), -sin(r), sin(r), cos(r)), nrow = 2, ncol = 2)  
  
} 

multi_antenna_pattern <- function(ant_stations, stn_id, ant_angle_st=60, antenna_num, ant_X=0, ant_Y=0, ant_HT, z, min_xi,interval=100, 
                                  single_antenna_detect_poly=NULL, crs_locs = 3857, crs_out=NULL, noisy = F){
  #generate multi-antenna detection pattern for mapping based on single antenna estimate, rotated and shifted from x,y=0,0 and theta=0

  # WebMerc <- CRS("+init=epsg:3857")
  
  # #if single antenna detection poly not passed to function, create the poly pattern
  # if(is.null(single_antenna_detect_poly)){
  #   # min_max_lims <- detection_limit_distance(z_in=z, ant_HT=ant_HT, min_xi_dbm=min_xi, x_max_end = 75000, x_min_end = -15000, inc = 10000, seq_inc = 50)
  #   xmax <- detection_limit_distance(z_in=z, ant_HT=ant_HT, min_xi_dbm=min_xi, x_max_end = 75000, inc = 10000, seq_inc = 50)
  #   #calc single antenna pattern to replicate to all other positions
  #   single_antenna_detect_poly <- antenna_detect_patterns(x_min=-1*xmax, x_max=xmax, y_min=-1*xmax, y_max=1*xmax, z=z, interval=100, 
  #                                                         ant_X=ant_X, ant_Y=ant_Y, ant_HT=ant_HT, ant_theta=0, min_xi = min_xi, crs_locs = 3857, noisy = noisy)}
  #generate angles from start angle
  antenna_angles <- seq(ant_angle_st, 360, 360/antenna_num)
  # ant_stations <- spTransform(ant_stations, CRSobj = WebMerc)
  antenna_array_detect_polys <- data.frame()
  for (i in seq(1,length(ant_stations),1)) {
    #get each antenna station
    antenna <- ant_stations[i,]
    # lat <- antenna[["lat"]]
    # long <- antenna[["lat"]]
    #cycle through each location and antenna placement
    # antenna_x <- antenna@coords[1]
    # antenna_y <- antenna@coords[2]
    
    for (j in (1:antenna_num)) {
      theta <- antenna_angles[j]
      #rotate about theta
      single_antenna_rotated <- single_antenna_detect_poly * rotation(theta)
      
      #affine shift to location of antenna
      single_antenna_rotated <- single_antenna_rotated + antenna@coords[,1:2]
      antenna_array_detect_polys <- rbind(antenna_array_detect_polys, cbind("station" = stn_id, "theta"=theta, "geometry"=single_antenna_rotated))
      
      # antenna_array_detect_polys <- rbind(antenna_array_detect_polys, cbind("station" = stn_id, "lat" = lat, "long" = long, "theta"=theta, "geometry"=single_antenna_rotated))
    }
  }

  multiple_antennas_detect_polys <- st_sf(antenna_array_detect_polys, crs=crs_locs)
  if(!is.null(crs_out)){multiple_antennas_detect_polys <- st_transform(multiple_antennas_detect_polys, crs=4326)}
  
  #clean up
  rm(antenna_array_detect_polys)
  rm(single_antenna_rotated)
  # gc()
  
  return(multiple_antennas_detect_polys) 
  
}

antenna_angle_optim_effecient <- function(proposed_stn_points, n_antennas, ant_ang_inc, single_antenna_optim, detect_locs, debug.out){
  require(sf)
  require(data.table)
  require(nngeo)
  require(dplyr)
  # 1) determine nearest station pairs
  # 2) determine angles of least overlap for adjacent stations
  # 3) Combine all min. overlap station to find optimal solution
  # 4) look at angles +/- an angle increment to find best
  
  n_stations <- length(proposed_stn_points)
  proposed_stn_points_sf <- st_transform(st_as_sf(proposed_stn_points), WebMerc)
  #get closest 3 points (k=3) if num of stations >= 3 or 2 if only 2 to optimize
  num_nn = min(c(nrow(proposed_stn_points_sf), 3))
  # print(paste("K:", num_nn))
  near_pts <- nngeo::st_nn(proposed_stn_points_sf, proposed_stn_points_sf, k=num_nn, returnDist = T, progress=F)
  
  if (num_nn >= 3){
    near_pts_idx <- rbindlist(lapply(near_pts$nn, function(x) as.data.table(cbind(x[2:3])))) #get index of the second/third closest (first is itself)
    near_pts_dist <- rbindlist(lapply(near_pts$dist, function(x) as.data.table(cbind(x[2:3])))) #get distance of the second/third closest (first is itself)
  } else{
    # browser()
    near_pts_idx <- rbindlist(lapply(near_pts$nn, function(x) as.data.table(cbind(x[2])))) #get index of the second/third closest (first is itself)
    near_pts_dist <- rbindlist(lapply(near_pts$dist, function(x) as.data.table(cbind(x[2])))) #get distance of the second/third closest (first is itself)
  }

  point_pairs <- data.frame(stn1=sort(rep(1:n_stations, 2)))
  point_pairs <- cbind(point_pairs, near_pts_idx)
  point_pairs <- cbind(point_pairs, near_pts_dist)
  colnames(point_pairs) <- c("stn1","stn2","stn1_2_dist")
  point_pairs$pairs <- apply(point_pairs, 1, function(x) paste(sort(c(x[["stn1"]], x[["stn2"]])), collapse = ","))
  
  #Since pairs of points - can remove half because closest pair already found
  point_pairs_no_dups <- point_pairs[!duplicated(point_pairs[,c("pairs", "stn1_2_dist")]),] #remove dups
  point_pairs_no_dups <- point_pairs_no_dups[order(point_pairs_no_dups$stn1_2_dist),]
  #select top n stations -1 for comparison
  # point_pairs_optim <-  point_pairs_no_dups[1:(n_stations-1),]
  point_pairs_optim <-  point_pairs_no_dups[1:n_stations,]
  
  # st_area(st_intersection())
  ant_angle_seq <- seq(0,floor(360/n_antennas), ant_ang_inc)
  n_ant_ang_seq <- length(ant_angle_seq)
  
  #get set of all possible antenna angle permutations to test between pairs of stations
  ant_perms <- as.data.frame(gtools::permutations(n_ant_ang_seq, 2, ant_angle_seq, ant_ang_inc, repeats.allowed = T))

  ###JUST CHANGED BELOW on 11.2.20 due to different CRS error only in shinyapps.io
  points_to_detect <- st_transform(detect_locs, crs=3857)
  # points_to_detect <- detect_locs
  
  #cycle through list of proposed station pairs to get best coverage for pairs
  station_pair_list=list()
  for (i in 1:nrow(point_pairs_optim)) {
    max_detected = 0
    stn1_val <- point_pairs_optim[i, ]$stn1
    stn2_val <- point_pairs_optim[i, ]$stn2
    pt1 <- proposed_stn_points[stn1_val,]
    pt2 <- proposed_stn_points[stn2_val,]
    close_stns <- maptools::spRbind(pt1, pt2)

    for (k in 1:nrow(ant_perms)){ 
      angle_set <- ant_perms[k,]  
      #build antenna stations based on angle set and determine how many point covered
      #step through each station to generate different pattern and combine
      station_set <- list()
      for (j in 1:ncol(angle_set)){
        ant_angle_j <- angle_set[,j] 
        stn <- close_stns[j,]
        if (j==1) {station_set[[paste0("stn_", j)]]$station <- stn1_val}
        else {station_set[[paste0("stn_", j)]]$station <- stn2_val}
        station_set[[paste0("stn_", j)]] <- multi_antenna_pattern(ant_stations = stn, stn_id = station_set[[paste0("stn_", j)]]$station,ant_angle_st = ant_angle_j, 
                                                                  antenna_num = n_antennas, single_antenna_detect_poly = single_antenna_optim)

      }

      station_set <- data.table::rbindlist(station_set)

      #combine polys to for intersection of one surface, otherwise given more points than possible because intersection by each poly
      detected_points <- sum(st_intersects(st_union(station_set$geometry), points_to_detect, sparse = F)) #total number of detected points for this array

      if (max_detected < detected_points) {
        max_detected <- detected_points
        max_angles <- station_set
      }
      
      # detected_points_sum <- sum(detected_points)  #total number of detected points for this array

    }
    # return(build_stations)
    station_pair_list[[i]] <- list("max_detected"=max_detected, "max_angles"=max_angles)
  }
  # station_pair_list_combine <- data.frame()
  station_pair_list_combine <- lapply(station_pair_list, function(x) {
    tbl <- as.data.frame(x$max_angles)
    # station_pair_list_combine <- rbind(station_pair_list_combine, tbl)
  })
  
  station_pair_list_combine <- data.table::rbindlist(station_pair_list_combine)
  station_pair_list_combine$station <- unlist(station_pair_list_combine$station)
  station_pair_list_combine$theta <- unlist(station_pair_list_combine$theta)
  
  #remove full dups first
    station_pair_list_combine <- unique(station_pair_list_combine, by=c("station", "theta"))
  station_pair_list_combine$id <- unlist(lapply(1:(nrow(station_pair_list_combine)/n_antennas), function(x) rep(x, n_antennas)))
  #determine which stations are duplicate and use if same or use mean (rounded to nearest 15) for angles
  
  #generates optimal list of start angles from which we can get pattern
  stn_dups <- station_pair_list_combine %>% 
    dplyr::group_by(station, id) %>% 
    dplyr::summarise(theta=min(theta)) %>% 
    dplyr::group_by(station) %>%
    dplyr::summarise(theta=DescTools::RoundTo(mean(theta), 15)) %>% #round to 15 multiple 
    dplyr::mutate(theta_min=theta-15, theta_plus=theta+15) 
  
  #generate combos of angles
  stn_dups_list <- as.list(as.data.frame(t(stn_dups[,2:ncol(stn_dups)])))
  stn_dups_exp <- expand.grid(stn_dups_list)
  
  max_detected = 0
  for (i in 1:nrow(stn_dups_exp)) {
    angle_opts <- stn_dups_exp[i,]
    #build antenna stations based on angle set and determine how many point covered
    #step through each station to generate different pattern and combine
    station_set <- list()
    for (j in 1:ncol(angle_opts)){
      ant_angle_j <- angle_opts[,j] 
      stn <- proposed_stn_points[j, ]
      station_set[[paste0("stn_", j)]] <- multi_antenna_pattern(ant_stations = stn, stn_id = station_set[[paste0("stn_", j)]]$station, ant_angle_st = ant_angle_j, 
                                                                antenna_num = n_antennas, single_antenna_detect_poly = single_antenna_optim)
      station_set[[paste0("stn_", j)]]$station <- j
    }
    
    # points_to_detect
    
    station_set <- data.table::rbindlist(station_set)

    #combine polys to for intersection of one surface, otherwise given more points than possible because intersection by each poly
    detected_points <- sum(st_intersects(st_union(station_set$geometry), points_to_detect, sparse = F)) #total number of detected points for this array

    #look for max detected of points to find optimal arrangement record best set if more detected
    if (max_detected < detected_points) {
      max_detected <- detected_points
      ant_optimized_stn_polys_sf <- station_set
    }
  }
  #added below to create regular df from lists
  ant_optimized_stn_polys_sf$station <- unlist(ant_optimized_stn_polys_sf$station)
  ant_optimized_stn_polys_sf$theta <- unlist(ant_optimized_stn_polys_sf$theta)
  # ant_optimized_stn_polys_sf <- sf::st_sf(ant_optimized_stn_polys_sf)
  
  #clean up
  rm(proposed_stn_points_sf)
  rm(near_pts)
  rm(point_pairs)
  rm(station_pair_list)
  rm(station_pair_list_combine)
  rm(detected_points)
  rm(station_set)
  rm(stn_dups)
  rm(stn_dups_list)
  rm(stn_dups_exp)
  rm(point_pairs_optim)
  # gc()
  
  return(ant_optimized_stn_polys_sf)
}

boot.mean = function(x, indices) {
  return( mean( x[indices] ) )
}


#Function for calculating the bootstrapped mean detections and CI for antennas detecting simulated birds passage
boot_mean_detection_CI <- function(flt_ht, antenna_pattern, simulated_birds, n_birds, R){
  #antenna_pattern: the pattern of attenna beams for the study
  #simulated_birds: simulated bird tracks to sample from
  #n_birds: Number of birds to sample at a times
  #R: number of replicates
  #subset data for each flight height
  withProgress(message = "Simulated bird detection processing", min = 0,  max = 1, {
    # Create a Progress object
    #generate detection coverage percentage over all simulated 
    start_time <- Sys.time()
    
    resample_detection <- sapply(1:R, function(x) {
      #sample n_birds and calculate proportion detected in a single sample
      bird_sample <- st_as_sf(simulated_birds[sample(1:nrow(simulated_birds), size = n_birds, replace = T), ])
      #combine antenna pattern into single surface for determing how many lines intersect
      prop_detected <- sum(sf::st_intersects(bird_sample, st_union(antenna_pattern), sparse = F), na.rm = T)/n_birds
      if (x %% (R/10) == 0){
        # browser()
        perc_complete <- round((x/R) * 100, 0)
        incProgress(amount = 0.1, detail = paste0(": Processing ", flt_ht, "m flight height; completed ", perc_complete, "% of ", R," replicates at: ", 
                           strftime(Sys.time()), ", elapsed time: ", round(as.numeric(difftime(Sys.time(), start_time, units = "min")), 1), " minutes"))
        # updateProgress(value = perc_complete, detail = prog_txt)
      }
      return(prop_detected)
    })
  }) #withprogress
  #Calc bootstrap mean and CI for proportions
  #https://www.r-bloggers.com/2021/11/calculate-confidence-intervals-in-r/
  mean_prop <- mean(resample_detection)
  margin <- qnorm(0.975)*sqrt(mean_prop*(1-mean_prop)/R)
  CI_lower<- mean_prop - margin
  CI_upper <- mean_prop + margin
  
  return(data.frame(flt_ht=flt_ht, mean_prop=round(mean_prop, 3), CI_lower=round(CI_lower, 3), CI_upper=round(CI_upper, 3)))
}
  
  
# #Calc bootstrap CI for each species season combo using the aliquot and survey as the bootstrapped values
# APEM_seasonal_boot_mean_CI <-  APEM_spp_density_by_survey_aliq %>% 
#   group_by(effort_season,in_out_lease, taxa_group, common_name) %>% 
#   summarise(APEM_mean_spp_density=round(boot(data=spp_density, boot.mean, R=10000)$t0, 3), 
#             APEM_lower_CI=round(boot.ci(boot(data=spp_density, boot.mean, R=10000), type="perc")$percent[4], 3), 
#             APEM_upper_CI=round(boot.ci(boot(data=spp_density, boot.mean, R=10000), type="perc")$percent[5], 3)) %>% 
#   as.data.frame()

  
  
  
  
  
  
  
  
