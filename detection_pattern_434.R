#################Functions for calculating the detection patterns for Lifetags at 434 MHz#########################


# library(lattice)
# library(dplyr)
# library(ggplot2)

antenna_detection_pattern_434_horizon_FSPL <- function(z, stn_height=25, transmitter_gain = 2, receiver_gain){
  # Update code from Erik Carlson to deal with Horizon and Free Space path Loss issues
  # Per email from EC on 15 Mar 2022
  # z is bird height in meters
  # stn_height is height of antenna station in meters
  # transmitter_gain = 2 #transmitter gain in dbi, this is direct from CTTs FCC filing
  # receiver_gain = 12 #reciever gain in dbi, this is direct from our conversations with the antenna developer
  
  require(sp)
  library(sf)
  
  
  #radius of the Earth in meters
  R_earth = 6371000 #meters
  # stn_height = 0.1 #height of station in kilometers (100 meters)
  # z = 3 #height of bird in kilometers ( 3000 meters)
  
  #galilean transformation to find the distance to the horizon in the birds frame of reference
  if (z <= stn_height) {
    g_transform_height = stn_height
  }else if (z > stn_height) {
    g_transform_height = stn_height + z
  }
  
  max_g_distance_to_horizon = sqrt(2 * R_earth * g_transform_height) #projected distance to horizon in km, this is approximately the same as the radial distance as the size of the earth is much lareceiver_gainer than the height of any birds
  f = 434 *10^6 #frequency of the antenna
  c = 299792458 #speed of light
  
  #theoretical free space path distance losses
  FSPL1 = 100 #cut off at -110 dbm from the antenna

  #calculate the distance in which the free space loss is 100dbm to find the max range of the antenna
  free_space_path_range_drop_off_dist = exp((1/20)*(FSPL1*log(10) + receiver_gain*log(10) + transmitter_gain*log(10) -20*log(f)-20*log(4*pi/c)))
  free_space_path_range_drop_off_dist = free_space_path_range_drop_off_dist  #in meters - original code in KM from EC
  
  #making sure the horizon does not get in the way
  if (free_space_path_range_drop_off_dist <= max_g_distance_to_horizon) {
    abs_max_detection_range = free_space_path_range_drop_off_dist
  }else if (free_space_path_range_drop_off_dist > max_g_distance_to_horizon) {
    abs_max_detection_range = max_g_distance_to_horizon
  }
  
  #We use kilometers here but there is nothing stopping you from using any unit of distance you so please. 
  max_range = abs_max_detection_range
  scaling_factor = abs_max_detection_range/(2*pi)  #max range of the antenna detection scaled to pi
  
  t <- seq(0, pi, length.out=50); 
  r <- scaling_factor*(2.092 +0.7284 * cos(2.004* t) + 0.4391 * cos(4.008 * t)  - 0.4641*sin(2.004* t) + 0.199352* sin(4.008* t)) 
  
  detect_pattern_434 <- data.frame(xt = c(seq(0, pi, 0.01), seq(pi, 0, -0.01)))
  detect_pattern_434$id <- 1
  
  detect_pattern_434$yt <- rep(seq(0, pi, 0.01), 2)
  
  #per Erik Carlson on 8 Feb 2022: antenna is actually placed at (-7.5,0) so you need to shift your center point ever so slightly (see the red arrow to black dot). 
  x_shift <- max_range/2 
  # browser()
  x_points <- function(xt) scaling_factor * (2.092 + 0.7284 * cos(2.004 * xt) + 0.4391 * cos(4.008 * xt)  - 0.4641 * sin(2.004 * xt) + 0.199352 * sin(4.008 * xt)) * cos(xt)
  y_points <- function(yt) scaling_factor * (2.092 + 0.7284 * cos(2.004 * yt) + 0.4391 * cos(4.008 * yt)  - 0.4641 * sin(2.004 * yt) + 0.199352 * sin(4.008 * yt)) * sin(yt)
  y_points_neg <- function(yt) -scaling_factor * (2.092 + 0.7284 * cos(2.004 * yt) + 0.4391 * cos(4.008 * yt)  - 0.4641 * sin(2.004 * yt) + 0.199352 * sin(4.008 * yt)) * sin(yt)
  
  detect_pattern_434$x <- x_points(detect_pattern_434$xt) + x_shift
  detect_pattern_434$y <- c(y_points(seq(0, pi, 0.01)), rev(y_points_neg(seq(0, pi, 0.01))))
  
  #need to close polys
  detect_pattern_434[nrow(detect_pattern_434)+1,] <- detect_pattern_434[1,]
  detect_434_poly_sf <- st_sfc(st_polygon(list(as.matrix(detect_pattern_434[,c("x", "y")]))))
  
  #clean up
  rm(detect_pattern_434)
  # gc()
  
  return(detect_434_poly_sf)
}

