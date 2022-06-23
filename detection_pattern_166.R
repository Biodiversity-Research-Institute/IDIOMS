
#################Functions for calculating the detection patterns for Nanotags at 166 MHz#########################
# source(file.path(currentdir, "radio_telemetry_functions.R"))

# encoded_signal_to_actual_watts <- function(Z, p0 = 4.3458*10^-11, b = 0.3012) {
encoded_signal_to_actual_watts <- function(Z, p0 = 4.89*10^-11, b = 0.3013) {
  #convert Z integer value (0-255) to an actual signal strength in watts
  return((exp(atanh(Z/255)/b)-1) * p0)
  
}

encoded_signal_to_actual_dbm <- function(Z, p0 = 4.89*10^-11, b = 0.3013) {
  #convert Z integer value (0-255) to an actual signal strength in dBm
  signal_watts <- encoded_signal_to_actual_watts(Z, p0, b)
  return(watts_to_dbm(signal_watts))
}

xi_square_calc_dbm_r_psi <- function(r, psi, z, ant_HT, min_xi_dbm, lambda, D0, p0){
  D <- 10^(D0/10)  #Where D0 is the abosolute antenna gain and D = 7.22*Le/lambda
  le <- D*lambda/7.22
  
  #default antenna params from Janaswamy et al 2018
  k0 <- 2*pi/lambda # wavenumber in freespace
  beta0 <- -(k0+2.94/le) 
  p <- beta0*le/2 
  q <- k0*le/2
  
  # r=radial distance in meters
  # The direct line-of-sight distance between the tower and the bird in m
  R <- sqrt(r^2 + (z - ant_HT)^2) 
  # psi <- angle in radians around antenna
  g_psi <- cos(pi/2*sin(psi))/cos(psi) * (sin(p+q*cos(psi))/(p+q*cos(psi))) #version from Janaswamy - eqn 19 - Antenna field pattern
  
  xi_square <- (g_psi*sin(k0*ant_HT*z/R)/(k0*R))^2  #derived from eqn 40 in  Janawany
  xi_square_dbm <- watts_to_dbm(xi_square)
  
  return(xi_square_dbm)
  
}

#original below for 9 element yagi, default to 5
# xi_square_calc_dbm <- function(x, y, z, XT, YT, HT, theta, lambda=1.8, D0=11.1, p0=4.3458*10^-11, noisy = F){

xi_square_calc_dbm <- function(x, y, z, XT, YT, HT, theta, lambda, D0, p0, noisy = F){
  # Function for calculating receiver power at an x,y,z coordinate in dBm
  # 3D coordinates of the animal = x, y, z
  # coordinates of tower and angle of antenna = XT, YT, HT, theta
  # Antenna and receiver specific info = p,q,k0, p0
  # noise value = mu and noisy is whether to include noise or not
  # (r, phi) are the polar coordinates of the point (x,y)
  #lambda = wavelength in free space
  #le the effective length of the overall array
  #from email from R. Janaswamy on 25 Jun 2020
  #   Here is the formula I suggest:
  #     (i) Let D = absolute gain of antenna, D0 = dB gain of the antenna: D = 10^{D0/10). In your case D0 = 9 dB. Then D = 7.94
  #     (ii) D = 7.22*Le/lambda, lambda = wavelength = 1.8m in your case, Le = effective length. Using the value of D from (i) you would get Le = D*lambda/7.22 = approx D/4 = approx 2m. 
  #     (iii) Use this value of Le in equation (19) of my paper.
  #     
  #     IN response to from P Paton.
  #     1) How is the antenna gain included in the equations when estimating the radiation pattern (in the findinters.m script)? I see that Eq. 19 in Rama's paper 
  #        has been used for the radiation pattern (g(psi)) which is the function of p,q, and psi. On the other hand, p, q, and beta0 are also functions of effective 
  #      length (Le) and wave number (K0). As K0 is constant, does it mean that the effect of antenna gain has only been included by the effective length? Then, what about the number of elements on the Yagi?
  #     2) If that is correct, let's say we want to revise the code for a 5-element yagi with 9 dB, should we just replace the effective length for that? (it does not make sense to me)
  # 11 Dec 2020 - default set to 5 element yagi at 166 MHZ,  p0=4.89*10^-11 from 2017 calibration
  
  D <- 10^(D0/10)  #Where D0 is the absolute antenna gain and D = 7.22*Le/lambda
  le <- D*lambda/7.22
  
  #default antenna params from Janaswamy et al 2018
  k0 <- 2*pi/lambda # wavenumber in freespace
  beta0 <- -(k0+2.94/le) 
  p <- beta0*le/2 
  q <- k0*le/2
  
  # radial distance in meters
  r <- sqrt((x-XT)^2 + (y-YT)^2)
  # The direct line-of-sight distance between the tower and the bird in m
  R <- sqrt(r^2 + (z - HT)^2) 
  # psi <- angle in radians around antenna
  psi <- psi_calc(x,y,XT,YT, theta)
  g_psi <- cos(pi/2*sin(psi))/cos(psi) * (sin(p+q*cos(psi))/(p+q*cos(psi))) #version from Janaswamy - eqn 19
  
  # xi_square <- (r1_g_psi*sin(k0*HT*z/R)/(k0*R))^2 # 2.2 in Hua Bai in his code he had k0*HT*z^2 and
  xi_square <- (g_psi*sin(k0*HT*z/R)/(k0*R))^2  #derived from eqn 40 in  Janawany
  xi_square_dbm <- watts_to_dbm(xi_square)
  
  xi_square_noisy <- (sqrt(xi_square) + sqrt(p0))^2  #+ 2*sqrt(xi_square)*sqrt(p0) + p0*mu^2eqn 27 Ranasmamy
  xi_square_noisy_dbm <- watts_to_dbm(xi_square_noisy)
  
  if (noisy==F) {return(xi_square_dbm)
  } else {return(xi_square_noisy_dbm)}
  
}


r_squared <- function(x, y, z, XT, YT, HT, theta, lambda){
  k0 <- 2*pi/lambda # wavenumber in freespace
  r <- sqrt((x-XT)^2 + (y-YT)^2)
  R <- sqrt(r^2 + (z - HT)^2)
  psi <- psi_calc(x,y,XT,YT, theta)
  g_psi <- cos(pi/2*sin(psi))/cos(psi) * (sin(p+q*cos(psi))/(p+q*cos(psi))) #version from Janaswamy - eqn 19)
  xi <- g_psi*sin(k0*HT*z/R)/(k0*R)
  return((HT*z/abs(xi)*abs(g_psi)))
}

field_radiation_pattern <- function(lambda, D0, xi_min_dbm){
  D <- 10^(D0/10)  #Where D0 is the absolute antenna gain and D = 7.22*Le/lambda
  le <- D*lambda/7.22
  
  #default antenna params from Janaswamy et al 2018
  k0 <- 2*pi/lambda # wavenumber in freespace
  beta0 <- -(k0+2.94/le) 
  p <- beta0*le/2 
  q <- k0*le/2
  
  min_xi_sq_watts <- dbm_to_watts(xi_min_dbm)
  
  gpsi_df <- data.frame()
  for (psi in seq(0,(2*pi), pi/1000)){
    #from eqn 41 Ratnaswamy to approximate the field radiation pattern at a given power
    g_psi <- cos(pi/2*sin(psi))/cos(psi) * (sin(p+q*cos(psi))/(p+q*cos(psi))) #version from Janaswamy - eqn 19
    R <- g_psi/(k0*sqrt(min_xi_sq_watts))
    gpsi_df <- rbind(gpsi_df, cbind(psi=psi, g_psi=g_psi, R=R))
    
  }
  
  return(gpsi_df)
}

bisect_root <- function (f, a, b, num = 10, eps = 1e-05) {
  h = abs(b - a)/num
  i = 0
  j = 0
  a1 = b1 = 0
  while (i <= num) {
    a1 = a + i * h
    b1 = a1 + h
    if (f(a1) == 0) {
      val <- a1
    }
    else if (f(b1) == 0) {
      val <- b1
    }
    else if (f(a1) * f(b1) < 0) {
      repeat {
        if (abs(b1 - a1) < eps) 
          break
        x <- (a1 + b1)/2
        if (f(a1) * f(x) < 0) 
          b1 <- x
        else a1 <- x
      }
      j = j + 1
      val <- (a1 + b1)/2
    }
    i = i + 1
  }
  if (j == 0) 
  {
    # print("finding root is fail")
    return(NA)}
  else {
    # print("finding root is successful")
    return(val)}
}

# Field radiation pattern for 166 MHZ Yagi based on Ratnaswamy
yagi_antenna_detect_pattern_166 <- function(z, ant_HT, xi_min_dbm, lambda, D0, p0, maxit=10000){
  require(sf)
  D <- 10^(D0/10)  #Where D0 is the absolute antenna gain and D = 7.22*Le/lambda
  le <- D*lambda/7.22
  
  #default antenna params from Janaswamy et al 2018
  k0 <- 2*pi/lambda # wavenumber in freespace
  beta0 <- -(k0+2.94/le) 
  p <- beta0*le/2 
  q <- k0*le/2
  
  # xi_square <- (g_psi*sin(k0*HT*z/R)/(k0*R))^2  #derived from eqn 40 in  Janawany
  min_xi_watts <- dbm_to_watts(xi_min_dbm)  #convert dbm to watts
  
  root_R_fx=function(R) {
    (g_psi*sin(k0*ant_HT*z/R)/(k0*R))^2-min_xi_watts
  }
  
  field_radiation_pattern <- data.frame()
  for (psi in seq(0,(2*pi), pi/1000)){
    #from eqn 41 Ratnaswamy to approximate the field radiation pattern at a given power
    #rootSolve deals with finding the roots of n nonlinear (or linear) equations
    g_psi <- cos(pi/2*sin(psi))/cos(psi) * (sin(p+q*cos(psi))/(p+q*cos(psi))) #version from Janaswamy - eqn 19
    #The root of a function f(x) is the value of x for which f(x) = 0
    R1 <- max(rootSolve::uniroot.all(f=root_R_fx, interval=c(1,100000), maxiter=maxit))
    # R <- uniroot(f=root_R_fx, interval=c(10,1000),maxiter=maxit, extendInt="downX", tol = 1e-7)$root
    R2 <- bisect_root(f = root_R_fx, a = 1, b = 100000)
    # R3 <- pracma::brent(fun = root_R_fx, a=-1, b=100000, maxiter = 1000)$root
    R <- max(c(R1, R2), na.rm = T)
    
    # R <- R1
    # R <- R2
    field_radiation_pattern <- rbind(field_radiation_pattern, cbind(psi=psi, R=R))
  }
  
  XT = 0; YT = 0
  
  field_radiation_pattern$x = field_radiation_pattern$R*cos(field_radiation_pattern$psi) + XT
  field_radiation_pattern$y = field_radiation_pattern$R*sin(field_radiation_pattern$psi) + YT
  field_radiation_pattern$stn_ht=ant_HT
  field_radiation_pattern$z=z
  #Some floating point math? issues where  erratic points around x=0, clean
  field_radiation_pattern <- field_radiation_pattern[which(round(field_radiation_pattern$x, 5)!=0),]
  field_radiation_pattern[nrow(field_radiation_pattern)+1,] <- field_radiation_pattern[1,]
  field_radiation_pattern <- field_radiation_pattern[which(!is.infinite(field_radiation_pattern$R)),]
  
  field_radiation_pattern_sf <-  st_sfc(st_polygon(list(as.matrix(field_radiation_pattern[,c("x", "y")]))), crs = 3857) #pseudomercator
  
  return(field_radiation_pattern_sf)
}

# Basic omni antenna pattern as a simple circular radius with defaul 1km at 0,0
# omni_antenna_detect_pattern <- function(z, referenceIsotropicRange = 1000){
#   require(sf)
#   require(dplyr)
#   #returns an idealized omni-directional detection pattern as sf poly - adapted from MOTUS.org js function
#   omniRange = 1.640923 #relative to an isotropic radiator in km
#   
#   TAU <- 2 * pi # 360 degrees in radians
#   mat <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
# 
#   numSegments = 100
#   path <- list()
#   inc <- TAU/numSegments
#   j <- 0
#   for (i in seq(0, TAU, inc)) {
#     # Set up omnidirectional antenna coordinates at the north pole:
#     j <- j + 1
#     r <- omniRange * referenceIsotropicRange
#     x1 <- r * cos(i)
#     y1 <- r * sin(i)
#     # Apply the transform:
#     x2 <- mat[1,1] * x1 + mat[1,2] * y1 + mat[1,3] * z
#     y2 <- mat[2,1] * x1 + mat[2,2] * y1 + mat[2,3] * z
#     z2 <- mat[3,1] * x1 + mat[3,2] * y1 + mat[3,3] * z
#     path[[j]] <- list(x=x2, y=y2, z=z2)
# 
#   }
#   path[[j+1]] <-  path[[1]]
#   #create a polygon from the paths in R
#   path_pts <- data.table::rbindlist(path)
#   # browser()
#   poly_sf <-  st_sfc(st_polygon(list(as.matrix(path_pts))), crs = 3857)
#   # poly_sf <-  st_sfc(st_polygon(list(as.matrix(path_pts))), crs = 4326) %>% sf::st_transform(crs = 3857)
#   return(poly_sf)
# }

omni_antenna_detect_pattern <- function(z, referenceIsotropicRange = 1000){
  poly_sf <- st_sfc(st_point(x = c(0, 0)), crs = 3857) %>% st_buffer(referenceIsotropicRange)
}
