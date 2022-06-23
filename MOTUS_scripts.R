#Convert MOTUS javascript code to produce antenna array representations as shown on https://motus.org/data/receiversMap
#data can be downloaded from https://motus.org/data/downloads
#Probably should call download every week or other week throughout the year to update these data. 

yagi5Pattern = c(10.21716, 10.20632, 10.17385, 10.11995, 10.04491, 9.949187, 9.833309, 9.697941, 9.543848, 9.371896, 9.183044, 8.978338, 8.758897, 8.525911, 8.280626, 8.024336, 7.758373, 7.484096, 7.20288, 6.916108, 6.625157, 6.331388, 6.036141, 5.740719, 5.446385, 5.154347, 4.865757, 4.5817, 4.303189, 4.031159, 3.766463, 3.509867, 3.262052, 3.023604, 2.79502, 2.576707, 2.36898, 2.172065, 1.986102, 1.811149, 1.647185, 1.494113, 1.351769, 1.219924, 1.09829, 0.9865295, 0.884259, 0.7910564, 0.706468, 0.6300145, 0.561198, 0.499508, 0.4444276, 0.3954391, 0.3520294, 0.313695, 0.2799462, 0.2503114, 0.2243404, 0.2016074, 0.1817132, 0.1642873, 0.1489891, 0.1355088, 0.1235675, 0.1129175, 0.1033414, 0.09465143, 0.08668791, 0.07931799, 0.0724337, 0.06594997, 0.05980247, 0.05394539, 0.04834908, 0.04299777, 0.03788722, 0.03302248, 0.02841578, 0.02408448, 0.02004916, 0.01633199, 0.01295516, 0.009939538, 0.007303602, 0.005062475, 0.003227222, 0.001804327, 0.0007953531, 0.0001967796, 0, 0.000191459, 0.0007529164, 0.001661815, 0.002891734, 0.00441291, 0.0061928, 0.008196681, 0.01038824, 0.01273021, 0.01518488, 0.01771473, 0.02028285, 0.02285348, 0.02539235, 0.02786703, 0.03024725, 0.03250506, 0.03461507, 0.0365545, 0.03830324, 0.03984392, 0.04116185, 0.042245, 0.04308391, 0.04367166, 0.04400372, 0.04407794, 0.04389441, 0.04345546, 0.04276556, 0.0418313, 0.04066142, 0.03926674, 0.03766024, 0.03585706, 0.03387456, 0.03173233, 0.02945227, 0.02705859, 0.02457784, 0.02203889, 0.01947289, 0.01691319, 0.01439523, 0.01195637, 0.009635649, 0.007473526, 0.00551155, 0.003791967, 0.002357284, 0.001249774, 0.0005109465, 0.0001809708, 0.0002980741, 0.0008979174, 0.002012962, 0.003671836, 0.005898717, 0.008712738, 0.01212744, 0.01615025, 0.02078207, 0.02601689, 0.03184152, 0.03823541, 0.04517054, 0.05261147, 0.06051549, 0.06883282, 0.07750703, 0.08647545, 0.09566984, 0.105017, 0.1144396, 0.1238569, 0.1331859, 0.1423424, 0.1512414, 0.159799, 0.1679329, 0.1755638, 0.1826162, 0.1890194, 0.1947086, 0.1996259, 0.2037206, 0.2069504, 0.2092819, 0.2106905, 0.2111617)
yagi9Pattern = c(19.54296, 19.51802, 19.44334, 19.31925, 19.1463, 18.92528, 18.65723, 18.34337, 17.98519, 17.58441, 17.14297, 16.66305, 16.14708, 15.59768, 15.01774, 14.41032, 13.77873, 13.12642, 12.45705, 11.7744, 11.08239, 10.38503, 9.68639, 8.990542, 8.301554, 7.623427, 6.960045, 6.31514, 5.692232, 5.094588, 4.525175, 3.986616, 3.481151, 3.010603, 2.576353, 2.179317, 1.819936, 1.498176, 1.213532, 0.9650477, 0.7513441, 0.5706536, 0.4208667, 0.299585, 0.2041802, 0.1318578, 0.07972364, 0.04485103, 0.02434638, 0.01541166, 0.01540144, 0.02187273, 0.0326263, 0.04573822, 0.05958091, 0.0728333, 0.08448036, 0.09380243, 0.1003555, 0.1039434, 0.1045844, 0.1024727, 0.09793823, 0.09140573, 0.08335514, 0.07428533, 0.06468219, 0.05499227, 0.04560269, 0.03682751, 0.02890057, 0.02197429, 0.01612369, 0.01135473, 0.007615686, 0.004810547, 0.00281314, 0.00148095, 0.0006677056, 0.0002340181, 5.55903e-05, 2.876977e-05, 7.344258e-05, 0.000133477, 0.0001750921, 0.0001836415, 0.000159365, 0.0001126662, 5.943554e-05, 1.685336e-05, 0, 1.947165e-05, 8.007435e-05, 0.0001805492, 0.0003141835, 0.0004700926, 0.0006349141, 0.0007946532, 0.0009364313, 0.001049942, 0.001128472, 0.001169422, 0.001174319, 0.001148391, 0.001099804, 0.001038704, 0.0009762131, 0.0009235214, 0.000891185, 0.0008887156, 0.0009244892, 0.001005961, 0.00114013, 0.001334161, 0.001596047, 0.001935201, 0.002362838, 0.002892079, 0.003537691, 0.004315439, 0.005241076, 0.006329016, 0.00759079, 0.009033401, 0.01065772, 0.01245709, 0.01441618, 0.01651041, 0.01870575, 0.02095921, 0.02321992, 0.0254307, 0.02753018, 0.02945534, 0.03114421, 0.03253884, 0.03358805, 0.03425015, 0.03449521, 0.03430693, 0.03368398, 0.03264062, 0.03120685, 0.02942772, 0.0273622, 0.02508137, 0.02266627, 0.02020529, 0.01779136, 0.01551903, 0.01348147, 0.01176764, 0.01045963, 0.009630275, 0.00934112, 0.009640806, 0.01056386, 0.01212994, 0.0143435, 0.01719392, 0.02065597, 0.02469062, 0.02924622, 0.03425983, 0.03965882, 0.04536256, 0.05128421, 0.05733258, 0.06341392, 0.06943374, 0.07529853, 0.08091737, 0.08620341, 0.09107531, 0.09545838, 0.09928577, 0.1024993, 0.1050504, 0.1069005, 0.1080218, 0.1083974)

createOmni <- function(lat, lng) { 
  #returns an idealized omni-directional detection pattern as sf poly - adapted from MOTUS.org js function
  
  referenceIsotropicRange = 1000 # in meters
  omniRange = 1.640923 #relative to an isotropic radiator
  
  TAU <- 2 * pi # 360 degrees in radians
  mat <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
  mat <- MOTUS_rotation(0, 1, 0, TAU * (90 - lat) / 360) %*% mat # rotation along the meridian to the given latitude
  mat <- MOTUS_rotation(0, 0, 1, TAU * lng / 360) %*% mat # rotation around the north pole to the given longitude
  # mat <- product(rotation(0, 1, 0, TAU * (90 - lat) / 360), mat) # rotation along the meridian to the given latitude
  # mat <- product(rotation(0, 0, 1, TAU * lng / 360), mat) # rotation around the north pole to the given longitude
  
  numSegments = 80
  z1 = 6371000 # mean radius of Earth in meters, from https://en.wikipedia.org/wiki/Earth
  path <- list()
  inc <- TAU/numSegments
  j <- 0
  for (i in seq(0, TAU, inc)) {
    # Set up omnidirectional antenna coordinates at the north pole:
    j <- j + 1
    r <- omniRange * referenceIsotropicRange
    x1 <- r * cos(i)
    y1 <- r * sin(i)
    # Apply the transform:
    x2 <- mat[1,1] * x1 + mat[1,2] * y1 + mat[1,3] * z1
    y2 <- mat[2,1] * x1 + mat[2,2] * y1 + mat[2,3] * z1
    z2 <- mat[3,1] * x1 + mat[3,2] * y1 + mat[3,3] * z1
    # Convert from Cartesian to lat/lng:
    length <- sqrt(x2 * x2 + y2 * y2 + z2 * z2)
    lat <- (TAU/4 - acos(z2 / length)) * 360 / TAU
    lng <- atan2(y2, x2) * 360 / TAU
    while (lat < -90){
      lat <- lat + 180}
    while (lat > 90){
      lat <- lat - 180}
    path[[j]] <- list(lng=lng, lat=lat)
  }
  #create a polygon from the paths in R
  path_pts <- rbindlist(path)
  poly_sf <-  st_sfc(st_polygon(list(as.matrix(path_pts))), crs = 4326)
  return(poly_sf)
}

createYagi <- function(lat, lng, bearing, pattern) { 
  #returns an idealized yagi detection pattern as sf poly - adapted from MOTUS.org js function
  require(sf)
  referenceIsotropicRange = 1000 # in meters
  
  TAU <- 2 * pi # 360 degrees in radians
  mat <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
  mat <- MOTUS_rotation(0, 0, 1, TAU/2 - TAU * bearing / 360) %*% mat # rotation around north pole to account for bearing
  # mat <- product(rotation(0, 0, 1, TAU/2 - TAU * bearing / 360), mat) # rotation around north pole to account for bearing
  mat <- MOTUS_rotation(0, 1, 0, TAU * (90 - lat) / 360) %*% mat # rotation along the meridian to the given latitude
  # mat <- product(rotation(0, 1, 0, TAU * (90 - lat) / 360), mat) # rotation along the meridian to the given latitude
  mat <- MOTUS_rotation(0, 0, 1, TAU * lng / 360) %*% mat # rotation around the north pole to the given longitude
  # mat <- product(rotation(0, 0, 1, TAU * lng / 360), mat) # rotation around the north pole to the given longitude
  
  numSegments = 200
  z1 = 6371000 # mean radius of Earth in meters, from https://en.wikipedia.org/wiki/Earth
  path <- list()
  inc <- TAU/numSegments
  j <- 0
  for (i in seq(0, TAU, inc)) {
    # Set up yagi-uda antenna coordinates at the north pole:
    j <- j + 1
    deg <- 360 * i / TAU
    if(deg > 180){
      deg <- 360 - deg}
    floor <- floor(deg)
    ceil <- pracma::ceil(deg)
    gain <- pattern[floor+1]  #R is not zero indexed - add 1
    # browser()
    
    if(ceil != floor){
      gain <- gain + (pattern[ceil + 1] - gain) * (deg - floor)} #R is not zero indexed - add 1
    r <- gain * referenceIsotropicRange
    x1 <- r * cos(i)
    y1 <- r * sin(i)
    # Apply the transform:
    x2 <- mat[1,1] * x1 + mat[1,2] * y1 + mat[1,3] * z1
    y2 <- mat[2,1] * x1 + mat[2,2] * y1 + mat[2,3] * z1
    z2 <- mat[3,1] * x1 + mat[3,2] * y1 + mat[3,3] * z1
    # Convert from Cartesian to lat/lng:
    length <- sqrt(x2 * x2 + y2 * y2 + z2 * z2)
    lat <- 360 * (TAU/4 - acos(z2 / length)) / TAU
    lng <- 360 * atan2(y2, x2) / TAU
    
    while(lat < -90){
      lat <- lat + 180}
    while(lat > 90){
      lat <- lat - 180}
    path[[j]] <- list(lng=lng, lat=lat)
  }
  #create a polygon from the paths in R
  path_pts <- rbindlist(path)
  #Very small possible floating point issues - round lat/longs and remove dups
  path_pts$lat <- round(path_pts$lat, 6)
  path_pts$lng <- round(path_pts$lng, 6)
  # browser()
  #encounter non-closed polygon - need to fix -self intersection due to repeating consecutive points
  #remove last point from duplication check and then add back so we can close the poly
  path_pts_nodups <- path_pts[c(!duplicated(path_pts[-201,]), T),]
  poly_sf <- st_sfc(st_polygon(list(as.matrix(path_pts_nodups))), crs = 4326)
  
  return(poly_sf)
}

createYagi9 <- function(lat, lng, bearing) {return(createYagi(lat, lng, bearing, get("yagi9Pattern")))}
createYagi5 <- function(lat, lng, bearing) {return(createYagi(lat, lng, bearing, get("yagi5Pattern")))}

createFake <- function(lat, lng, bearing) {
  #returns an idealized fake detection pattern as sf poly - adapted from MOTUS.org js function
  # distances in meters
  outerRadius = 10000
  innerRadius = 6666
  width = 2500
  
  TAU <- 2 * pi # 360 degrees in radians
  mat <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
  mat <- MOTUS_rotation(0, 0, 1, TAU/2 - TAU * bearing / 360) %*% mat # rotation around north pole to account for bearing
  mat <- MOTUS_rotation(0, 1, 0, TAU * (90 - lat) / 360) %*% mat # rotation along the meridian to the given latitude
  mat <- MOTUS_rotation(0, 0, 1, TAU * lng / 360) %*% mat # rotation around the north pole to the given longitude
  # mat <- product(rotation(0, 0, 1, TAU/2 - TAU * bearing / 360), mat) # rotation around north pole to account for bearing
  # mat <- product(rotation(0, 1, 0, TAU * (90 - lat) / 360), mat) # rotation along the meridian to the given latitude
  # mat <- product(rotation(0, 0, 1, TAU * lng / 360), mat) # rotation around the north pole to the given longitude
  # 
  numSegments = 80
  z1 = 6371000 # mean radius of Earth in meters, from https://en.wikipedia.org/wiki/Earth
  path <- list()
  inc <- TAU/numSegments
  j <- 0
  for (i in seq(0, TAU, inc)) {
    # Set up generic ellipse coordinates at the north pole:
    j <- j + 1
    c <- cos(i)
    # x1 <- innerRadius + c * (c < 0 ? innerRadius : (outerRadius - innerRadius))  #uses ternary operator? = ifelse
    x1 <- innerRadius + c * (ifelse(c < 0, innerRadius, (outerRadius - innerRadius)))
    y1 <- width * sin(i)
    # Apply the transform:
    x2 <- mat[1,1] * x1 + mat[1,2] * y1 + mat[1,3] * z1
    y2 <- mat[2,1] * x1 + mat[2,2] * y1 + mat[2,3] * z1
    z2 <- mat[3,1] * x1 + mat[3,2] * y1 + mat[3,3] * z1
    # Convert from Cartesian to lat/lng:
    length <- sqrt(x2 * x2 + y2 * y2 + z2 * z2)
    lat <- 360 * (TAU/4 - acos(z2 / length)) / TAU
    lng <- 360 * atan2(y2, x2) / TAU
    while(lat < -90){
      lat <- lat + 180}
    while(lat > 90){
      lat <- lat - 180}
    path[[j]] <- list(lng=lng, lat=lat)
  }
  #create a polygon from the paths in R
  path_pts <- rbindlist(path)
  poly_sf <-  st_sfc(st_polygon(list(as.matrix(path_pts))), crs = 4326)
  return(poly_sf)
}

MOTUS_rotation <- function (x, y, z, angle) {
  c <- cos(angle)
  s <- sin(angle)
  a <- 1 - c
  rotated_mat <- rbind(c(c+a*x*x, a*x*y-s*z, a*x*z+s*y),
                       c(a*y*x+s*z,   c+a*y*y, a*y*z-s*x),
                       c(a*z*x-s*y, a*z*y+s*x,   c+a*z*z))
  return(rotated_mat)
}

MOTUS_antenna_geometry <- function(ID, ant_type, lat, lng, bearing = NULL) {
  require(sf)
  #function to generate geometry for each antenna type as idealized detection geometry
  if (ant_type %in% c('omni-mast', 'omni-whip', 'j-pole', 'monopole')) {
    ant_geom <-
      createOmni(lat,
                 lng)
  } else {
    if (!is.na(bearing) &
        (bearing >= 0) &
        (bearing < 360)) {
      if (ant_type %in% c('yagi-9-laird','yagi-9','yagi-8','custom-9','yagi-9-maple')) {
        ant_geom <-
          createYagi9(lat,
                      lng,
                      bearing)
      } else {
        if (ant_type %in% c('yagi-6', 'yagi-5', 'custom-6', 'yagi-4')) {
          ant_geom <-
            createYagi5(lat,
                        lng,
                        bearing)
        } else {
          ant_geom <-
            createFake(lat,
                       lng,
                       bearing)
        }
      }
    } else {
      #Create circle poly with radius 15000
      #buffer point at lat long by 15000 m
      # WebMerc
      #WGS84=4326
      #WebMerc=3857
      point_merc <-
        st_sfc(st_point(c(lng, lat))) %>% st_sf(crs = 4326) %>% st_transform(crs = 3857)
      ant_geom <-
        st_buffer(point_merc, 15000) %>% st_transform(crs = 4326)
    }
  }
  suppressWarnings(if(class(ant_geom)=="sf" | class(ant_geom)=="data.frame"){
    #in some cases an SF class is created instead of sfc, extract geom from it
    ant_geom <- ant_geom$geometry
  })
  return(ant_geom)
}

get_motus_receiver_antenna_data <- function(){
  MOTUS_projects <- read.csv("https://motus.org/data/downloads/api-proxy/projects/descriptions?fmt=csv&showAll=true")
  #remove descriptions - too much data
  MOTUS_projects <- MOTUS_projects %>% select(-c(descriptionLong, descriptionShort))
  
  MOTUS_antenna_deploy <- read.csv("https://motus.org/data/downloads/api-proxy/receivers/antennas?fmt=csv")
  MOTUS_receiver_deploy <- read.csv("https://motus.org/data/downloads/api-proxy/receivers/deployments?fmt=csv")
  #Create a df with receiver lat long and the antenna info
  MOTUS_receiver_antennas <- dplyr::full_join(MOTUS_receiver_deploy, MOTUS_antenna_deploy) 
  MOTUS_receiver_antennas <- dplyr::full_join(MOTUS_receiver_antennas, MOTUS_projects, by=c("recvProjectID"="projectID")) 
  
  #remove receivers with not spatial info
  MOTUS_receiver_antennas <- MOTUS_receiver_antennas[which(!is.na(MOTUS_receiver_antennas$latitude) & !is.na(MOTUS_receiver_antennas$longitude)), ]
  MOTUS_receiver_antennas$dtStart <- strptime(MOTUS_receiver_antennas$dtStart, "%Y-%m-%d %H:%M:%S")
  MOTUS_receiver_antennas$dtEnd <- strptime(MOTUS_receiver_antennas$dtEnd, "%Y-%m-%d %H:%M:%S")
  
  MOTUS_receiver_antennas$startYear <- lubridate::year(MOTUS_receiver_antennas$dtStart)
  MOTUS_receiver_antennas$endYear <- lubridate::year(MOTUS_receiver_antennas$dtEnd)
  
  #get magnetic declination for each location
  MOTUS_receiver_antennas$magdecl <-
    oce::magneticField(
      longitude = MOTUS_receiver_antennas$longitude,
      latitude = MOTUS_receiver_antennas$latitude,
      time = MOTUS_receiver_antennas$startYear
    )$declination
  
  #trueBearing has some gaps, use magdecl to correct
  MOTUS_receiver_antennas$calcBearing <- MOTUS_receiver_antennas$magneticBearing + MOTUS_receiver_antennas$magdecl
  MOTUS_receiver_antennas[which(is.na(MOTUS_receiver_antennas$trueBearing)), "trueBearing"] <- MOTUS_receiver_antennas[which(is.na(MOTUS_receiver_antennas$trueBearing)), "calcBearing"]
  
  #, by=c("motusRecvID"="motusRecvID", "recvDeployID"="recvDeployID", "recvProjectID"="recvProjectID", "receiverID"="receiverID"))
  
  #filter by active and have lat/longs
  MOTUS_receiver_antennas_active <- MOTUS_receiver_antennas %>% 
    dplyr::filter(deploymentStatus=="active")
  
  #filter by active and have lat/longs
  MOTUS_receiver_antennas_inactive <- MOTUS_receiver_antennas %>% 
    dplyr::filter(deploymentStatus=="terminated")
  
  MOTUS_receiver_ant_sf <- MOTUS_receiver_antennas_active %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(geometry=MOTUS_antenna_geometry(motusRecvID,antennaType, latitude, longitude, trueBearing))
  
  MOTUS_receiver_ant_sf <- st_sf(MOTUS_receiver_ant_sf)
  
  return(MOTUS_receiver_ant_sf)
}

# Get new MOTUS receiver station data if the data is older than 1 week stored on the server
# reduce data loading by caching new data for loading later
if (file.exists("data/MOTUS_receiver_antennas_active_sf.RData")){
  finf <- file.info("data/MOTUS_receiver_antennas_active_sf.RData", extra_cols = FALSE)
  if (difftime(Sys.time(), finf[,"mtime"], units = "days") > 7) {
    print("Retreiving new Motus data >1 week of the data age")
    # Data download fails sometimes - times out
    # Put a question to Stu Mackenzie about this issue, until then catch the error and move one
    result <- tryCatch({
      MOTUS_receiver_antennas_active_sf <- get_motus_receiver_antenna_data()
      save(MOTUS_receiver_antennas_active_sf, file = "data/MOTUS_receiver_antennas_active_sf.RData")
    }, error = function(err) {
      # error handler picks up where error was generated
      print(paste("MY_ERROR:  ",err))
      load("data/MOTUS_receiver_antennas_active_sf.RData", verbose = F)

    }, finally = {

    })  #end trycatch
  } else {
    print("Loading existing Motus data - within 1 week of the data age")

    load("data/MOTUS_receiver_antennas_active_sf.RData", verbose = F)

  }
} else {
  #MOTUS receiver data not yet stored
  print("Getting MOTUS data, none stored yet")

  MOTUS_receiver_antennas_active_sf <- get_motus_receiver_antenna_data()
  save(MOTUS_receiver_antennas_active_sf, file = "data/MOTUS_receiver_antennas_active_sf.RData")

}

MOTUS_receiver_antennas_active_sf_merc <- st_transform(MOTUS_receiver_antennas_active_sf, 3857)



##############################JS CODE FROM MOTUS CONVERTED TO R#####################
# product <- function(r1, r2) {
#   mat <- rbind(c(0, 0, 0), c(0, 0, 0), c(0, 0, 0))
#   for (i in seq(0, 2, 1)) {
#     for (j in seq(0, 2, 1)) {
#       for (k in seq(0, 2, 1)) {
#         mat[c(i), c(j)] <- mat[c(i), c(j)] + (r1[c(i), c(k)] * r2[c(k), c(j)])
#       }
#     }
#   }
#   return(mat)
# }
# 
# receiver.antenna = [];
# if(receiver.antennas === undefined || receiver.antennas.length === 0) {
#   var circleOptions = newShapeOptions();
#   circleOptions.center = center;
#   circleOptions.radius = 15000;
#   circleOptions.visible = visible && showAntennas;
#   receiver.antenna.push(new google.maps.Circle(circleOptions));
# } 
# else {
#   receiver.antennas.forEach(function(antenna) {
#     var bearing = antenna[0];
#     var antennaType = antenna[1];
#     if(antennaType === 'omni-mast' || antennaType === 'omni-whip' || antennaType === 'j-pole' || antennaType === 'monopole') {
#       var polygonOptions = newShapeOptions();
#       polygonOptions.paths = createOmni(receiver.lat, receiver.lng);
#       polygonOptions.visible = visible && showAntennas;
#       receiver.antenna.push(new google.maps.Polygon(polygonOptions));
#     } else if(typeof bearing === 'number' && bearing >= 0 && bearing < 360) {
#       if(antennaType === 'yagi-9-laird' || antennaType === 'yagi-9' || antennaType === 'yagi-8' || antennaType === 'custom-9' || antennaType === 'yagi-9-maple') {
#         var polygonOptions = newShapeOptions();
#         polygonOptions.paths = createYagi9(receiver.lat, receiver.lng, bearing);
#         polygonOptions.visible = visible && showAntennas;
#         receiver.antenna.push(new google.maps.Polygon(polygonOptions));
#       } else if(antennaType === 'yagi-6' || antennaType === 'yagi-5' || antennaType === 'custom-6' || antennaType === 'yagi-4') {
#         var polygonOptions = newShapeOptions();
#         polygonOptions.paths = createYagi5(receiver.lat, receiver.lng, bearing);
#         polygonOptions.visible = visible && showAntennas;
#         receiver.antenna.push(new google.maps.Polygon(polygonOptions));
#       } else {
#         var polygonOptions = newShapeOptions();
#         polygonOptions.paths = createFake(receiver.lat, receiver.lng, bearing);
#         polygonOptions.visible = visible && showAntennas;
#         receiver.antenna.push(new google.maps.Polygon(polygonOptions));
#       }
#     } else {
#       var circleOptions = newShapeOptions();
#       circleOptions.center = center;
#       circleOptions.radius = 15000;
#       circleOptions.visible = visible && showAntennas;
#       receiver.antenna.push(new google.maps.Circle(circleOptions));
#     }
#   });
# }
# 
# var referenceIsotropicRange = 1000; // in meters
# var omniRange = 1.640923; // relative to an isotropic radiator
# var yagi5Pattern = [10.21716, 10.20632, 10.17385, 10.11995, 10.04491, 9.949187, 9.833309, 9.697941, 9.543848, 9.371896, 9.183044, 8.978338, 8.758897, 8.525911, 8.280626, 8.024336, 7.758373, 7.484096, 7.20288, 6.916108, 6.625157, 6.331388, 6.036141, 5.740719, 5.446385, 5.154347, 4.865757, 4.5817, 4.303189, 4.031159, 3.766463, 3.509867, 3.262052, 3.023604, 2.79502, 2.576707, 2.36898, 2.172065, 1.986102, 1.811149, 1.647185, 1.494113, 1.351769, 1.219924, 1.09829, 0.9865295, 0.884259, 0.7910564, 0.706468, 0.6300145, 0.561198, 0.499508, 0.4444276, 0.3954391, 0.3520294, 0.313695, 0.2799462, 0.2503114, 0.2243404, 0.2016074, 0.1817132, 0.1642873, 0.1489891, 0.1355088, 0.1235675, 0.1129175, 0.1033414, 0.09465143, 0.08668791, 0.07931799, 0.0724337, 0.06594997, 0.05980247, 0.05394539, 0.04834908, 0.04299777, 0.03788722, 0.03302248, 0.02841578, 0.02408448, 0.02004916, 0.01633199, 0.01295516, 0.009939538, 0.007303602, 0.005062475, 0.003227222, 0.001804327, 0.0007953531, 0.0001967796, 0, 0.000191459, 0.0007529164, 0.001661815, 0.002891734, 0.00441291, 0.0061928, 0.008196681, 0.01038824, 0.01273021, 0.01518488, 0.01771473, 0.02028285, 0.02285348, 0.02539235, 0.02786703, 0.03024725, 0.03250506, 0.03461507, 0.0365545, 0.03830324, 0.03984392, 0.04116185, 0.042245, 0.04308391, 0.04367166, 0.04400372, 0.04407794, 0.04389441, 0.04345546, 0.04276556, 0.0418313, 0.04066142, 0.03926674, 0.03766024, 0.03585706, 0.03387456, 0.03173233, 0.02945227, 0.02705859, 0.02457784, 0.02203889, 0.01947289, 0.01691319, 0.01439523, 0.01195637, 0.009635649, 0.007473526, 0.00551155, 0.003791967, 0.002357284, 0.001249774, 0.0005109465, 0.0001809708, 0.0002980741, 0.0008979174, 0.002012962, 0.003671836, 0.005898717, 0.008712738, 0.01212744, 0.01615025, 0.02078207, 0.02601689, 0.03184152, 0.03823541, 0.04517054, 0.05261147, 0.06051549, 0.06883282, 0.07750703, 0.08647545, 0.09566984, 0.105017, 0.1144396, 0.1238569, 0.1331859, 0.1423424, 0.1512414, 0.159799, 0.1679329, 0.1755638, 0.1826162, 0.1890194, 0.1947086, 0.1996259, 0.2037206, 0.2069504, 0.2092819, 0.2106905, 0.2111617];
# var yagi9Pattern = [19.54296, 19.51802, 19.44334, 19.31925, 19.1463, 18.92528, 18.65723, 18.34337, 17.98519, 17.58441, 17.14297, 16.66305, 16.14708, 15.59768, 15.01774, 14.41032, 13.77873, 13.12642, 12.45705, 11.7744, 11.08239, 10.38503, 9.68639, 8.990542, 8.301554, 7.623427, 6.960045, 6.31514, 5.692232, 5.094588, 4.525175, 3.986616, 3.481151, 3.010603, 2.576353, 2.179317, 1.819936, 1.498176, 1.213532, 0.9650477, 0.7513441, 0.5706536, 0.4208667, 0.299585, 0.2041802, 0.1318578, 0.07972364, 0.04485103, 0.02434638, 0.01541166, 0.01540144, 0.02187273, 0.0326263, 0.04573822, 0.05958091, 0.0728333, 0.08448036, 0.09380243, 0.1003555, 0.1039434, 0.1045844, 0.1024727, 0.09793823, 0.09140573, 0.08335514, 0.07428533, 0.06468219, 0.05499227, 0.04560269, 0.03682751, 0.02890057, 0.02197429, 0.01612369, 0.01135473, 0.007615686, 0.004810547, 0.00281314, 0.00148095, 0.0006677056, 0.0002340181, 5.55903e-05, 2.876977e-05, 7.344258e-05, 0.000133477, 0.0001750921, 0.0001836415, 0.000159365, 0.0001126662, 5.943554e-05, 1.685336e-05, 0, 1.947165e-05, 8.007435e-05, 0.0001805492, 0.0003141835, 0.0004700926, 0.0006349141, 0.0007946532, 0.0009364313, 0.001049942, 0.001128472, 0.001169422, 0.001174319, 0.001148391, 0.001099804, 0.001038704, 0.0009762131, 0.0009235214, 0.000891185, 0.0008887156, 0.0009244892, 0.001005961, 0.00114013, 0.001334161, 0.001596047, 0.001935201, 0.002362838, 0.002892079, 0.003537691, 0.004315439, 0.005241076, 0.006329016, 0.00759079, 0.009033401, 0.01065772, 0.01245709, 0.01441618, 0.01651041, 0.01870575, 0.02095921, 0.02321992, 0.0254307, 0.02753018, 0.02945534, 0.03114421, 0.03253884, 0.03358805, 0.03425015, 0.03449521, 0.03430693, 0.03368398, 0.03264062, 0.03120685, 0.02942772, 0.0273622, 0.02508137, 0.02266627, 0.02020529, 0.01779136, 0.01551903, 0.01348147, 0.01176764, 0.01045963, 0.009630275, 0.00934112, 0.009640806, 0.01056386, 0.01212994, 0.0143435, 0.01719392, 0.02065597, 0.02469062, 0.02924622, 0.03425983, 0.03965882, 0.04536256, 0.05128421, 0.05733258, 0.06341392, 0.06943374, 0.07529853, 0.08091737, 0.08620341, 0.09107531, 0.09545838, 0.09928577, 0.1024993, 0.1050504, 0.1069005, 0.1080218, 0.1083974];
# 
# function createOmni(lat, lng) { 
#   var TAU = 2 * Math.PI; // 360 degrees in radians
#   var t = [[1, 0, 0], [0, 1, 0], [0, 0, 1]];
#   t = product(rotation(0, 1, 0, TAU * (90 - lat) / 360), t); // rotation along the meridian to the given latitude
#   t = product(rotation(0, 0, 1, TAU * lng / 360), t); // rotation around the north pole to the given longitude
#   
#   var numSegments = 40;
#   var z1 = 6371000; // mean radius of Earth in meters, from https://en.wikipedia.org/wiki/Earth
#   var path = [];
#   for(var i=0; i<TAU; i+=TAU/numSegments) {
#     // Set up omnidirectional antenna coordinates at the north pole:
#       var r = omniRange * referenceIsotropicRange;
#       var x1 = r * Math.cos(i);
#       var y1 = r * Math.sin(i);
#       // Apply the transform:
#         var x2 = t[0][0] * x1 + t[0][1] * y1 + t[0][2] * z1;
#         var y2 = t[1][0] * x1 + t[1][1] * y1 + t[1][2] * z1;
#         var z2 = t[2][0] * x1 + t[2][1] * y1 + t[2][2] * z1;
#         // Convert from Cartesian to lat/lng:
#           var length = Math.sqrt(x2 * x2 + y2 * y2 + z2 * z2);
#           var lat = (TAU/4 - Math.acos(z2 / length)) * 360 / TAU;
#           var lng = Math.atan2(y2, x2) * 360 / TAU;
#           while(lat < -90)
#             lat += 180;
#             while(lat > 90)
#               lat -= 180;
#               path.push(new google.maps.LatLng(lat, lng));
#   }
#   return path;
# }
# 
# function createYagi9(lat, lng, bearing) { return createYagi(lat, lng, bearing, yagi9Pattern); }
# function createYagi5(lat, lng, bearing) { return createYagi(lat, lng, bearing, yagi5Pattern); }
# 
# function createYagi(lat, lng, bearing, pattern) { 
#   var TAU = 2 * Math.PI; // 360 degrees in radians
#   var t = [[1, 0, 0], [0, 1, 0], [0, 0, 1]];
#   t = product(rotation(0, 0, 1, TAU/2 - TAU * bearing / 360), t); // rotation around north pole to account for bearing
#   t = product(rotation(0, 1, 0, TAU * (90 - lat) / 360), t); // rotation along the meridian to the given latitude
#   t = product(rotation(0, 0, 1, TAU * lng / 360), t); // rotation around the north pole to the given longitude
#   
#   var numSegments = 200;
#   var z1 = 6371000; // mean radius of Earth in meters, from https://en.wikipedia.org/wiki/Earth
#   var path = [];
#   for(var i=0; i<TAU; i+=TAU/numSegments) {
#     // Set up yagi-uda antenna coordinates at the north pole:
#       var deg = 360 * i / TAU;
#       if(deg > 180)
#         deg = 360 - deg;
#       var floor = Math.floor(deg);
#       var ceil = Math.ceil(deg);
#       var gain = pattern[floor];
#       if(ceil !== floor)
#         gain += (pattern[ceil] - gain) * (deg - floor);
#         var r = gain * referenceIsotropicRange;
#         var x1 = r * Math.cos(i);
#         var y1 = r * Math.sin(i);
#         // Apply the transform:
#           var x2 = t[0][0] * x1 + t[0][1] * y1 + t[0][2] * z1;
#           var y2 = t[1][0] * x1 + t[1][1] * y1 + t[1][2] * z1;
#           var z2 = t[2][0] * x1 + t[2][1] * y1 + t[2][2] * z1;
#           // Convert from Cartesian to lat/lng:
#             var length = Math.sqrt(x2 * x2 + y2 * y2 + z2 * z2);
#             var lat = 360 * (TAU/4 - Math.acos(z2 / length)) / TAU;
#             var lng = 360 * Math.atan2(y2, x2) / TAU;
#             while(lat < -90)
#               lat += 180;
#               while(lat > 90)
#                 lat -= 180;
#                 path.push(new google.maps.LatLng(lat, lng));
#   }
#   return path;
# }
# 
# function createFake(lat, lng, bearing) {
#   // distances in meters
#   var outerRadius = 10000;
#   var innerRadius = 6666;
#   var width = 2500;
#   
#   var TAU = 2 * Math.PI; // 360 degrees in radians
#   var t = [[1, 0, 0], [0, 1, 0], [0, 0, 1]];
#   t = product(rotation(0, 0, 1, TAU/2 - TAU * bearing / 360), t); // rotation around north pole to account for bearing
#   t = product(rotation(0, 1, 0, TAU * (90 - lat) / 360), t); // rotation along the meridian to the given latitude
#   t = product(rotation(0, 0, 1, TAU * lng / 360), t); // rotation around the north pole to the given longitude
#   
#   var numSegments = 40;
#   var z1 = 6371000; // mean radius of Earth in meters, from https://en.wikipedia.org/wiki/Earth
#   var path = [];
#   for(var i=0; i<TAU; i+=TAU/numSegments) {
#     // Set up generic ellipse coordinates at the north pole:
#       var c = Math.cos(i);
#       var x1 = innerRadius + c * (c < 0 ? innerRadius : (outerRadius - innerRadius));
#       var y1 = width * Math.sin(i);
#       // Apply the transform:
#         var x2 = t[0][0] * x1 + t[0][1] * y1 + t[0][2] * z1;
#         var y2 = t[1][0] * x1 + t[1][1] * y1 + t[1][2] * z1;
#         var z2 = t[2][0] * x1 + t[2][1] * y1 + t[2][2] * z1;
#         // Convert from Cartesian to lat/lng:
#           var length = Math.sqrt(x2 * x2 + y2 * y2 + z2 * z2);
#           var lat = 360 * (TAU/4 - Math.acos(z2 / length)) / TAU;
#           var lng = 360 * Math.atan2(y2, x2) / TAU;
#           while(lat < -90)
#             lat += 180;
#             while(lat > 90)
#               lat -= 180;
#               path.push(new google.maps.LatLng(lat, lng));
#   }
#   return path;
# }
# 
# function product(r1, r2) {
#   var t = [[0, 0, 0], [0, 0, 0], [0, 0, 0]];
#   for(var i=0; i<3; i++)
#     for(var j=0; j<3; j++)
#       for(var k=0; k<3; k++)
#         t[i][j] += r1[i][k] * r2[k][j];
#         return t;
# }
# 
# function rotation(x, y, z, angle) {
#   var c = Math.cos(angle);
#   var s = Math.sin(angle);
#   var a = 1 - c;
#   return [[  c+a*x*x, a*x*y-s*z, a*x*z+s*y],
#           [a*y*x+s*z,   c+a*y*y, a*y*z-s*x],
#           [a*z*x-s*y, a*z*y+s*x,   c+a*z*z]];
# }


