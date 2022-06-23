
library(ggplot2)


knn_Radiation_Patterns <- function (xyz_df,calibration_df){
  # knn_Radiation_Patterns <- function (X,Y,Z,antenna_csv){
    
  #Nearest_Neighbor_Search_Radiation_Patterns intakes an interger value for X, Y and Z as well as a string containing the name of the antenna csv and exports an interger value of the associated RSSI
  # require(class)

  # data <- read.csv(antenna_csv)
  #dropping exta data column
  # data = data.drop(columns = ['Unnamed: 0'])
  # data <- data[,-c(1)]
  #ensuring the value provided is within the bounds of the calibration data
  # if X > max(data.X) * 1.05 or X < min(data.X) * 1.05:
  #   RSSI_Generated = 0
  # return RSSI_Generated 
  # elif Y > max(data.Y) * 1.05 or Y < min(data.Y) * 1.05:
  #   RSSI_Generated = 0
  # return RSSI_Generated 
  # elif Z > max(data.Z) * 1.05 or Z < 0:
  #   RSSI_Generated = 0
  # return RSSI_Generated 
  
  #converting the columns to numpy arrays for speed by the learning algorithm  
  # RSSI = data.RSSI.to_numpy()
  # data = data.drop(columns = ['RSSI']).to_numpy()
  
  #generating a nearest neighbor model using 5 neighbors
  # neigh_model = KNeighborsRegressor(n_neighbors = 5)
  # neigh_model.fit(data,RSSI)
  #creating an array with the test point
  # test_point = np.array([[X,Y,Z]])
  # 
  # #using the model to predict the value of the RSSI point
  # RSSI_Generated = neigh_model.predict(test_point)
  # RSSI_Generated = RSSI_Generated[0] 
  # data[,c("X","Y","Z")]
  
  # #ensuring the value provided is within the bounds of the calibration data
  # if (X > max(data$X) * 1.05 | X < min(data$X) * 1.05){
  #   RSSI_Generated = 0
  #   
  # }
  # else if (Y > max(data$Y) * 1.05 | Y < min(data$Y) * 1.05) {
  #   RSSI_Generated = 0
  #   
  # }
  # else if (Z > max(data$Z) * 1.05 | Z < 0){
  #   RSSI_Generated = 0
  #   
  # } else {
  #   # set.seed(1000)
  #   
  #   # neigh_model <- class::knn(train = data[,c("X","Y","Z")], test = data.frame(X=X,Y=Y,Z=Z), cl = as.factor(sort(data$RSSI)), k = 5,prob = F)
  #   # neigh_model <- neighbr::knn(train_set = data, test_set = data.frame(X=X,Y=Y,Z=Z), continuous_target = "RSSI", k = 5, 
  #   #                  comparison_measure="euclidean")
  #   
  #   neigh_model <- FNN::knn.reg(train = data[,c("X","Y","Z")], test = pattern_df, y = data$RSSI,k = 5)
  #   # browser()
  #   # print(attributes(neigh_model)$prob)
  #   # RSSI_Generated <- as.numeric(as.character(neigh_model))
  #   # RSSI_Generated <- neigh_model$test_set_scores$continuous_target #neighbr
  #   RSSI_Generated <- neigh_model$pred #FNN
  #   
  # }
  
  neigh_model <- FNN::knn.reg(train = calibration_df[,c("X","Y","Z")], test = xyz_df, y = calibration_df$RSSI,k = 5)
  RSSI_Generated <- neigh_model$pred #FNN
  

  return(RSSI_Generated)
} 


results <- data.frame(i=1:20, nn=NA)
for (i in 1:20){
  
  results[i,"nn"] <- (knn_Radiation_Patterns(500,5000,1500,'P:/NYSERDA Nanotag tools/URI_Carlson_434_calibration/final_MON_composite_data_upd80.csv'))
  
}

mean(results$nn)

test <- knn_Radiation_Patterns(500,5000,1500,'P:/NYSERDA Nanotag tools/URI_Carlson_434_calibration/final_MON_composite_data_upd80.csv')
create_knn_pattern(45, -70, 45)

for (i in 1:5){
  print(knn_Radiation_Patterns(500,5000,1500,'P:/NYSERDA Nanotag tools/URI_Carlson_434_calibration/final_MON_composite_data_upd80.csv'))
  
}

#generate dataframe of X, Y, Z coordinates for generating the antenna patterns
x.seq <- seq.int(-10000, 10000, 100)
y.seq <- seq.int(-10000, 30000, 100)
z.seq <- seq.int(0, 5000, 100)

xyz_df = expand.grid(X=x.seq, Y=y.seq, Z=z.seq)
dims <- c(length(x.seq), length(y.seq), length(z.seq))

#perform KNN.reg from calibration to get the RSSI for each X,Y,Z combo
# knn_RSSI_pattern <- knn_Radiation_Patterns(pattern_array,'P:/NYSERDA Nanotag tools/URI_Carlson_434_calibration/final_MON_composite_data_upd80.csv')
calibration_df <- read.csv('P:/NYSERDA Nanotag tools/URI_Carlson_434_calibration/final_MON_composite_data_upd80.csv')
  #dropping exta data column
calibration_df <- calibration_df[,-c(1)]

RSSI_model <- FNN::knn.reg(train = calibration_df[,c("X","Y","Z")], test = xyz_df, y = calibration_df$RSSI,k = 10)
knn_RSSI_pattern <- RSSI_model$pred 

#bind coords to est. RSSI
knn_RSSI_pattern <- cbind(xyz_df, RSSI_pred = knn_RSSI_pattern)
#Remove  values less than 1.05 the smallest value or greater than 1.05 the largest RSSI value from calibration
max_RSSI <- max(calibration_df$RSSI) * 1.05
min_RSSI <- min(calibration_df$RSSI) * 1.05

max_X_with_RSSI <- max(calibration_df$X) * 1.05
min_X_with_RSSI <- min(calibration_df$X) * 1.05
max_Y_with_RSSI <- max(calibration_df$Y) * 1.05
min_Y_with_RSSI <- min(calibration_df$Y) * 1.05
max_Z_with_RSSI <- max(calibration_df$Z) * 1.05
min_Z_with_RSSI <- min(calibration_df$Z) * 1.05

knn_RSSI_pattern_bounded <- knn_RSSI_pattern
  
knn_RSSI_pattern_bounded[which(knn_RSSI_pattern_bounded$X > max_X_with_RSSI | knn_RSSI_pattern_bounded$X < min_X_with_RSSI), "RSSI_pred"] <- NA
knn_RSSI_pattern_bounded[which(knn_RSSI_pattern_bounded$Y > max_Y_with_RSSI | knn_RSSI_pattern_bounded$Y < min_Y_with_RSSI), "RSSI_pred"] <- NA
knn_RSSI_pattern_bounded[which(knn_RSSI_pattern_bounded$Z > max_Z_with_RSSI | knn_RSSI_pattern_bounded$Z < min_Z_with_RSSI), "RSSI_pred"] <- NA


test_slice_100 <- knn_RSSI_pattern_bounded[which(knn_RSSI_pattern_bounded$Z==100),]
test_slice_100 <- sf::st_as_sf(test_slice_100, coords=c("X", "Y"))

ggplot(test_slice_100) + 
  geom_sf(aes(color=RSSI_pred))


test_slice_1000 <- knn_RSSI_pattern_bounded[which(knn_RSSI_pattern_bounded$Z==1000),]
test_slice_1000 <- sf::st_as_sf(test_slice_1000, coords=c("X", "Y"))

ggplot(test_slice_1000) + 
  geom_sf(aes(color=RSSI_pred))


r_psi_df <- xyz_df

r_psi_df$r <- apply(r_psi_df, 1, function(row) sqrt(row[["X"]]^2 + row[["Y"]]^2))
r_psi_df$psi <-  apply(r_psi_df, 1, function(row) psi_calc(row[["X"]],row[["Y"]],0,0, 0))
knn_RSSI_pattern_polar <- cbind(r_psi_df[,c("r", "psi", "Z")], RSSI_pred = knn_RSSI_pattern_bounded$RSSI_pred)

knn_RSSI_pattern_polar_1000 <- knn_RSSI_pattern_polar[which(knn_RSSI_pattern_polar$Z==1000),]

ggplot(knn_RSSI_pattern_polar_1000) +
  geom_point(aes(x=r, y=psi, color=RSSI_pred)) +
  coord_polar(theta="y") 


#generate dataframe of X, Y, Z coordinates for generating the antenna patterns
r.seq <- seq.int(0, 30000, 100)
psi.seq <- seq(0,(2*pi), pi/1000)
z.polar.seq <- seq.int(0, 5000, 100)

r_psi_RSSI_df = expand.grid(r=r.seq, psi=psi.seq, Z=z.polar.seq)
dims <- c(length(r.seq), length(psi.seq), length(z.polar.seq))

r_psi_RSSI_df$X <- r_psi_RSSI_df$r * cos(r_psi_RSSI_df$psi)
r_psi_RSSI_df$Y <- r_psi_RSSI_df$r * sin(r_psi_RSSI_df$psi)

calibration_SEL_df <- read.csv('P:/NYSERDA Nanotag tools/URI_Carlson_434_calibration/final_SEL_composite_data_upd17.csv')
#dropping exta data column
calibration_SEL_df <- calibration_SEL_df[,-c(1)]

RSSI_polar_model_SEL <- FNN::knn.reg(train = calibration_SEL_df[,c("X","Y","Z")], test = r_psi_RSSI_df[,c("X", "Y", "Z")], y = calibration_SEL_df$RSSI,k = 5)
RSSI_polar_REGCV_SEL <- FNN::knn.reg(train = calibration_SEL_df[,c("X","Y","Z")], y = calibration_SEL_df$RSSI,k = 5)
r_psi_RSSI_df$RSSI_pred_SEL <- RSSI_polar_model_SEL$pred 


r_psi_RSSI_df_1000 <- r_psi_RSSI_df[which(r_psi_RSSI_df$Z==1000),]

ggplot(r_psi_RSSI_df_1000) +
  geom_point(aes(x=r, y=psi, color=RSSI_pred_SEL)) +
  coord_polar(theta="y") 

r_psi_RSSI_df_200 <- r_psi_RSSI_df[which(r_psi_RSSI_df$Z==200),]

ggplot(r_psi_RSSI_df_200) +
  geom_point(aes(x=r, y=psi, color=RSSI_pred_SEL)) +
  coord_polar(theta="y")


calibration_SEL_df$Z_bin <- cut(calibration_SEL_df$Z, breaks = 20)

ggplot(calibration_SEL_df) +
  geom_point(aes(x=Y, y=RSSI)) + 
  facet_wrap(facets = calibration_SEL_df$Z_bin)


ggplot(calibration_SEL_df) +
  geom_point(aes(x=X, y=RSSI)) + 
  facet_wrap(facets = calibration_SEL_df$Z_bin)

calibration_SEL_sp <- SpatialPointsDataFrame(coords = cbind(calibration_SEL_df$X, calibration_SEL_df$Y, calibration_SEL_df$Z), data = calibration_SEL_df)

library(geosptdb)
SEL_IDW_model <- geosptdb::idwST(RSSI~1, data = calibration_SEL_sp, newdata = SpatialPoints(r_psi_RSSI_df[,c("X", "Y", "Z")]), n.neigh = 5)


library(KernelKnn)
y_calib_SEL = c(1:length(unique(calibration_SEL_df$RSSI)))

RSSI_polar_model_SEL <- KernelKnn::KernelKnn(data = calibration_SEL_df[,c("X","Y","Z")], TEST_data = r_psi_RSSI_df[,c("X", "Y", "Z")], 
                                             y = calibration_SEL_df$RSSI, k = 5)

data(ionosphere, package = 'KernelKnn')

apply(ionosphere, 2, function(x) length(unique(x)))
ionosphere = ionosphere[, -2]
