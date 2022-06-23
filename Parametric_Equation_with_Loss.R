#load in packages
library(lattice)
library(dplyr)
library(ggplot2)

t <- seq(0, pi, length.out=50); 
r <- 2.092 +0.7284 * cos(2.004* t) + 0.4391 * cos(4.008 * t)  - 0.4641*sin(2.004* t) + 0.199352* sin(4.008* t) 
plot(t, r)

#radius of the Earth in kilometers
R_earth = 6371 #kilometers
height_of_station = 0.1 #height of station in kilometers (100 meters)
height_of_bird = 3 #height of bird in kilometers ( 3000 meters)

#galilean transformation to find the distance to the horizon in the birds frame of reference
if (height_of_bird <= height_of_station) {
  g_transform_height = height_of_station
}else if (height_of_bird > height_of_station) {
  g_transform_height = height_of_station + height_of_bird
}

max_g_distance_to_horizon = sqrt(2 * R_earth * g_transform_height) #projected distance to horizon in km, this is approximately the same as the radial distance as the size of the earth is much larger than the height of any birds
f = 434 *10^6 #frequency of the antenna
c = 299792458 #speed of light

#theoretical free space path distance losses
FSPL1 = 100 #cut off at -110 dbm from the antenna
tg = 2 #transmitter gain in dbi, this is direct from CTTs FCC filing
rg = 12 #reciever gain in dbi, this is direct from our conversations with the antenna developer


#calculate the distance in which the free space loss is 100dbm to find the max range of the antenna
free_space_path_range_drop_off_dist = exp((1/20)*(FSPL1*log(10) + rg*log(10) + tg*log(10) -20*log(f)-20*log(4*pi/c)))
free_space_path_range_drop_off_dist = free_space_path_range_drop_off_dist/1000

#making sure the horizon does not get in the way
if (free_space_path_range_drop_off_dist <= max_g_distance_to_horizon) {
  abs_max_detection_range = free_space_path_range_drop_off_dist
}else if (free_space_path_range_drop_off_dist > max_g_distance_to_horizon) {
  abs_max_detection_range = max_g_distance_to_horizon
}

#We use kilometers here but there is nothing stopping you from using any unit of distance you so please. 
max_range = abs_max_detection_range
scaling_factor = abs_max_detection_range/(2*pi)
t <- seq(0, pi, length.out=50); 
r <- scaling_factor*(2.092 +0.7284 * cos(2.004* t) + 0.4391 * cos(4.008 * t)  - 0.4641*sin(2.004* t) + 0.199352* sin(4.008* t)) 
plot(t, r)

dat <- data.frame(t=seq(0, pi, 0.1))
x_points <- function(t) scaling_factor * ( 2.092 +0.7284 * cos(2.004* t) + 0.4391 * cos(4.008 * t)  - 0.4641*sin(2.004 * t) + 0.199352 * sin(4.008 * t) ) * cos(t)
y_points <- function(t) scaling_factor * ( 2.092 +0.7284 * cos(2.004* t) + 0.4391 * cos(4.008 * t)  - 0.4641*sin(2.004 * t) + 0.199352 * sin(4.008 * t) )*sin(t)
dat$y=y_points(dat$t)
dat$x=x_points(dat$t)
with(dat, plot(x,y, type="l"))

dat <- data.frame(t=seq(0, pi, 0.01))
x_points <- function(t) scaling_factor * ( 2.092 +0.7284 * cos(2.004* t) + 0.4391 * cos(4.008 * t)  - 0.4641*sin(2.004 * t) + 0.199352 * sin(4.008 * t) ) * cos(t)
y_points <- function(t) scaling_factor * ( 2.092 +0.7284 * cos(2.004* t) + 0.4391 * cos(4.008 * t)  - 0.4641*sin(2.004 * t) + 0.199352 * sin(4.008 * t) )*sin(t)
y_points1 <- function(t) -scaling_factor * ( 2.092 +0.7284 * cos(2.004* t) + 0.4391 * cos(4.008 * t)  - 0.4641*sin(2.004 * t) + 0.199352 * sin(4.008 * t) )*sin(t)

dat$y_pos=y_points(dat$t)
dat$y_neg=y_points1(dat$t)
dat$x_pos_neg=x_points(dat$t)

p = ggplot() + 
  geom_line(data = dat, aes(x = x_pos_neg, y = y_pos), color = "blue") +
  geom_line(data = dat, aes(x = x_pos_neg, y = y_neg), color = "blue") +
  xlab('X') +
  ylab('Y')

print(p)


#below is legacy code for used for identifying how the radiation pattern behaves at higher altitudes using
#cyclindrical symmetry as opposed to free space path loss


#z = 0
#r_2 = 1.5
#const_p = asin(z/r_2)
#print(const_p)

#dat <- data.frame(t=seq(0, pi, 0.01))
#x_points <- function(t) scaling_factor * ( 2.092 +0.7284 * cos(2.004* t) + 0.4391 * cos(4.008 * t)  - 0.4641*sin(2.004 * t) + 0.199352 * sin(4.008 * t) ) * cos(t)
#y_points <- function(t) scaling_factor * ( 2.092 +0.7284 * cos(2.004* t) + 0.4391 * cos(4.008 * t)  - 0.4641*sin(2.004 * t) + 0.199352 * sin(4.008 * t) )*sin(t) * cos(const_p)
#y_points1 <- function(t) -scaling_factor * ( 2.092 +0.7284 * cos(2.004* t) + 0.4391 * cos(4.008 * t)  - 0.4641*sin(2.004 * t) + 0.199352 * sin(4.008 * t) )*sin(t) 

#dat$y_pos=y_points(dat$t)
#dat$y_neg=y_points1(dat$t)
#dat$x_pos_neg=x_points(dat$t)

#p = ggplot() + 
#  geom_line(data = dat, aes(x = x_pos_neg, y = y_pos), color = "blue") +
#  geom_line(data = dat, aes(x = x_pos_neg, y = y_neg), color = "red") +
#  xlab('X') +
#  ylab('Y')

#print(p)

