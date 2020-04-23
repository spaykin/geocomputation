library(sf)
library(raster)
library(dplyr)
library(spData)

# 4.2.1 Spatial Subsetting

# create object representing Canterbury region in NZ
canterbury = nz %>% filter(Name == "Canterbury")

# return all high points in Canterbury
canterbury_height = nz_height[canterbury, ]

plot(canterbury_height)

# return points that do not intersect with Canterbury
nz_height[canterbury, , op = st_disjoint]

# How many high points in Canterbury?
canterbury <- nz %>% filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]
nrow(canterbury_height)

region_peaks <- 
  nz %>%
  st_join(nz_height) %>%
  group_by(Name) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  st_drop_geometry()

region_peaks


# 4.2.2 Topological Relations

# creating simple test data

# create polygon
a_poly = st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1))))
a = st_sfc(a_poly)

# create line
l_line = st_linestring(x = matrix(c(-1, -1, -0.5, 1), ncol = 2))
l = st_sfc(l_line)

# create points
p_matrix = matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2)
p_multi = st_multipoint(x = p_matrix)
p = st_cast(st_sfc(p_multi), "POINT")

# which of the points in p intersect with polygon a?
st_intersects(p, a, sparse = FALSE)

# st_disjoint() is the opposite of st_intersects() - returns objects that do not relate
st_disjoint(p, a, sparse = FALSE)

# st_within returns objects that are completely within
st_within(p, a, sparse = FALSE) [,1]

# st_touches() returns what touches part of the border
st_touches(p, a, sparse = FALSE) [,1]

# st_is_within_distance() returns what almost touches the object, within a certain distance
sel = st_is_within_distance(p, a, dist = 0.9)
lengths(sel) > 0

# 4.2.3 Spatial Joining

# joining based on shared areas of geographic space / spatial overlay

# Of 10 random points on Earth, which countries are they in?
# Create random points
set.seed(2018) # set seed for reproducability

bb_world = st_bbox(world) # the world's bounds

random_df = tibble(
  x = runif(n = 10, min = bb_world[1], max = bb_world[3]),
  y = runif(n = 10, min = bb_world[2], max = bb_world[4]))

 random_points = random_df %>%
   st_as_sf(coords = c("x", "y")) %>% # set coordinates
   st_set_crs(4326) # set geographic CRS
 
 plot(random_points)
 
world_random = world[random_points, ]
nrow(world_random)
random_joined = st_join(random_points, world["name_long"])

plot(random_joined) 

# 4.2.4 Non-Overlapping Joins

# Geographic datasets that do not touch but still have strong geographic relationship

plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")

any(st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE))

# non-overlapping join

cycle_hire_P = st_transform(cycle_hire, 27700)
cycle_hire_osm_P = st_transform(cycle_hire_osm, 27700)
sel = st_is_within_distance(cycle_hire_P, cycle_hire_osm_P, dist = 20)
summary(lengths(sel) > 0)
# shows that there are 438 points within the threshold distance of cycle_hire_osm_P

# retrieve values associated with these within-threshhold points
z = st_join(cycle_hire_P, cycle_hire_osm_P, join = st_is_within_distance, dist = 20)
nrow(cycle_hire)  
nrow(z)  

# aggregate values for overlapping points & return the mean
z = z %>% 
  group_by(id) %>% 
  summarize(capacity = mean(capacity))
nrow(z) == nrow(cycle_hire)

plot(cycle_hire_osm["capacity"])
plot(z["capacity"])

# 4.2.5 Spatial Data Aggregation

# New Zealand - find the average height of high points in each region
nz_avheight = aggregate(x = nz_height, by = nz, FUN=mean)
plot(nz_avheight)

nz_avheight2 = nz %>%
  st_join(nz_height) %>%
  group_by(Name) %>%
  summarize(elevation = mean(elevation, na.rm = TRUE))
plot(nz_avheight2)

# Area weighted spatial interpolation - st_interpolate_aw()
agg_aw = st_interpolate_aw(incongruent[,"value"], aggregating_zones,
                           extensive = TRUE)
agg_aw$value

# 4.2.6 Distance Relations
# Calculate distaince between two objects

# Find distance between highest point in NZ and centroid of Canterbury region
nz_highest = nz_height %>% top_n(n=1, wt = elevation)
canterbury_centroid = st_centroid(canterbury)
st_distance(nz_highest, canterbury_centroid)

# Find distance between first 3 features of nz_height and Otago & Canterbury
co = filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)
# indicates that 2nd and 3rd highest points may be IN Otago?
plot(st_geometry(co)[2])
plot(st_geometry(nz_height)[2:3], add=TRUE)

# 4.3 Spatial Operations on Raster Data

# 4.3.1 Spatial Subsetting

# Translate coordinates (raster location) into a cell ID in order to use coordinates for subsetting
id = cellFromXY(elev, xy = c(0.1, 0.1))
elev[id]

# Raster objects can be subset with another raster object
clip = raster(xmn = 0.9, xmx = 1.8, ymn = -0.45, ymx = 0.45,
              res= 0.3, vals = rep(1,9))

elev[clip]

# Mask another raster with same extent and resolution
# Create raster mask
rmask = elev
values(rmask) = sample(c(NA, TRUE), 36, replace = TRUE) # mask object with values randomly assigned as NA and TURE

# Spatial subsetting - keep values of elev which are TRUE in rmask (mask elev with rmask)
elev[rmask, drop = FALSE]
mask(elev, rmask)
overlay(elev, rmask, fun = "max")

# 4.3.2 Map Algebra

rcl = matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
recl = reclassify(elev, rcl = rcl)

# Raster algebra
elev + elev
elev^2
log(elev)
elev > 5

# 4.3.4 Focal Operations

# Spatial filtering with focal()
r_focal = focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min)

# 4.3.5 Zonal Operations

# Find the mean elevation for each grain size class using zonal()
z = zonal(elev, grain, fun = "mean") %>%
  as.data.frame()
z

# 4.3.6 Global Operations and Distances

# 4.3.7 Map Algebra Counterparts in Vector Processing

# Download SRTM elevation data for Austria and Switzerland
aut = getData("alt", country = "AUT", mask = TRUE)
ch = getData("alt", country = "CHE", mask = TRUE)

# Merge rasters into one
aut_ch = merge(aut, ch)
plot(aut_ch)

# EXERCISES

# Calculate NDVI of a LANDSAT image

# Load landsat image
landsat = system.file("raster/landsat.tif", package="spDataLarge")

# Documentation on landsat file, including NDVI bands: https://github.com/Nowosad/spDataLarge/blob/master/R/rasters.R
# This is a dataset containing the four bands (2, 3, 4, 5) of the Landsat 8 image for
# the area of Zion National Park

# make landsat a brick
landsat_br = brick(landsat)
landsat_br 
# 4 layers:
# NIR = 5 = landsat.4 
# Red = band 4 = landsat.3

# Calculate NDVI = (NIR - Red) / (NIR + Red)
NDVI <- (landsat_br[[4]] - landsat_br[[3]]) / (landsat_br[[4]] + landsat_br[[3]])
plot(NDVI, main = "NDVI of Zion National Park")

library(RQGIS)
data(dem, package = "RQGIS")
summary(dem)
