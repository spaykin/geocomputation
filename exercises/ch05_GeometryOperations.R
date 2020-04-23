# Author: Susan Paykin
# Date: April 22, 2020
# Geocomputation in R
# for GIS III: University of Chicago

######################################
### Chapter 5: Geometry Operations ###
######################################

# Load libraries
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)

###############################################
### 5.2 Geometric Operations on Vector Data ###
###############################################

##### 5.2.1 Simplification ######

# Definition: process for generalizatino of vector objects usually for smaller scale maps
# Also: reduce amount of memory, simplify geometries before using as interactive maps

# st_simplify() 
seine_simp = st_simplify(seine, dTolerance = 2000) #2000 meters
plot(seine_simp)

object.size(seine)
object.size(seine_simp)

us_states2163 = st_transform(us_states, 2163)
us_states_simp1 = st_simplify(us_states2163, dTolerance = 100000) #100 km

# ms_simplify() overcomes the issue of overlapping and "holy" areal units
us_states2163$AREA = as.numeric(us_states2163$AREA)
us_states_simp2 = rmapshaper::ms_simplify(us_states2163, keep = 0.01, keep_shapes = TRUE)

plot(us_states)
plot(us_states_simp1)
plot(us_states_simp2)

##### 5.2.2 Centroids #####

# st_centroid()
nz_centroid = st_centroid(nz)
plot(nz_centroid["geom"])

seine_centroid = st_centroid(seine)
plot(seine_centroid["geometry"])

# points on surface - guarantee the point will be in the parent object (good for labeling irregular multipolygon objects)
nz_pos = st_point_on_surface(nz)
plot(nz_pos["geom"])

seine_pos = st_point_on_surface(seine)
plot(seine_pos["geometry"])

##### 5.2.3 Buffers #####

# st_buffer()
seine_buff_5km = st_buffer(seine, dist = 5000) #500m buffer
plot(seine_buff_5km)

sein_buff_50km = st_buffer(seine, dist = 50000) #50km buffer
plot(sein_buff_50km)

##### 5.2.4 Affine Transformations #####

# transformation that preserves lines and parallelism
nz_sfc = st_geometry(nz)

# Shift y coordinates by 100,000 meters, but do not shift x coordinates
nz_shift = nz_sfc + c(1, 100000)

# Scaling
nz_centroid_sfc = st_centroid(nz_sfc)
nz_scale = (nz_sfc - nz_centroid_sfc) * 0.5 + nz_centroid_sfc # sizes are reduced by half (0.5)

# Rotation matrix - rotates points in clockwise direction
rotation = function(a){
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 

# rotation() function accepts one argument - rotation angle in degrees
nz_rotate = (nz_sfc - nz_centroid_sfc) * rotation(30) + nz_centroid_sfc

##### 5.2.5 Clipping #####

# spatial subsetting that involves changes to geometry column

# two overlapping circles with center point one unit away from one another; radius = 1
b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
b = st_buffer(b, dist = 1) # convert points to circles
plot(b)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y")) # add text

# st_intersection() - select where the circles intersect, both x and y
x = b[1]
y = b[2]
x_and_y = st_intersection(x, y)
plot(b)
plot(x_and_y, col = "lightgrey", add = TRUE) # color intersecting area

# subset points that cover the bounding box of circles x and y
bb = st_bbox(st_union(x, y))
box = st_as_sfc(bb)
set.seed(2017)
p = st_sample(x = box, size = 10)
plot(box)
plot(x, add = TRUE)
plot(y, add = TRUE)
plot(p, add = TRUE)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y"))

# using clipped polygon
sel_p_xy = st_intersects(p, x, sparse = FALSE)[, 1] &
  st_intersects(p, y, sparse = FALSE)[, 1]
p_xy1 = p[sel_p_xy]
p_xy2 = p[x_and_y]
identical(p_xy1, p_xy2)

##### 5.2.6 Geometry Unions #####

regions = aggregate(x = us_states[, "total_pop_15"], by = list(us_states$REGION),
                    FUN = sum, na.rm = TRUE)
plot(regions)

regions2 = us_states %>% group_by(REGION) %>%
  summarize(pop = sum(total_pop_15, na.rm = TRUE))
plot(regions2)

# st_union()
us_west = us_states[us_states$REGION == "West", ]
us_west_union = st_union(us_west)

texas = us_states[us_states$NAME == "Texas", ]
texas_union = st_union(us_west_union, texas)
plot(texas_union)

##### 5.2.7 Type Transformations #####

# geometry casting - st_cast()
multipoint = st_multipoint(matrix(c(1, 3, 5, 1, 3, 1), ncol = 2))
plot(multipoint)
linestring = st_cast(multipoint, "LINESTRING")
polyg = st_cast(multipoint, "POLYGON")

multilinestring_list = list(matrix(c(1, 4, 5, 3), ncol = 2), 
                            matrix(c(4, 4, 4, 1), ncol = 2),
                            matrix(c(2, 4, 2, 2), ncol = 2))
multilinestring = st_multilinestring((multilinestring_list))
multilinestring_sf = st_sf(geom = st_sfc(multilinestring))
multilinestring_sf

linestring_sf2 = st_cast(multilinestring_sf, "LINESTRING")
linestring_sf2

linestring_sf2$name = c("Riddle Rd", "Marshall Ave", "Foulke St")
linestring_sf2$length = st_length(linestring_sf2)
linestring_sf2

###############################################
### 5.3 Geometric Operations on Raster Data ###
###############################################

# Operations include the shift, flipping, mirroring, scaling, rotation or warping of images. 
# Necessary for a variety of applications including georeferencing, 
# used to allow images to be overlaid on an accurate map with a known CRS.

##### 5.3.1 Geometric Intersections #####

# keep matrix structure by setting drop parameter to FALSE; 
# returns raster object containing the cells whose midpoints overlap with clip
data("elev", package = "spData")
clip = raster(xmn = 0.9, xmx = 1.8, ymn = -0.45, ymx = 0.45,
              res = 0.3, vals = rep(1, 9))
elev[clip, drop = FALSE]

##### 5.3.2 Extent and Origin #####

# how to merge satellite imagery from different sensors with different projections and resolutions
# adds one row and two columns to each side of the raster while setting all new values to an elevation of 1000 meters
data(elev, package = "spData")
elev_2 = extend(elev, c(1, 2), value = 1000)
plot(elev_2)

# test different extents in algebraic operation
elev_3 = elev + elev_2 #returns error

# extend() function - extend elev to elev_2
elev_4 = extend(elev, elev_2)

# origin() returns coordinate of origin of raster (cell closest to coordinate 0,0)
origin(elev_4)

# change the origin
origin(elev_4) = c(0.25, 0.25)
plot(elev_4)
# and add the original raster
plot(elev, add = TRUE)

##### 5.3.3 Aggregation and Disaggregation #####

# decrease (aggregate()) or increase (disaggregate()) resolution of a raster

# change spatial resolution of dem raster (from RGIS package)
data("dem", package = "RQGIS")
plot(dem)
dem_agg = aggregate(dem, fact = 5, fun = mean)
plot(dem_agg)

# disaggregate() increases resolution; need to specify method
# method = "" gives all output cells value of input cell, hence duplicates values
# method = bilinear, is interpolation technique, uses 4 nearest pixel centers to compute average weighted by distnace

dem_disagg = disaggregate(dem_agg, fact = 5, method = "bilinear")
plot(dem_disagg)
plot(dem)

identical(dem, dem_disagg)

# resample() function allows you to align several raster properties inc. origin, extent, resolution
# add 2 rows and columns, i.e. change the extent
dem_agg = extend(dem_agg, 2)
dem_disagg_2 = resample(dem_agg, dem)
plot(dem_disagg_2)

#############################################
###### 5.4 Raster-Vector Interactions #######
#############################################

##### 5.4.1 Raster Cropping #####
# often the extent of raster input datasets is larger than area of interest
# raster cropping and masking can help unify the spatial extent of input data

# load raster and vector data; reproject zion vector data. srtm = raster representing elevation (meters above sea level) in SW Utah
srtm = raster(system.file("raster/srtm.tif", package = "spDataLarge"))
zion = st_read(system.file("vector/zion.gpkg", package = "spDataLarge"))
zion = st_transform(zion, projection(srtm))

# crop() reduces rectangular extent of first argument object based on extent of second argument object
srtm_cropped = crop(srtm, zion)
plot(srtm_cropped)

# mask() is different
# Setting updatevalue = 0 will set all pixels outside the national park to 0. 
# Setting inverse = TRUE will mask everything inside the bounds of the park
srtm_inv_masked = mask(srtm, zion, inverse = TRUE)
plot(srtm_inv_masked)

##### 5.4.2 Raster Extraction #####

# extract() function to extract value of raster cell at specific points
data("zion_points", package = "spDataLarge")
zion_points$elevation = raster::extract(srtm, zion_points)

raster::extract(srtm, zion_points, buffer = 1000)

zion_transect = cbind(c(-113.2, -112.9), c(37.45, 37.2)) %>%
  st_linestring() %>% 
  st_sfc(crs = projection(srtm)) %>% 
  st_sf()

transect = raster::extract(srtm, zion_transect, 
                           along = TRUE, cellnumbers = TRUE)

transect_df = purrr::map_dfr(transect, as_data_frame, .id = "ID")
transect_df

transect_coords = xyFromCell(srtm, transect_df$cell)
pair_dist = geosphere::distGeo(transect_coords)[-nrow(transect_coords)]
transect_df$dist = c(0, cumsum(pair_dist))

# extract() extract value of raster polygons
zion_srtm_values = raster::extract(x = srtm, y = zion, df = TRUE) 

group_by(zion_srtm_values, ID) %>%
  summarize_at(vars(srtm), list(~min(.), ~mean(.), ~max(.)))

# land cover dataset nlcd, counting occurances of categorical raster values within polygons
# extract zion land cover data
zion_nlcd = raster::extract(nlcd, zion, df = TRUE, factors = TRUE) 
# group by land cover categories
dplyr::select(zion_nlcd, ID, levels) %>% 
  tidyr::gather(key, value, -ID) %>%
  group_by(ID, key, value) %>%
  tally() %>% 
  tidyr::spread(value, n, fill = 0)

##### 5.4.3 Rasterization #####

# rasterize() - convert vector objections into raster
cycle_hire_osm_projected = st_transform(cycle_hire_osm, 27700)
raster_template = raster(extent(cycle_hire_osm_projected), resolution = 1000,
                         crs = st_crs(cycle_hire_osm_projected)$proj4string)

# 3 Approaches to Rasterization

# Approach 1 - absence or presence of something (binary)
# requires only one argument in addition to x and y (vector and raster objects): 
# a value to be transferred to all non-empty cells specified by field
ch_raster1 = rasterize(cycle_hire_osm_projected, raster_template, field = 1)
plot(ch_raster1)

# Approach 2 - fun argument specifies summary stats, such as count, to count number of points in each cell
ch_raster2 = rasterize(cycle_hire_osm_projected, raster_template, 
                       field = 1, fun = "count")
plot(ch_raster2)

# Approach 3 - capacity of each cell - sum the field ("capacity")
ch_raster3 = rasterize(cycle_hire_osm_projected, raster_template, 
                       field = "capacity", fun = sum)
plot(ch_raster3)

# California polygons and borders, create template raster with resolution of 0.5 degree
california = dplyr::filter(us_states, NAME == "California")
california_borders = st_cast(california, "MULTILINESTRING")
raster_template2 = raster(extent(california), resolution = 0.5,
                          crs = st_crs(california)$proj4string)

# Line rasterization
# All cells touched by a line get a value
california_raster1 = rasterize(california_borders, raster_template2) 
plot(california_raster1)

# Polygon rasterization
# All cells whose centroids are inside selector polygon get a value
california_raster2 = rasterize(california, raster_template2)
plot(california_raster2)

##### Spatial Vectorization #####

# convert spatially continuous raster data into spatially discrete vector data
# such as points, lines or polygons

# rasterToPoints() converts centroids of raster cells into point
elev_point = rasterToPoints(elev, spatial = TRUE) %>% #spatial=TRUE gets spatial object instead of matrix
  st_as_sf()
plot(elev_point)

# rasterToContour() creates contour lines representing continuous height or temperatures
data(dem, package = "RQGIS")
cl = rasterToContour(dem)
plot(dem, axes = FALSE)
plot(cl, add = TRUE)

# Isolines can be labeled
# create hillshade
hs = hillShade(slope = terrain(dem, "slope"), aspect = terrain(dem, "aspect"))
plot(hs, col = gray(0:100 / 100), legend = FALSE)
# overlay with DEM
plot(dem, col = terrain.colors(25), alpha = 0.5, legend = FALSE, add = TRUE)
# add contour lines
contour(dem, col = "white", add = TRUE)

# rasterToPolygons() converts rasters to polygons
grain_poly = rasterToPolygons(grain) %>% 
  st_as_sf() #convert foreign object to sf object
plot(grain_poly)

grain_poly2 = grain_poly %>% 
  group_by(layer) %>%
  summarize()
plot(grain_poly2)

############################
###### 5.5 Exercises #######
############################

# 4 - Most world maps have a north-up orientation. A world map with a south-up orientation could be created by a reflection (one of the affine transformations not mentioned in Section 5.2.4) of the world object’s geometry. 
# Write code to do so. Hint: you need to use a two-element vector for this transformation.
# Bonus: create an upside-down map of your country.

# create world map with south-up orientation
world_south <- st_geometry(world) * c(-1, -1)
plot(world_south)

# create south-up map of Russia
russia_south <- 
  world %>%
  filter(name_long == "Russian Federation") %>%
  st_geometry() * c(-1, -1)

plot(russia_south)

# 5 - Subset the point in p that is contained within x and y (see Section 5.2.5 and Figure 5.8).
# Using base subsetting operators.
# Using an intermediary object created with st_intersection().

x = b[1]
y = b[2]
x_and_y = st_intersection(x, y)
plot(b)
plot(x_and_y, col = "lightblue", add = TRUE) # color intersecting area

set.seed(1948)
p = st_sample(x = st_union(x, y), size = 5)
plot(x_and_y, col = "lightblue", add = TRUE)
plot(p, add = TRUE)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y"))

# base subsetting
p[x_and_y]
#POINT (0.6334506 1.118806)

#intermediary object
st_intersection(p, x_and_y)
#POINT (0.6334506 1.118806)

# 7 - Crop the ndvi raster using (1) the random_points dataset and (2) the ch dataset. 
# Are there any differences in the output maps? 
# Next, mask ndvi using these two datasets. Can you see any difference now? How can you explain that?

library(RQGIS)
data(random_points)
data(ndvi)
ch = st_combine(random_points) %>% 
  st_convex_hull()

# make convex hull polygon (ch) an sf object
ch1 <- st_as_sf(ch)

# crop with ch
ndvi_crop <- crop(ndvi, ch1)
plot(ndvi_crop)

# crop with random_points
ndvi_crop2 <- crop(ndvi, random_points)
plot(ndvi_crop2)

# mask with ch
ndvi_mask <- mask(ndvi, ch1)
plot(ndvi_mask)

# mask with random_points
ndvi_mask2 <- mask(ndvi, random_points)
plot(ndvi_mask2)
