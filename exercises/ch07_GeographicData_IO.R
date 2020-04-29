# Author: Susan Paykin
# Date: April 29, 2020
# Geocomputation in R
# for GIS III: University of Chicago, Spring 2020

########################################
#### Chapter 7: Geographic Data I/O ####
########################################

library(sf)
library(raster)
library(tidyverse)
library(spData)

####################################
##### 7.2 Retrieving Open Data #####
####################################

# Download files hosted on static URLs - use download.file()
download.file(url = "https://nrdata.nps.gov/programs/lands/nps_boundary.zip",
              destfile = "nps_boundary.zip")
unzip(zipfile = "nps_boundary.zip")
usa_parks = st_read(dsn = "temp/Current_Shapes/Data_Store/06-06-12_Posting/nps_boundary.shp")


########################################
##### 7.3 Geographic Data Packages #####
########################################

# Selected R packages for geographic data retrieval

#getlandsat()
#osmdata()
#raster() - getData() importas admin, elevation, WorldClim data
#rnaturalearth() - access to natural earth vector and raster
#rnoaa() - NOAA cliamte data
#rWBclimate() - access World Bank climate data

# Country borders using ne_countries()
library(rnaturalearth)
usa = ne_countries(country = "United States of America") # United States borders
class(usa)

# convert into sf object
usa_sf = st_as_sf(usa)
class(usa_sf)

# download series of rasters - multilayer object of class "RasterStack"
library(raster)
worldclim_prec = getData(name = "worldclim", var = "prec", res = 10)
class(worldclim_prec)

# osmdata package, function opq() 
library(osmdata)
parks = opq(bbox = "leeds uk") %>%
  add_osm_feature(key = "leisure", value = "parks") %>%
  osmdata_sf()

#######################################
##### 7.4 Geographic Web Services #####
#######################################

# getCapabilities

base_url = "http://www.fao.org"
endpoint = "/figis/geoserver/wfs"
q = list(request = "GetCapabilities")
res = httr::GET(url = httr::modify_url(base_url, path = endpoint), query = q)
res$url

txt = httr::content(res, "text")
xml = xml2::read_xml(txt)

# Data can be download from WFS [web feature service] services with the GetFeature request and a specific typeName

qf = list(request = "GetFeature", typeName = "area:FAO_AREAS")
file = tempfile(fileext = ".gml")
httr::GET(url = base_url, path = endpoint, query = qf, httr::write_disk(file))
fao_areas = sf::read_sf(file)

# packages for working with OWS servives in general, WFS, and SOS [sensor observation service]
# only ows4R is on CRAN

library(ows4R)
wfs = WFSClient$new("http://www.fao.org/figis/geoserver/wfs",
                    serviceVersion = "1.0.0", logger = "INFO")
fao_areas = wfs$getFeatures("area:FAO_AREAS")

############################
##### 7.5 File Formats #####
############################

# GDAL - pronounced "goo-dal" with "oo" in reference to object orientation 
# Geospatial Data Abstraction Library - resolves issues related to incompatibility btwn geographic file formats
# provides access to more than 200 vector and raster data formats, including:

# .shp - vector
# .geojson - vector
# .kml - developed for use with Google Earth - vector
# .gpx - exchange of GPS data - vector
# .tif/.tiff - raster format 
# .asc - Arc ASCII - raster
# .gri, .grd - native raster format or R package raster - raster
# .splite - SQLite/SpatiaLite - vector and raster
# .gdb - ESRI fileGDP - spatial/nonspatial objects created by ArcGIS - vector and raster
# .gpkg - database container based on SQLite - vector and raster


##############################
##### 7.6 Data Input (I) #####
##############################

##### 7.6.1 Vector Data #####

# st_read() - main function for loading vector data

sf_drivers = st_drivers()
head(sf_drivers, n = 5)
tail(sf_drivers, n = 5)

vector_filepath = system.file("shapes/world.gpkg", package = "spData")
world = st_read(vector_filepath)

# CSV format - to read in CSVs as spatial objects, specify X and Y columns (or cols representing coordinates)

cycle_hire_txt = system.file("misc/cycle_hire_xy.csv", package = "spData")
cycle_hire_xy = st_read(cycle_hire_txt, options = c("X_POSSIBLE_NAMES=X",
                                                    "Y_POSSIBLE_NAMES=Y"))

# Alternatively a single column can also contain geometry ifo
# Well-known text (WKT), well-known binary (WKB), and GeoJSON formats
# world_wkt.csv has column named WKT representing polygons of countries
# Use read_sf() (tidyverse version of st_read(), resulting data frame is a tibble)

world_txt = system.file("misc/world_wkt.csv", package = "spData")
world_wkt = read_sf(world_txt, options = "GEOM_POSSIBLE_NAMES=WKT")
# the same as
world_wkt = st_read(world_txt, options = "GEOM_POSSIBLE_NAMES=WKT", 
                    quiet = TRUE, stringsAsFactors = FALSE, as_tibble = TRUE)

# Reading in KML files 
# KML files store geo data in XML format
# XML is a format for creation of web pages and transfer of data in application-independent way

u = "https://developers.google.com/kml/documentation/KML_Samples.kml"
download.file(u, "KML_Samples.kml")
st_layers("KML_Samples.kml")

# Choose first layer "Placemarks" and say so with the help of "layer" parameter in read_sf()
kml = read_sf("KML_Samples.kml", layer = "Placemarks")
kml

##### 7.6.2 Raster Data #####

# Reading in raster data with raster()
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
single_layer = raster(raster_filepath)

# Read in single band from multilayer file, use band parameter
multilayer_filepath = system.file("raster/landsat.tif", package = "spDataLarge")
band3 = raster(multilayer_filepath, band = 3)

# Read in all bands with brick() or stack()
multilayer_brick = brick(multilayer_filepath)
multilayer_stack = stack(multilayer_filepath)


###############################
##### 7.7 Data Output (O) #####
###############################

##### 7.7.1 Vector Data #####

# st_write() is counterpart to st_read()
# allows you to write to vector file formats i.e. .geojson, .shp and .gpkg

st_write(obj = world, dsn = "world.gpkg")

# write again to same layer, use the append argument
st_write(obj = world, dsn = "world.gpkg", append = TRUE)
st_write(obj = world, dsn = "world.gpkg", append = FALSE)

# write spatial data to a test file, like .csv
st_write(cycle_hire_xy, "cycle_hire_xy.csv", layer_options = "GEOMETRY=AS_XY")
st_write(world_wkt, "world_wkt.csv", layer_options = "GEOMETRY=AS_WKT")

##### 7.7.2 Raster Data #####

# writeRaster() saves raster* objects to files on disk
# 9 different data types when saving a raster: 
# LOG1S, INT1S, INT1U, INT2S, INT2U, INT4S, INT4U, FLT4S, and FLT8S

#By default, writeRaster() saves outputs in its native format as .grd files, when a file extension is invalid or missing. 
# Other file formats can be specified by changing the extension of the output file name.
# Naming a file *.tif will create a GeoTIFF file, as demonstrated below:

writeRaster(single_layer, filename = "my_raster.tif", datatype = "INT2U")

# Set different format options by providing GDAL parameters to the options argument
# GeoTIFF files can be compressed using COMPRESS
writeRaster(x = single_layer,
            filename = "my_raster.tif",
            datatype = "INT2U",
            options = c("COMPRESS=DEFLATE"),
            overwrite = TRUE)

##############################
##### 7.8 Visual Outputs #####
##############################

png(filename = "lifeExp.png", width = 500, height = 350)
plot(world["lifeExp"])
dev.off()

# saving tmap graphic outputs to different formats
library(tmap)
tmap_obj = tm_shape(world) + tm_polygons(col = "lifeExp")
tmap_save(tm = tmap_obj, filename = "lifeExp_tmap.png")

# save interactive maps created in mapview packages as an HTML file or image using mapshot()
library(mapview)

mapview_obj = mapview(world, zcol = "lifeExp", legend = TRUE)

mapshot(mapview_obj, file = "my_interactive_map.html")

#########################
##### 7.9 Exercises #####
#########################

# 2. Name at least two differences between read_sf() and the more well-known function st_read().

# read_sf() is the tidyverse "version" of st_read(). 
# With read_sf(), strings are read as characters instead of factors, resulting in a tibble (the updated version of a data frame via tidyverse packages).
# Additionally, with st_read(), the driver and layer into is printed to the console, 
# while read_sf() is quiet and does not print unless you tell it to with the "quiet = FALSE" option.

# example
world_txt = system.file("misc/world_wkt.csv", package = "spData")
world_wkt = read_sf(world_txt, options = "GEOM_POSSIBLE_NAMES=WKT")
# versus
world_wkt = st_read(world_txt, options = "GEOM_POSSIBLE_NAMES=WKT")

# 3. Read the cycle_hire_xy.csv file from the spData package as a spatial object.
# What is a geometry type of the loaded object?

cycle_hire_csv <- system.file("misc/cycle_hire_xy.csv", package = "spData")
cycle_hire_xy <- st_read(cycle_hire_csv, options = c("X_POSSIBLE_NAMES=X",
                                                    "Y_POSSIBLE_NAMES=Y"))
st_geometry(cycle_hire_xy)

# The geometry of the sf object cycle_hire_xy is POINTS. 

