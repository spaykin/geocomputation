# Author: Susan Paykin
# Date: May 6, 2020
# Geocomputation in R
# for GIS III: University of Chicago, Spring 2020

########################################
##### Chapter 8: Making Maps in R #####
########################################

library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)

library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(shiny)   # for web applications

####################################
######### 8.2 Static Maps ##########
####################################

##### 8.2.1 tmap basics #####

# like ggplot2, tmap is based on "grammar of graphics"
# separation between the input data and the aestethics

# Add fill layer to nz shape
tm_shape(nz) +
  tm_fill() 
# Add border layer to nz shape
tm_shape(nz) +
  tm_borders() 
# Add fill and border layers to nz shape
tm_shape(nz) +
  tm_fill() +
  tm_borders() 

# Quick Thematic Maps qtm() - equivalent to tm_shape() + tm_fill() + tm_borders()
# quick, but makes aesthetics harder to control
qtm(nz)


##### 8.2.2 Map Objects #####

# store objects representing maps
map_nz = tm_shape(nz) + tm_polygons()
class(map_nz)

# adding new shapes - creating maps with multiple shapes and layers
# no limit to the number of shapes or layers that can be added to tmap objects
map_nz1 = map_nz +
  tm_shape(nz_elev) + tm_raster(alpha = 0.7)
map_nz1

# adding more - NZ's territorial waters and adding lines
nz_water = st_union(nz) %>% st_buffer(22200) %>% 
  st_cast(to = "LINESTRING")
map_nz2 = map_nz1 +
  tm_shape(nz_water) + tm_lines()
map_nz2

# adding layer representing high points with tm_dots()
map_nz3 = map_nz2 +
  tm_shape(nz_height) + tm_dots()
map_nz3

# arrange multiple maps in a single "metaplot" with tmap_arrange()
tmap_arrange(map_nz1, map_nz2, map_nz3)

##### 8.2.3 Aesthetics #####

# tmap accepts aesthetic arguments atht are either variable fields (based on col names) or constant values
# most common: col, alpha, lwd, lty (line type)

ma1 = tm_shape(nz) + tm_fill(col = "red")
ma2 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3)
ma3 = tm_shape(nz) + tm_borders(col = "blue")
ma4 = tm_shape(nz) + tm_borders(lwd = 3)
ma5 = tm_shape(nz) + tm_borders(lty = 2)
ma6 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3) +
  tm_borders(col = "blue", lwd = 3, lty = 2)
tmap_arrange(ma1, ma2, ma3, ma4, ma5, ma6)

# tmap aes arguments will not accept a numeric vector
plot(st_geometry(nz), col = nz$Land_area)  # works
tm_shape(nz) + tm_fill(col = nz$Land_area) # fails

# Instead, col and other aes arguments requires a character string naming an attribute
tm_shape(nz) + tm_fill(col = "Land_area")

# Change title - use expression() and title =
legend_title = expression("Area (km"^2*")")
map_nza = tm_shape(nz) +
  tm_fill(col = "Land_area", title = legend_title) + tm_borders()
map_nza

##### 8.2.4 Color Settings #####

# default setting uses "pretty" breaks
# use "breaks =" to set breaks
# n sets numeber of bins
# palette defines color scheme, i.e. BuGn

tm_shape(nz) + tm_polygons(col = "Median_income")
breaks = c(0, 3, 4, 5) * 10000
tm_shape(nz) + tm_polygons(col = "Median_income", breaks = breaks)
tm_shape(nz) + tm_polygons(col = "Median_income", n = 10)
tm_shape(nz) + tm_polygons(col = "Median_income", palette = "BuGn")

# Useful break styles

# style = "pretty", the default setting, rounds breaks into whole numbers where possible and spaces them evenly;
# style = "equal" divides input values into bins of equal range and is appropriate for variables with a uniform distribution (not recommended for variables with a skewed distribution as the resulting map may end-up having little color diversity);
# style = "quantile" ensures the same number of observations fall into each category (with the potential downside that bin ranges can vary widely);
# style = "jenks" identifies groups of similar values in the data and maximizes the differences between categories;
# style = "cont" (and "order") present a large number of colors over continuous color fields and are particularly suited for continuous rasters ("order" can help visualize skewed distributions);
# style = "cat" was designed to represent categorical values and assures that each category receives a unique color.

# Palette colors

tmaptools::palette_explorer()

# Categorial, Sequential, Diverging
# consider: perceptibility and accessibility.
# Colors should match our perceptions. View colors through our cultural lenses. 

# sequential examples
tm_shape(nz) + tm_polygons("Population", palette = "Blues")
tm_shape(nz) + tm_polygons("Population", palette = "YlOrBr")

##### 8.2.5 Layouts #####

# Additional elements - tm_compass() and tm_scale_bar() !!!
map_nz + 
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)

# Change a variety of settings in tm_layout()
map_nz + tm_layout(title = "New Zealand")
map_nz + tm_layout(scale = 5) # scale parameter
map_nz + tm_layout(bg.color = "lightblue") # background color
map_nz + tm_layout(frame = FALSE) # remove frame

# Frame width (frame.lwd) and an option to allow double lines (frame.double.line)
# Margin settings including outer.margin and inner.margin
# Font settings controlled by fontface and fontfamily
# Legend settings including binary options such as legend.show (whether or not to show the legend) legend.only (omit the map) and legend.outside (should the legend go outside the map?), as well as multiple choice settings such as legend.position
# Default colors of aesthetic layers (aes.color), map attributes such as the frame (attr.color)
# Color settings controlling sepia.intensity (how yellowy the map looks) and saturation (a color-grayscale)

# High-level styles, using tm_style() for stylized maps
map_nza + tm_style("bw")
map_nza + tm_style("classic")
map_nza + tm_style("cobalt")
map_nza + tm_style("col_blind")

# Preview of predefined styles
# tmap_style_catalog() 
# takes time to run

##### 8.2.6 Faceted Maps #####

# Also referred to as 'small multiples'
# Useful as foundation for animated maps
# Composted of many maps arranged side-by-side, sometimes stacked vertically.
# Visualization of how spatial relationships change with respect to another variable, such as time.

urb_1970_2030 = urban_agglomerations %>% 
  filter(year %in% c(1970, 1990, 2010, 2030))

tm_shape(world) +
  tm_polygons() +
  tm_shape(urb_1970_2030) +
  tm_symbols(col = "black", border.col = "white", size = "population_millions") +
  tm_facets(by = "year", nrow = 2, free.coords = FALSE)

# Shapes that do not have a facet variable are repeated (the countries in world in this case)
# The by argument which varies depending on a variable (year in this case).
# The nrow/ncol setting specifying the number of rows and columns that facets should be arranged into
# The free.coords parameter specifying if each map has its own bounding box

##### 8.2.7 Inset Maps #####

# Smaller map rendered within or next to the main map

# Example: Inset map shows where main map in relation to all NZ
# Step 1: define area of interest, create nz_region
nz_region = st_bbox(c(xmin = 1340000, xmax = 1450000,
                      ymin = 5130000, ymax = 5210000),
                    crs = st_crs(nz_height)) %>% 
  st_as_sfc()

# Step 2: create base map showing NZ's Southern Alps region
nz_height_map = tm_shape(nz_elev, bbox = nz_region) +
  tm_raster(style = "cont", palette = "YlGn", legend.show = TRUE) +
  tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 1) +
  tm_scale_bar(position = c("left", "bottom"))
nz_height_map

# Step 3: create inset map
nz_map = tm_shape(nz) + tm_polygons() +
  tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 0.1) + 
  tm_shape(nz_region) + tm_borders(lwd = 3)
nz_map

# Step 4: combine two maps using viewport() from grid package
library(grid)
nz_height_map
print(nz_map, vp = viewport(0.8, 0.27, width = 0.5, height = 0.5))
# Arguments of vewiports() specify the center location (x and y) and size (width and height) of the inset map.

# Inset maps for one map of non-contiguous areas, i.e. US + Hawaii and Alaska

us_states_map = tm_shape(us_states, projection = 2163) + tm_polygons() + 
  tm_layout(frame = FALSE)

hawaii_map = tm_shape(hawaii) + tm_polygons() + 
  tm_layout(title = "Hawaii", frame = FALSE, bg.color = NA, 
            title.position = c("LEFT", "BOTTOM"))

alaska_map = tm_shape(alaska) + tm_polygons() + 
  tm_layout(title = "Alaska", frame = FALSE, bg.color = NA)

# Combine US map + inset maps
us_states_map
print(hawaii_map, vp = grid::viewport(0.35, 0.1, width = 0.2, height = 0.1))
print(alaska_map, vp = grid::viewport(0.15, 0.15, width = 0.3, height = 0.3))

####################################
######## 8.3 Animated Maps #########
####################################

# Using same tmap facetated map techniques, can create animated maps
# Some differences: 
# along = "year" is used instead of by = "year".
# free.coords = FALSE, which maintains the map extent for each map iteration.

urb_anim = tm_shape(world) + tm_polygons() + 
  tm_shape(urban_agglomerations) + tm_dots(size = "population_millions") +
  tm_facets(along = "year", free.coords = FALSE)

# Save as a .gif file with tmap_animation()
tmap_animation(urb_anim, filename = "urb_anim.gif", delay = 25)


####################################
####### 8.4 Interactive Maps #######
####################################

# Slippy or Interactive web maps using leaflet package

# tmap can create static and interactive maps
# change view to interactive by switching to view mode
# tmap_mode("view")
tmap_mode("view")
map_nz

# Specify basemap
map_nz + tm_basemap(server = "OpenTopoMap")

# Also works with faceted plots, using argument sync in tm_facets
# Produce multiple maps with synchronized zoom and pan settings

world_coffee = left_join(world, coffee_data, by = "name_long")
facets = c("coffee_production_2016", "coffee_production_2017")
tm_shape(world_coffee) + tm_polygons(facets) + 
  tm_facets(nrow = 1, sync = TRUE)

tmap_mode("plot")

# Using mapview as the quickest way to create interactive maps (not as specific as tmap)

library(systemfonts)
mapview(nz)

trails %>%
  st_transform(st_crs(franconia)) %>%
  st_intersection(franconia[franconia$district == "Oberfranken", ]) %>%
  st_collection_extract("LINE") %>%
  mapview(color = "red", lwd = 3, layer.name = "trails") +
  mapview(franconia, zcol = "district", burst = TRUE) +
  breweries

# Mapdeck - provides access to Uber's Deck.gl framework
# interactively visualize large datasets - up to millions of points

library(mapdeck)
set_token(Sys.getenv("pk.eyJ1Ijoic3BheWtpbiIsImEiOiJjazl2bGlpcHUwMGZkM2duemN3aGd3MmFlIn0.4Gt2FEIHR0jaBQjTyrGcDQ"))
crash_data = read.csv("https://git.io/geocompr-mapdeck")
crash_data = na.omit(crash_data)
ms = mapdeck_style("dark")

mapdeck(style = ms, pitch = 45, location = c(0, 52), zoom = 4) %>%
  add_grid(data = crash_data, lat = "lat", lon = "lng", cell_size = 1000,
           elevation_scale = 50, layer_id = "grid_layer",
           colour_range = viridisLite::plasma(6))

# Leaflet

pal = colorNumeric("RdYlBu", domain = cycle_hire$nbikes)
leaflet(data = cycle_hire) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(col = ~pal(nbikes), opacity = 0.9) %>% 
  addPolygons(data = lnd, fill = FALSE) %>% 
  addLegend(pal = pal, values = ~nbikes) %>% 
  setView(lng = -0.1, 51.5, zoom = 12) %>% 
  addMiniMap()


####################################
##### 8.5 Mapping Applications #####
####################################

library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(spData)   # loads the world dataset 

ui = fluidPage(
  sliderInput(inputId = "life", "Life expectancy", 49, 84, value = 80),
  leafletOutput(outputId = "map")
)

server = function(input, output) {
  output$map = renderLeaflet({
    leaflet() %>% 
      # addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
      addPolygons(data = world[world$lifeExp < input$life, ])})
}

shinyApp(ui, server)

# Coffee App

library(sf)
library(shiny)
library(spData)
library(leaflet)
library(tidyverse)

world_coffee = left_join(world, coffee_data)
pal = colorNumeric(palette = "RdYlBu", domain = c(0, 4000))

ui = fluidPage(
  sidebarPanel(
    sliderInput("range", "Coffee Production", 0, 4000,
                value = c(1000, 4000), step = 100),
    selectInput("year", "Year", c(2016, 2017)),
    checkboxInput("legend", "Show legend", FALSE)
  ),
  mainPanel(
    leafletOutput("map")
  )
)

server = function(input, output, session) {
  
  map_centre = st_centroid(world %>% filter(name_long == "Brazil")) %>% 
    st_coordinates()
  
  # This reactive expression returns a character string representing the selected variable
  yr = reactive({
    paste0("coffee_production_", input$year)
  })
  
  # Reactive expression for the data subset to what the user selected
  filteredData = reactive({
    world_coffee$Production = world_coffee[[yr()]]
    filter(world_coffee, Production >= input$range[1] &
             Production <= input$range[2])
  })
  
  output$map = renderLeaflet({
    # Things that do not change go here:
    leaflet() %>% addTiles() %>%
      setView(lng = map_centre[, "X"], map_centre[, "Y"], zoom = 2)
  })
  
  # Changes to the map performed in an observer
  observe({
    proxy = leafletProxy("map", data = filteredData()) %>% 
      clearShapes()
    # Show or hide legend
    proxy %>% clearControls() %>% addPolygons(fillColor = ~pal(Production))
    if (input$legend) {
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~Production)
    }
  })
}

shinyApp(ui, server)


####################################
#### 8.6 Other Mapping Packages ####
####################################

# Using plot() to combine raster and vector, use add=TRUE
g = st_graticule(nz, lon = c(170, 175), lat = c(-45, -40, -35))
plot(nz_water, graticule = g, axes = TRUE, col = "blue")
raster::plot(nz_elev / 1000, add = TRUE)
plot(st_geometry(nz), add = TRUE)

# ggplot2 can use geom_sf() for sf objects or geom_points()!!!
# ggplot plots graticules (network of lines) by default, override using:
# scale_x_continuous(), scale_y_continuous() or coord_sf(datum = NA)

g1 = ggplot() + geom_sf(data = nz, aes(fill = Median_income)) +
  geom_sf(data = nz_height) +
  scale_x_continuous(breaks = c(170, 175))
g1

g2 = ggplot() + geom_sf(data = nz, aes(fill = Median_income)) +
  geom_sf(data = nz_height)
g2

# Cartogram

# library(cartogram)
# nz_carto = cartogram_cont(nz, "Median_income", itermax = 5)
# tm_shape(nz_carto) + tm_polygons("Median_income")


####################################
######### 8.7 Exercises ###########
####################################

africa = world %>% 
  filter(continent == "Africa", !is.na(iso_a2)) %>% 
  left_join(worldbank_df, by = "iso_a2") %>% 
  dplyr::select(name, subregion, gdpPercap, HDI, pop_growth) %>% 
  st_transform("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25")

zion = st_read((system.file("vector/zion.gpkg", package = "spDataLarge")))
data(nlcd, package = "spDataLarge")

# 1. Create a map showing the geographic distribution of the Human Development Index (HDI) across Africa with base graphics (hint: use plot()) and tmap packages (hint: use tm_shape(africa) + ...).
# Name two advantages of each based on the experience.
# Name three other mapping packages and an advantage of each.
# Bonus: create three more maps of Africa using these three packages.

# base
plot(africa["HDI"])
# Advantages of base: simple! Uses the fewest lines of text. Generates a title and scale bar. 

# tmap
tm_shape(africa) +
  tm_polygons("HDI", palette = "BuPu") +
  tm_layout(frame = FALSE)
# Advantages of tmap: More attractive and customizable. Generates cleaner legend with variable title. 

# ggplot2 - strong user community; integrated into tidyverse universe
ggplot() + geom_sf(data = africa, aes(fill = HDI)) +
  scale_x_continuous()

# leaflet - nice basemap selection
leaflet(data = africa$HDI) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = st_transform(africa, 4326), fill = TRUE, color = "pink")
 
# mapview - good for quick interactive maps
africa_hdi <- mapview(africa, zcol = "HDI")
mapshot(africa_hdi, file = "mapview_africa.html")
africa_hdi

# 5. Create facet maps of countries in Eastern Africa:
# With one facet showing HDI and the other representing population growth (hint: using variables HDI and pop_growth, respectively)
# With a ‘small multiple’ per country

library(RColorBrewer)
display.brewer.all()

eastern <- africa %>% 
  filter(subregion == "Eastern Africa")

tm_shape(africa) +
  tm_polygons() +
  tm_shape(eastern) +
  tm_fill(col=c('HDI', 'pop_growth'), palette='BuPu') +
  tm_borders(lwd = 1) +
  tm_facets(nrow=1, free.coords=FALSE) +
  tm_layout(main.title = 'Development in East Africa', 
            title.position = c("center", "TOP"),
            frame = FALSE)


# 12. Visualize population growth in Africa. 
# Next, compare it with the maps of a hexagonal and regular grid created using the geogrid package.

library(geogrid)

pop <- tm_shape(africa) +
  tm_fill(col = "pop_growth", palette = "BuPu") +
  tm_borders(lwd = .5) +
  tm_layout(frame = FALSE, title = "Africa: Population Growth", title.position = c("left", "TOP"))
pop

hex <- assign_polygons(africa, calculate_grid(shape = africa, grid_type = "hexagonal", 
                                       seed = 999))

hex_map <- tm_shape(hex) +
  tm_fill(col='pop_growth', palette='BuPu') +
  tm_borders(lwd = .5) +
  tm_layout(frame = FALSE)
hex_map

grid <- assign_polygons(africa, calculate_grid(shape=africa, grid_type="regular", 
                                               seed=999))

grid_map <- tm_shape(grid) +
  tm_fill(col='pop_growth', palette="BuPu") +
  tm_borders(lwd = .5) +
  tm_layout(frame = FALSE)
grid_map

tmap_arrange(pop, hex_map, grid_map, ncol = 1)
