#install.packages("githubinstall")
#githubinstall::gh_install_packages("rtweet",  ref = "5ef897e",dependencies=TRUE, ask= FALSE)
#library(rtweet)
#hrc <- search_tweets(q = "hillaryclinton", n = 1000)

library(RCurl)
library(rjson)
library(geojsonio)
library(httr)
library(sf)
library(rlist)
library(rgdal)
library(tidyverse)
library(tmap)
library(leaflet)

# Read in Loudon County Soil Point Features from web
loudon <- geojson_read("https://opendata.arcgis.com/datasets/e77b3da29ea3447bb401ecaf1288eade_0.geojson",
                     what = "sp")

# Initial plot
plot(loudon)

# View data
loudon_df <- loudon@data

# Filter for rocky, clay, gravely, sandy, stoney, and wet soil features
loudon_df2 <- 
  loudon_df %>%
  filter(SP_SYMBOL == c("ROC", "CLA", "GRA", "SAN", "STN", "WET"))

loudon_plot <- 
  loudon[loudon@data$SP_SYMBOL == c("ROC", "CLA", "GRA", "SAN", "STN", "WET"), ]

# View Interactive Map
tmap_mode("view")

tm_shape(loudon_plot) +
  tm_symbols(col = "SP_SYMBOL", size = .2,
             border.alpha = 0.5) +
  tm_layout(frame = FALSE) +
  tm_legend(title = "Soil Point Features") +
  tm_basemap(leaflet::providers$OpenStreetMap.BlackAndWhite)


