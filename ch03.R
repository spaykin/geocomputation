library(sf)
library(raster)
library(dplyr)
library(stringr)
library(tidyr)

remotes::install_github("nowosad/spData@v0.34")

library(spData)

methods(class = "sf")

world <- load(file = "world.rda")

# 3.1 Reviewing basic R functions

dim(world) # 2 dimensional object, with rows and columns
nrow(world) # 177 rows
ncol(world) # 11 columns

world_df = st_drop_geometry(world) # remove the geometry (extract attributes)
class(world_df)

# 3.2 Vector Attribute Subsetting

world[1:6, ] # subset rows by position
world[, 1:3]
world[, c("name_long", "lifeExp")]

plot(world)
