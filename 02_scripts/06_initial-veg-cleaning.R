# Date Created: 2026-09-07 
# Date last modified: 2026-09-07
# Created by: Siena Blier

###############################################################################
## Initial veg data cleaning.
###############################################################################

# 0. Load libraries -------------------------------------------------------

library(dplyr)
library(sf)
#install.packages("renv")
library(renv)

# Initialize renv 
#renv::init()
renv::snapshot()
renv::restore()

# 1. Load data ------------------------------------------------------------

# Emma's data
ObsUrb.trees <- st_read("01_rawdata/veg/fieldmap/20260915_ArbresObsUrbFinal.gpkg")

# Data from my fieldmap
new.trees <- st_read("01_rawdata/veg/fieldmap/20260917_2026trees.gpkg")

# Public Tree Inventory Data
public.trees <- st_read("01_rawdata/veg/fieldmap/20260915_ArbresPublicsFinal.gpkg")

# Shrub data
shrubs <- st_read("01_rawdata/veg/fieldmap/20260917_2026shrubs.gpkg")

# ARU buffers
aru.buffers <- st_read("01_rawdata/veg/fieldmap/20261007_arubuffers.gpkg")

# CRS is NAD83 / MTM zone 8 for all, which is good

# 1. Remove points outside ARU buffers ------------------------------------

# Check if trees/shrubs are in the ARU buffers with st_intersects
# Only keep trees/shrubs within our buffers


# Emma's data
in.buffer = lengths(st_intersects(ObsUrb.trees, aru.buffers))>0
ObsUrb.in <- ObsUrb.trees[in.buffer,]

# My data
in.buffer = lengths(st_intersects(new.trees, aru.buffers))>0
new.in <- new.trees[in.buffer,]

# Public tree data
in.buffer = lengths(st_intersects(public.trees, aru.buffers))>0
public.in <- public.trees[in.buffer,]

# Shrub data
in.buffer = lengths(st_intersects(shrubs, aru.buffers))>0
shrubs.in <- shrubs[in.buffer,]

# Tidy environment
rm(ObsUrb.trees,new.trees,public.trees,shrubs)


# 2. Add Site_ID to all points --------------------------------------------

# Emma's data
ObsUrb.in$intersection <- st_intersects(ObsUrb.in,aru.buffers) %>% unlist()
ObsUrb.in$Site_ID <- aru.buffers$ARU_ID[ObsUrb.in$intersection]

# Public tree data
public.in$intersection <- st_intersects(public.in,aru.buffers) %>% unlist()
public.in$Site_ID <- aru.buffers$ARU_ID[public.in$intersection]

# My tree data
new.in$intersection <- st_intersects(new.in,aru.buffers) %>% unlist()
new.in$Site_ID <- aru.buffers$ARU_ID[new.in$intersection]

# Shrub data
shrubs.in$intersection <- st_intersects(shrubs.in,aru.buffers) %>% unlist()
shrubs.in$Site_ID <- aru.buffers$ARU_ID[shrubs.in$intersection]

# Save these files
st_write(ObsUrb.in,"03_outdata/shapefiles/ObsUrb.in.shp", append = FALSE, delete_layer = TRUE)
st_write(public.in,"03_outdata/shapefiles/public.in.shp", append = FALSE, delete_layer = TRUE)
st_write(new.in,"03_outdata/shapefiles/new.in.shp", append = FALSE, delete_layer = TRUE)
st_write(shrubs.in,"03_outdata/shapefiles/shrubs.in.shp", append = FALSE, delete_layer = TRUE)


# 3. Remove null entries --------------------------------------------------

# My trees

# From statology: https://www.statology.org/remove-empty-rows-in-r/
df[rowSums(is.na(df)) != ncol(df), ]

# Shrub layer
