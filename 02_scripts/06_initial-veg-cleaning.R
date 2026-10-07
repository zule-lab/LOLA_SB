# Date Created 
# Date last modified: 24APRIL2026
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
renv::init()
renv::snapshot()

# 1. Load data ------------------------------------------------------------

# Emma's data
ObsUrb.trees <- st_read("01_rawdata/veg/fieldmap/20260915_ArbresObsUrbFinal.gpkg")

# Data from my fieldmap
new.trees <- st_read("01_rawdata/veg/fieldmap/20260915_ArbresPublicsFinal.gpkg")

# Public Tree Inventory Data
public.trees <- st_read("01_rawdata/veg/fieldmap/20260917_2026trees.gpkg")

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

names(ObsUrb.in)

Site_ID = aru.buffers[16,"ARU_ID"]

ObsUrb.w.ID <- 
 
