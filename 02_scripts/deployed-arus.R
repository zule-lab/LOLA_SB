# Created: 11 May 2026
# Last modified: 28 May 2026
# Siena Blier
# Preparing shapefiles for QField map


# 0. Load libraries -------------------------------------------------------

library(dplyr)
library(sf)
library(mapview)


# 1. Load data ------------------------------------------------------------

deployment.data <- read.csv("01_rawdata/20260528_deployment-data.csv")
str(deployment.data)

# 2. ARU deployment data ---------------------------------------------

locations.only <- deployment.data %>% select(ARU.ID, Coordinates)
head(locations.only)

# Separate lat and long
locations.only.split <- tidyr::separate(locations.only, Coordinates, c("latitude", "longitude"), sep = ",", remove = TRUE, convert = FALSE)

# Write SF
locations.shp <- st_as_sf(locations.only.split, coords = c("longitude", "latitude"), crs = 4326)

# Convert to EPSG:32188

# Check that it's possible
st_can_transform(st_crs(locations.shp), "EPSG:32188")

# Convert
locations.shp.32188 <- st_transform(locations.shp,"EPSG:32188")

# Check
st_crs(locations.shp.32188)

# Save
st_write(locations.shp.32188, "04_shapefiles/deployed-arus.shp", append = FALSE, delete_layer = TRUE)

# Clean up
rm(locations.shp, locations.only, locations.only.split)

# Make 50 m radius buffers around points

aru.buffers <- st_buffer(locations.shp.32188, 50)
mapview(aru.buffers)

# Export
st_write(aru.buffers, "04_shapefiles/aru-buffers.shp", append = FALSE, delete_layer = TRUE)


# 3. ArbresObsUrb data -------------------------------------------------------------

# Read
ArbresObsUrbJul31 <- st_read("04_shapefiles/ArbresObsUrbJul31.shp")

# Convert to EPSG:32188

# Check that it's possible
st_can_transform(st_crs(ArbresObsUrbJul31), "EPSG:32188")

# Convert
ArbresObsUrbJul31.32188 <- st_transform(ArbresObsUrbJul31,"EPSG:32188")

# Check
st_crs(ArbresObsUrbJul31.32188)

# Save
st_write(ArbresObsUrbJul31.32188, "04_shapefiles/ArbresObsUrbJul31_32188.shp", append = FALSE, delete_layer = TRUE)

# Clean up
rm(ArbresObsUrbJul31)

# keep only trees within my buffers 

in.my.sites = lengths(st_intersects(ArbresObsUrbJul31.32188, aru.buffers)) > 0
head(in.my.sites)
# Code from: https://gis.stackexchange.com/questions/394954/r-using-st-intersects-to-classify-points-inside-outside-and-within-a-buffer

ArbresObsUrb.filtered <- ArbresObsUrbJul31.32188[in.my.sites,]
mapview(ArbresObsUrb.filtered)

# Save
st_write(ArbresObsUrb.filtered,"04_shapefiles/ArbresObsUrbFinal.shp", append = FALSE, delete_layer = TRUE)


# 4. Public Tree Inventory Data ------------------------------------------------

# Read
arbres.publics <- read.csv("01_rawdata/arbres-publics.csv")

# Look at structure
str(arbres.publics)

# Keep only columns of interest
arbres.publics.reduced <- arbres.publics %>% select(Essence_latin, 
                                                    Essence_ang,DHP,Date_Releve, 
                                                    Coord_X, Coord_Y,Longitude,
                                                    Latitude)

str(arbres.publics.reduced)

sum(is.na(arbres.publics.reduced$Coord_X))
sum(is.na(arbres.publics.reduced$Coord_Y))

arbres.publics.filtered <- arbres.publics.reduced %>% filter(!is.na(Coord_Y))

# Convert to shapefile
arbres.publics.shp <- st_as_sf(arbres.publics.filtered, coords = c("Coord_X", "Coord_Y"), crs = 32188)

# Check CRS
st_crs(arbres.publics.shp)

# Export
st_write(arbres.publics.shp, "04_shapefiles/arbres-publics_32188.shp", append = FALSE, delete_layer = TRUE)

# Clean up
rm(arbres.publics, arbres.publics.reduced, arbres.publics.filtered)

# Only keep public trees within my buffers

in.my.sites.public = lengths(st_intersects(arbres.publics.shp, aru.buffers)) > 0
head(in.my.sites.public)
# Code from: https://gis.stackexchange.com/questions/394954/r-using-st-intersects-to-classify-points-inside-outside-and-within-a-buffer

arbres.publics.filtered.1 <- arbres.publics.shp[in.my.sites.public,]
mapview(arbres.publics.filtered.1)

# Remove public trees that are in ArbresObsUrb's sites

# Load ArbresObsUrb sites
ArbresObsUrb.buffers <- st_read("04_shapefiles/ArbresObsUrb/BufferedPlots200m.shp")

# check crs
st_crs(ArbresObsUrb.buffers)

# Project to 32188

# Check that it's possible
st_can_transform(st_crs(ArbresObsUrb.buffers), "EPSG:32188")

# Convert
ArbresObsUrb.buffers.32188 <- st_transform(ArbresObsUrb.buffers,"EPSG:32188")

# Check
st_crs(ArbresObsUrb.buffers.32188)

# View
mapview(ArbresObsUrb.buffers.32188)

# Find public trees that are in ArbresObsUrb sites
public.to.remove = lengths(st_intersects(arbres.publics.filtered.1, ArbresObsUrb.buffers.32188)) > 0
head(public.to.remove)
# Code from: https://gis.stackexchange.com/questions/394954/r-using-st-intersects-to-classify-points-inside-outside-and-within-a-buffer

sum(public.to.remove) # expect to lose 64 rows

arbres.publics.filtered.2 <- arbres.publics.filtered.1[!public.to.remove,]
nrow(arbres.publics.filtered.2) # expected number of rows

mapview(arbres.publics.filtered.2)

# Save
st_write(arbres.publics.filtered.2,"04_shapefiles/ArbresPublics/ArbresPublicsFinal.shp", append = FALSE, delete_layer = TRUE)

# CUTS --------------------------------------------------------------------


# Remove public trees that are in my TPI project sites

# Load TPI project sites
TPI.buffers <- st_read("04_shapefiles/TPI_data/TPI_aru-buffers.shp")

# check crs
st_crs(TPI.buffers)

# View
mapview(TPI.buffers)

# Find public trees that are in TPI sites
public.to.remove.1 = lengths(st_intersects(arbres.publics.filtered.2, TPI.buffers)) > 0
head(public.to.remove.1)
# Code from: https://gis.stackexchange.com/questions/394954/r-using-st-intersects-to-classify-points-inside-outside-and-within-a-buffer

sum(public.to.remove.1) # expect to lose 36 rows

arbres.publics.filtered.3 <- arbres.publics.filtered.2[!public.to.remove.1,]
nrow(arbres.publics.filtered.3) # expected number of rows

mapview(arbres.publics.filtered.3)





