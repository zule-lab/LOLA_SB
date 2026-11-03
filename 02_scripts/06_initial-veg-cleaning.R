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
#library(renv)

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

# Read data

mytrees <- st_read("03_outdata/shapefiles/new.in.shp")
obstrees <- st_read("03_outdata/shapefiles/ObsUrb.in.shp")
publictrees <- st_read("03_outdata/shapefiles/public.in.shp")
shrubs <- st_read("03_outdata/shapefiles/shrubs.in.shp")


# Check for null entries (ie, points I put down accidentally, with no data) in
# my tree layer

mytrees[rowSums(is.na(mytrees)) == ncol(mytrees), ] # There are none!

# Repeat for shrub layer
shrubs[rowSums(is.na(shrubs)) == ncol(shrubs), ] # There are none!


# 4. Make sure columns are correct class ------------------------------------

# Start with my trees

str(mytrees)

# Remove "id", "ARU_ID" and "intrsection" columns
mytrees.1 <- mytrees %>% select(-c("id", "ARU_ID", "intrsct"))

# M_E should be a factor
unique(mytrees.1$M_E)
mytrees.1$M_E <- as.factor(mytrees.1$M_E)

# Hedge should be a factor
unique(mytrees.1$Hedge)

# Check out missing values
mytrees.1[is.na(mytrees.1$Hedge),] %>% View() # These should all be "No" (except for one)

# Correct NA values
# From: https://www.geeksforgeeks.org/r-language/replace-values-based-on-condition-in-r/
mytrees.1$Hedge[is.na(mytrees.1$Hedge)] <- "No"

unique(mytrees.1$Hedge) # Check

# Convert to factor
mytrees.1$Hedge <- as.factor(mytrees.1$Hedge)

# Site_ID should be a factor
unique(mytrees.1$Site_ID)

# Remove whitespace for "D3SHPL-2\n" sites
mytrees.1$Site_ID[(mytrees.1$Site_ID == "D3SHPL-2\n")] <- "D3SHPL-2"
unique(mytrees.1$Site_ID) # check

# Convert to factor
mytrees.1$Site_ID <- as.factor(mytrees.1$Site_ID)

str(mytrees.1) # Looks good!

# Emma's trees

str(obstrees)

# Remove columns I don't need
obstrees.1 <- obstrees %>% select(-c("Date", "Plot", "ObjctID", "rowid", 
                                     "cultivr", "landuse", "privpub", 
                                     "intrsct"))

# M_E should be a factor
unique(obstrees.1$M_E)
obstrees.1[is.na(obstrees.1$M_E),] %>% View() # These should all be "No" (except for one)
obstrees.1$M_E <- as.factor(obstrees.1$M_E)

# Hedge should be a factor
unique(obstrees.1$HedgY_N)
obstrees.1 <- rename(obstrees.1, Hedge = HedgY_N) # rename to match my column name
obstrees.1$Hedge <- as.factor(obstrees.1$Hedge)

# Hybrid should be a factor
obstrees.1$hybrid <- as.factor(obstrees.1$hybrid)

# Site_ID should be a factor
unique(obstrees.1$Site_ID)
obstrees.1$Site_ID <- as.factor(obstrees.1$Site_ID)

str(obstrees.1) # Looks good!

# Public trees

str(publictrees)

# Remove columns I don't need
publictrees.1 <- publictrees %>% select(-c("Essnc_n", "DHP", "Dat_Rlv", "Longitd",
                                           "Latitud", "intrsct"))

# Need to add M_E column
publictrees.1$M_E <- "Measured"
publictrees.1$M_E[(publictrees.1$cm_2026 == "Estimated")] <- "Estimated" # correct the one estimated tree
unique(publictrees.1$M_E)
publictrees.1$M_E <- as.factor(publictrees.1$M_E)

# Hybrid should be a factor
obstrees.1$hybrid <- as.factor(obstrees.1$hybrid)

# Add hedge column
publictrees.1$Hedge <- "No"
publictrees.1$Hedge <- as.factor(publictrees.1$Hedge)

# Change some column names
publictrees.1 <- rename(publictrees.1, Species = Essnc_l ) # rename to match my column name
publictrees.1 <- rename(publictrees.1, DBH_cm = X2026_D )
publictrees.1 <- rename(publictrees.1, Commnts = cm_2026 )

# Site_ID should be a factor
unique(publictrees.1$Site_ID)
# Remove whitespace for "D3SHPL-2\n" sites
publictrees.1$Site_ID[(publictrees.1$Site_ID == "D3SHPL-2\n")] <- "D3SHPL-2"
unique(publictrees.1$Site_ID) # check
publictrees.1$Site_ID <- as.factor(publictrees.1$Site_ID)

str(publictrees.1) # Looks good!

# Shrubs

str(shrubs)

# remove columns I don't need
shrubs.1 <- shrubs %>% select(-c("id", "ARU_ID", "intrsct"))

# Make Site_ID a factor
unique(shrubs.1$Site_ID)
# Remove whitespace for "D3SHPL-2\n" sites
shrubs.1$Site_ID[(shrubs.1$Site_ID == "D3SHPL-2\n")] <- "D3SHPL-2"
unique(shrubs.1$Site_ID) # check
shrubs.1$Site_ID <- as.factor(shrubs.1$Site_ID)

str(shrubs.1) # Looks good!

# Clean environment
rm(mytrees, obstrees, publictrees, shrubs)

# 5. Remove trees with missing DBH shrubs.in# 5. Remove trees with missing DBH ---------------------------------------

# Public trees

publictrees.1 %>% filter(DBH_cm == 0) # 8 trees with zero DBH
publictrees.2 <- publictrees.1 %>% filter(!DBH_cm == 0) # We lost 44 trees

# Check for NA's
sum(is.na(publictrees.1$DBH_cm))

# So the filtering function is also filtering out trees that have NA in DBH_cm column
# That's fine, because those are public trees that we did not see while surveying 
# (ie taken down since tree inventory) - thanks Nicole for helping with this!

# Emma's trees
sum(is.na(obstrees.1$DBH_fnl)) # ask Emma!!
obstrees.1[is.na(obstrees.1$DBH_fnl),]

# My trees
sum(is.na(mytrees.1$DBH_cm)) 

# Check it out
mytrees.1[is.na(mytrees.1$DBH_cm),] %>% View() # Most of these are multistem or
  # hedge trees, with DBH info in the comments

mytrees.1[(is.na(mytrees.1$DBH_cm)& is.na(mytrees.1$Commnts)),] %>% View() 
  # I have 5 with missing DBH and no info in the comments - could take average
  # like Emma did - ask Carly

# Save what we've done so far

# Save these files
st_write(obstrees.1,"03_outdata/shapefiles/Obsurb_classy.shp", append = FALSE, delete_layer = TRUE)
st_write(publictrees.2,"03_outdata/shapefiles/public_classy.shp", append = FALSE, delete_layer = TRUE)
st_write(mytrees.1,"03_outdata/shapefiles/new_classy.shp", append = FALSE, delete_layer = TRUE)
st_write(shrubs.1,"03_outdata/shapefiles/shrubs_classy.shp", append = FALSE, delete_layer = TRUE)

# Clean up environment
rm(mytrees.1, obstrees.1,publictrees.1,publictrees.2,shrubs.1)


# 6. Typooooooooos --------------------------------------------------------

# Assuming no typos in public trees and Emma's trees

# Read data

mytrees <- st_read("03_outdata/shapefiles/new_classy.shp")

unique(mytrees$Species) # Wowzah! 259 species

# Make a copy of dataframe to play with
mytrees.1 <- mytrees

# Start correcting species names :)
# Use "spp." in the case of genus-level identification

mytrees.1$Species[(mytrees.1$Species == "Tilia sp.")] <- "Tilia spp."
mytrees.1$Species[(mytrees.1$Species == "Malus")] <- "Malus spp."
mytrees.1$Species[(mytrees.1$Species == "Pyrus sp")] <- "Pyrus spp."
mytrees.1$Species[(mytrees.1$Species == "Thuya occidentalis")] <- "Thuja occidentalis"
mytrees.1$Species[(mytrees.1$Species == "Pyrus sp")] <- "Pyrus spp."
mytrees.1$Species[(mytrees.1$Species == "Forsythia sp.")] <- "Forsythia spp."
mytrees.1$Species[(mytrees.1$Species == "Acer negunfo")] <- "Acer negundo"
mytrees.1$Species[(mytrees.1$Species == "Salix sp.")] <- "Salix spp."
mytrees.1$Species[(mytrees.1$Species == "Faris grandifolia")] <- "Fagus grandifolia"
mytrees.1$Species[(mytrees.1$Species == "Tamarix sp.")] <- "Tamarix spp."
mytrees.1$Species[(mytrees.1$Species == "Honeysuckle sp.")] <- "Lonicera spp."
mytrees.1$Species[(mytrees.1$Species == "Ramnus cathartica")] <- "Rhamnus cathartica"
mytrees.1$Species[(mytrees.1$Species == "Juniperus sp.")] <- "Juniperus spp."
mytrees.1$Species[(mytrees.1$Species == "Acer Negundo")] <- "Acer negundo"
mytrees.1$Species[(mytrees.1$Species == "Acer platinoides")] <- "Acer platanoides"
mytrees.1$Species[(mytrees.1$Species == "Quercus spp")] <- "Quercus spp."
mytrees.1$Species[(mytrees.1$Species == "Rosalis")] <- "Rosalis spp."
mytrees.1$Species[(mytrees.1$Species == "Betula papyrivera")] <- "Betula papyrifera"
mytrees.1$Species[(mytrees.1$Species == "Glasitsia treacampos")] <- "Gleditsia triacanthos"
mytrees.1$Species[(mytrees.1$Species == "Franguls alnus")] <- "Frangula alnus"
mytrees.1$Species[(mytrees.1$Species == "Rosa rugusa")] <- "Rosa rugosa"
mytrees.1$Species[(mytrees.1$Species == "Taxes spp.")] <- "Taxus spp."
mytrees.1$Species[(mytrees.1$Species == "Betula papryifera")] <- "Betula papyrifera"
mytrees.1$Species[(mytrees.1$Species == "Gladitsia treacampus")] <- "Gleditsia triacanthos"



mytrees.1$Species[(mytrees.1$Species == "Gladitsia triacanthios")] <- "Gleditsia triacanthos"
mytrees.1$Species[(mytrees.1$Species == "Ulmus sp.")] <- "Ulmus spp."
mytrees.1$Species[(mytrees.1$Species == "Thuja occidentalis are")] <- "Thuja occidentalis"
mytrees.1$Species[(mytrees.1$Species == "Acer nagundo")] <- "Acer negundo"
mytrees.1$Species[(mytrees.1$Species == "Syringica reticulata")] <- "Syringa reticulata"
mytrees.1$Species[(mytrees.1$Species == "Philadelphia pubescens")] <- "Philadelphus pubescens"
mytrees.1$Species[(mytrees.1$Species == "Thuya")] <- "Thuja spp."
mytrees.1$Species[(mytrees.1$Species == "Amalanchier canadensis")] <- "Amelanchier canadensis"
mytrees.1$Species[(mytrees.1$Species == "Abies")] <- "Abies spp."
mytrees.1$Species[(mytrees.1$Species == "Acer negunda")] <- "Acer negundo"
mytrees.1$Species[(mytrees.1$Species == "Hydrangea sp.")] <- "Hydrangea spp."
mytrees.1$Species[(mytrees.1$Species == "Amalanchier spp.")] <- "Amelanchier spp."
mytrees.1$Species[(mytrees.1$Species == "Honeysuckle sp")] <- "Lonicera spp."
mytrees.1$Species[(mytrees.1$Species == "euonymus alatus")] <- "Euonymus alatus"
mytrees.1$Species[(mytrees.1$Species == "Syringica sp.")] <- "Syringa spp."
mytrees.1$Species[(mytrees.1$Species == "Rhus typina")] <- "Rhus typhina"
mytrees.1$Species[(mytrees.1$Species == "Morus spp")] <- "Morus spp."
mytrees.1$Species[(mytrees.1$Species == "Ulmus spp")] <- "Ulmus spp."
mytrees.1$Species[(mytrees.1$Species == "Gymnocladus dioecious")] <- "Gymnocladus dioicus"
mytrees.1$Species[(mytrees.1$Species == "Rose if Sharon")] <- "Hibiscus syriacus"
mytrees.1$Species[(mytrees.1$Species == "Thuja occisentalis")] <- "Thuja occidentalis"
mytrees.1$Species[(mytrees.1$Species == "Balsamea sp.")] <- "Balsamea spp."
mytrees.1$Species[(mytrees.1$Species == "Acer negunso")] <- "Acer negundo"
mytrees.1$Species[(mytrees.1$Species == "Parus spp.")] <- "Pyrus spp."
mytrees.1$Species[(mytrees.1$Species == "Lancera tatarica")] <- "Lonicera tatarica"
mytrees.1$Species[(mytrees.1$Species == "Fraxinux Pennsylvanica")] <- "Fraxinus pennsylvanica"
mytrees.1$Species[(mytrees.1$Species == "Gladitsia treeacampa")] <- "Gleditsia triacanthos"
mytrees.1$Species[(mytrees.1$Species == "Gladitsia treacanthos")] <- "Gleditsia triacanthos"
mytrees.1$Species[(mytrees.1$Species == "Thuya oxccidentalis")] <- "Thuja occidentalis"
mytrees.1$Species[(mytrees.1$Species == "Moris alba")] <- "Morus alba"
mytrees.1$Species[(mytrees.1$Species == "Gingko baloba")] <- "Ginkgo biloba"
mytrees.1$Species[(mytrees.1$Species == "Lonicera")] <- "Lonicera spp."
mytrees.1$Species[(mytrees.1$Species == "Ulmus Americana")] <- "Ulmus americana"
mytrees.1$Species[(mytrees.1$Species == "Stringa reticulata")] <- "Syringa reticulata"
mytrees.1$Species[(mytrees.1$Species == "Buckeye ?")] <- "Aesculus glabra"


mytrees.1$Species[(mytrees.1$Species == "Fraxinus sp.")] <- "Fraxinus spp."
mytrees.1$Species[(mytrees.1$Species == "Celtics occidentalis")] <- "Celtis occidentalis"
mytrees.1$Species[(mytrees.1$Species == "Prunus")] <- "Prunus spp."
mytrees.1$Species[(mytrees.1$Species == "Tilia chordata")] <- "Tilia cordata"
mytrees.1$Species[(mytrees.1$Species == "Rhamus cathartica")] <- "Rhamnus cathartica"
mytrees.1$Species[(mytrees.1$Species == "Ulmus lavis")] <- "Ulmus laevis"
mytrees.1$Species[(mytrees.1$Species == "Picea sp.")] <- "Picea spp."
mytrees.1$Species[(mytrees.1$Species == "Quercus sp.")] <- "Quercus spp."
mytrees.1$Species[(mytrees.1$Species == "Thuja")] <- "Thuja spp."
mytrees.1$Species[(mytrees.1$Species == "Syringa spp")] <- "Syringa spp."
mytrees.1$Species[(mytrees.1$Species == "Picea spp")] <- "Picea spp."
mytrees.1$Species[(mytrees.1$Species == "Dead")] <- "Dead standing"
mytrees.1$Species[(mytrees.1$Species == "Prunus sp")] <- "Prunus spp."
mytrees.1$Species[(mytrees.1$Species == "Rhus typhona")] <- "Rhus typhina"
mytrees.1$Species[(mytrees.1$Species == "Malus spp")] <- "Malus spp."
mytrees.1$Species[(mytrees.1$Species == "Sambucus")] <- "Sambucus spp."
mytrees.1$Species[(mytrees.1$Species == "Rose of Sharon")] <- "Hibiscus syriacus"
mytrees.1$Species[(mytrees.1$Species == "Fraxinus Pennsylvanica")] <- "Fraxinus pennsylvanica"
mytrees.1$Species[(mytrees.1$Species == "Tuja occidentalis")] <- "Thuja occidentalis"
mytrees.1$Species[(mytrees.1$Species == "Hydrangea Paniculata")] <- "Hydrangea paniculata"
mytrees.1$Species[(mytrees.1$Species == "Prunus super.")] <- "Prunus spp."
mytrees.1$Species[(mytrees.1$Species == "Ttsuga canadensis")] <- "Tsuga canadensis."
mytrees.1$Species[(mytrees.1$Species == "Hydrangea spp")] <- "Hydrangea spp."
mytrees.1$Species[(mytrees.1$Species == "Hyndrangea peniculata")] <- "Hydrangea paniculata"
mytrees.1$Species[(mytrees.1$Species == "Prunus Virginiana")] <- "Prunus virginiana"
mytrees.1$Species[(mytrees.1$Species == "Amalanchier grandiflora")] <- "Amelanchier grandiflora"


mytrees.1$Species[(mytrees.1$Species == "Malus sp.")] <- "Malus spp."
mytrees.1$Species[(mytrees.1$Species == "Populous deltoides")] <- "Populus deltoides"
mytrees.1$Species[(mytrees.1$Species == "Prunus sp.")] <- "Prunus spp."
mytrees.1$Species[(mytrees.1$Species == "Syringica vulgaris")] <- "Syringa vulgaris"
mytrees.1$Species[(mytrees.1$Species == "Fraxinus pennsylvannica")] <- "Fraxinus pennsylvanica"
mytrees.1$Species[(mytrees.1$Species == "Populous")] <- "Populus spp."
mytrees.1$Species[(mytrees.1$Species == "Ulmus Lavis")] <- "Ulmus laevis"
mytrees.1$Species[(mytrees.1$Species == "Acer freemanii")] <- "Acer x freemanii"
mytrees.1$Species[(mytrees.1$Species == "Picea Rubens")] <- "Picea rubens"
mytrees.1$Species[(mytrees.1$Species == "Syringa sp.")] <- "Syringa spp."
mytrees.1$Species[(mytrees.1$Species == "Magnolia sp.")] <- "Magnolia spp."
mytrees.1$Species[(mytrees.1$Species == "Lonicera spp")] <- "Lonicera spp."
mytrees.1$Species[(mytrees.1$Species == "Prunus spp")] <- "Prunus spp."
mytrees.1$Species[(mytrees.1$Species == "Rose of sharon")] <- "Hibiscus syriacus"
mytrees.1$Species[(mytrees.1$Species == "Gladitsia triacanthos")] <- "Gleditsia triacanthos"
mytrees.1$Species[(mytrees.1$Species == "Acer palmata")] <- "Acer palmatum"
mytrees.1$Species[(mytrees.1$Species == "Metasequioa glyptostroboides")] <- "Metasequoia glyptostroboides"
mytrees.1$Species[(mytrees.1$Species == "Gingko biloba")] <- "Ginkgo biloba"
mytrees.1$Species[(mytrees.1$Species == "Quercus rhubra")] <- "Quercus rubra"
mytrees.1$Species[(mytrees.1$Species == "Pinus strobes")] <- "Pinus strobus"
mytrees.1$Species[(mytrees.1$Species == "Acer gennala")] <- "Acer tataricum subsp. ginnala"
mytrees.1$Species[(mytrees.1$Species == "Acer genala")] <- "Acer tataricum subsp. ginnala"
mytrees.1$Species[(mytrees.1$Species == "Euonymus aladus")] <- "Euonymus alatus"

# Check

unique(mytrees.1$Species)
species <- as.data.frame(sort(unique(mytrees.1$Species))) # 169 spp - makes more sense!

# Fix typos

mytrees.1$Species[(mytrees.1$Species == "Gladitia triacanthos")] <- "Gleditsia triacanthos"
mytrees.1$Species[(mytrees.1$Species == "Hydrangea peniculata")] <- "Hydrangea paniculata"
mytrees.1$Species[(mytrees.1$Species == "Hydrangea penniculata")] <- "Hydrangea paniculata"
mytrees.1$Species[(mytrees.1$Species == "Lonicera tartarica")] <- "Lonicera tatarica"
mytrees.1$Species[(mytrees.1$Species == "Pinus sp.")] <- "Pinus spp."
mytrees.1$Species[(mytrees.1$Species == "Pyrus sp.")] <- "Pyrus spp."
mytrees.1$Species[(mytrees.1$Species == "Sambuca canadensis")] <- "Sambucus canadensis"
mytrees.1$Species[(mytrees.1$Species == "Spirea spp.")] <- "Spiraea spp."
mytrees.1$Species[(mytrees.1$Species == "Vibirnum opulus")] <- "Viburnum opulus"

# Check

# Code from: https://www.statology.org/sort-alphabetically-in-r/
species1 <- as.data.frame(sort(unique(mytrees.1$Species))) # 169 spp - makes more sense!

# More fixes
mytrees.1$Species[(mytrees.1$Species == "Juniper spp.")] <- "Juniperus spp."
mytrees.1$Species[(mytrees.1$Species == "Rhamnus catharica")] <- "Rhamnus cathartica"
mytrees.1$Species[(mytrees.1$Species == "Tsuga canadensis.")] <- "Tsuga canadensis"
mytrees.1$Species[(mytrees.1$Species == "Vibernum opulus")] <- "Viburnum opulus"

# Save

st_write(mytrees.1,"03_outdata/shapefiles/new_notypos.shp", append = FALSE, delete_layer = TRUE)

rm(mytrees, mytrees.1,species,species1)

# 7. Add columns for OpenRefine -------------------------------------------

# Read data

mytrees <- st_read("03_outdata/shapefiles/new_notypos.shp")

# Make a copy to play with
mytrees.1 <- mytrees

# Add a column for multistem DBH values


mytrees.1$DBH_multistem <- mytrees$Commnts

# Add columns for hedge math
mytrees.1$hedge_length_m <- NA
mytrees.1$hedge_stems_per_m <- NA
mytrees.1$hedge_avg_DBH_cm <- NA

# Add column to replicate tree
mytrees.1$replicate_n <- NA

# Save this new dataframe to work with in OpenRefine
# Code from: https://stackoverflow.com/questions/67203648/how-to-hide-remove-geometry-column
mytrees.2 <- sf::st_drop_geometry(mytrees.1)
mytrees.2 %>% str()

write.csv(mytrees.2,"03_outdata/new_OR_input.csv")

# Check that it worked
check <- as.data.frame(read.csv("03_outdata/new_OR_input.csv"))







# Missing species
mytrees[is.na(mytrees$Species),]  # There are none!

# Why are there some trees without a M_E value?
mytrees.1[(is.na(mytrees.1$M_E)),] %>% View()

# Missing M_E value...? Some of these are hedges, some seem to have been measured
# others idk... deal with this later

# Why missing Hedge column?

# Ask Emma missing M_E column values?

# To deal with later: There are no public tree hedges...

