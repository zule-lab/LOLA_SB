# Date created : 30th September 2026
# Date last modified: 30th September 2026
# Created by: Siena Blier
# Purpose: Create working folders with ARU recordings to be processed


# 0. Load libraries -------------------------------------------------------

library(tidyverse)
library(lubridate)


# 1. Load data ------------------------------------------------------------

# Temporarily change working directory to hard drive where acoustic data is kept
setwd("H:\\acoustic-data\\2026\\MTL")

# 2. Spring working folder ------------------------------------------------

# Start with bird ARUs

## Test out for one folder

### List out all the files in the folder
files <- list.files(path = "D2SHPL-2\\sd-2\\Data", full.names = TRUE) %>%
  # Turn this list into a data frame
  as.data.frame()

# Check out the data frame
files

# Add appropriate column title
names(files) <- "file_name"
str(files)

# Add a column that tells us whether or not to extract
files[,"to_extract"] <- 0

# Go through each row of the data frame
for(i in 1:nrow(file.names)){
  
  # Extract the date
  raw.date <- files[i,"file_name"]
  
  # Convert to date format
  clean.date <- raw.date %>% as.Date("%y%m%d")
  
  
  if.else()
  file.names[i,"to_extract"] <- 1
  
}
# Extract the date

# Convert to a date object

# Check if date is within a range of dates
sub(".*?_","", .) %>% sub("_.*","", .)



# For all bird ARUs

# Make a list of the bird ARU IDs
all.arus <- list.files()
is.bird.aru <- str_ends(all.arus,"2")
bird.arus <- all.arus[is.bird.aru]
bird.arus

# Make list of files to include in spring working folder
# Spring: May 16th to June 5th inclusive



# Go through each site

# Extract

# Is it within the range of dates we're interested in?

# Repeat with bat ARUs

# Dates

# Create working folder

file.copy()
%in%

# 3. Summer working folder ------------------------------------------------


