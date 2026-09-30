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

# Set start date of pre-breeding migration period
# May 16th to June 5th inclusive
start.spring <- as.Date("2026-05-16")
end.spring <- as.Date("2026-06-05")


# Start with bird ARUs

# Make a list of the bird-only ARU IDs

# List of folders for all MTL sites
all.arus <- list.files()
# Filter to keep bird-only ARUs
is.bird.aru <- str_ends(all.arus,"2")
bird.arus <- all.arus[is.bird.aru]

# Make list to keep all bird ARU file names
masterlist.spring.bird <- vector("list", 18)

# Get list of files to process
for(i in 1:length(bird.arus)){
  
  # Store ARU ID
  aru.id <- bird.arus[i]
  
  # Get list of all files in that ARU's folder
  # Search recursively (there are subfolders for each sd card)
  list.of.files <- list.files(aru.id, recursive=TRUE)
  # We only care about audio (.wav) files
  list.of.wav.files <- list.of.files[str_ends(list.of.files,"wav")]
  
  # Turn this list of audio files into dataframe we can work with
  files <- list.of.wav.files %>% as.data.frame()
  
  # Add appropriate column title
  names(files) <- "file_name"

  # Extract date from file names, store in new column
  
  files$raw.date <- files$file_name  %>% 
    # Remove before the first "_"
    sub(".*?_","", .) %>%
    # Remove before second "_"
    sub(".*?_", "", .) %>% 
    # Remove after remaining "_"
    sub("_.*","", .)
  
  # Format date
  files$clean.date <- files$raw.date %>% as.Date("%Y%m%d")
  
  # Check if the date is in the migration period (ie, to
  # be included in the working folder)
  files$to_extract <- ifelse(files$clean.date >= start.spring & files$clean.date <= 
                         end.spring,1,0)
  
  # Make list of files to extract
  list.of.files.to.extract <- filter(files, to_extract==1) %>% 
    select("file_name") %>% as.list()
  
  # Store that list
  masterlist.spring.bird[[i]] <- list.of.files.to.extract
  
  
}


# Create working folder

file.copy()
%in%

# 3. Summer working folder ------------------------------------------------


