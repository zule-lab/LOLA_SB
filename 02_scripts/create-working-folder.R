# Date created : 30th September 2026
# Date last modified: 2nd October 2026
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
  list.of.files <- list.files(aru.id, recursive=TRUE,full.names=TRUE)
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

# Success! Now do this for the bird/bat ARUs

# Filter to keep bird/bat ARUs
is.bat.aru <- str_ends(all.arus,"1")
bat.arus <- all.arus[is.bat.aru]

# Make list to keep all bird/bat ARU file names
masterlist.spring.bat <- vector("list", 18)

# Get list of files to process
for(i in 1:length(bat.arus)){
  
  # Store ARU ID
  aru.id <- bat.arus[i]
  
  # Get list of all files in that ARU's folder
  # Search recursively (there are subfolders for each sd card)
  list.of.files <- list.files(aru.id, recursive=TRUE,full.names=TRUE)
  # We only care about audio (.wav) files
  list.of.wav.files <- list.of.files[str_ends(list.of.files,"wav")]
  # We only want bird (acoustic) recordings, which are kept in "Data2" sufolder
  is.acoustic <- grepl("Data2", list.of.wav.files, fixed = TRUE)
  list.of.acoustic.wav.files <- list.of.wav.files[is.acoustic]
  

  # Turn this list of audio files into dataframe we can work with
  files <- list.of.acoustic.wav.files %>% as.data.frame()
  
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
  masterlist.spring.bat[[i]] <- list.of.files.to.extract
  
  
}

# Copy files from bird ARUs to spring working folder

# Iterating through all bird-only ARUs
for(i in 1:length(masterlist.spring.bird)){
  
  # Get the list of files we want to copy to working folder
  files.to.copy <- masterlist.spring.bird[[i]] %>% unlist()
  
  for(file in 1:length(files.to.copy)){
    file.copy(files.to.copy[[file]],"working-folder-spring")
  }
}

# Let's check how many files we should actually have

# Counter
spring.bird.files <- 0

for(i in 1:length(masterlist.spring.bird)){
  
  # Get the list of files we want to copy to working folder
  files.to.copy <- masterlist.spring.bird[[i]] %>% unlist()
  
  # Get the number of files to copy for that ARU
  spring.bird.files <- spring.bird.files + length(files.to.copy)
}
# Check output
spring.bird.files


# Copy files from bird/bat ARUs to spring working folder
for(i in 1:length(masterlist.spring.bat)){
  
  # Get the list of files we want to copy to working folder
  files.to.copy <- masterlist.spring.bat[[i]] %>% unlist()
  
  for(file in 1:length(files.to.copy)){
    file.copy(files.to.copy[[file]],"working-folder-spring")
  }
}

# Let's check how many files we should actually have

# Counter
spring.bat.files <- 0

for(i in 1:length(masterlist.spring.bat)){
  
  # Get the list of files we want to copy to working folder
  files.to.copy <- masterlist.spring.bat[[i]] %>% unlist()
  
  # Get the number of files to copy for that ARU
  spring.bat.files <- spring.bat.files + length(files.to.copy)
}
# Check output
spring.bat.files

total.spring.files <- spring.bat.files + spring.bird.files
total.spring.files # Matches! Phew!

# 3. Summer working folder ------------------------------------------------

# Set start date of breeding period (ie summer)
# June 6th to July 6th inclusive
start.summer <- as.Date("2026-06-06")
end.summer <- as.Date("2026-07-06")


# Start with bird-only ARUs


# Make list to keep all bird ARU file names
masterlist.summer.bird <- vector("list", 18)

# Get list of files to process
for(i in 1:length(bird.arus)){
  
  # Store ARU ID
  aru.id <- bird.arus[i]
  
  # Get list of all files in that ARU's folder
  # Search recursively (there are subfolders for each sd card)
  list.of.files <- list.files(aru.id, recursive=TRUE,full.names=TRUE)
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
  files$to_extract <- ifelse(files$clean.date >= start.summer & files$clean.date <= 
                               end.summer,1,0)
  
  # Make list of files to extract
  list.of.files.to.extract <- filter(files, to_extract==1) %>% 
    select("file_name") %>% as.list()
  
  # Store that list
  masterlist.summer.bird[[i]] <- list.of.files.to.extract
  
  
}

# Success! Now do this for the bird/bat ARUs

# Make list to keep all bird/bat ARU file names
masterlist.summer.bat <- vector("list", 18)

# Get list of files to process
for(i in 1:length(bat.arus)){
  
  # Store ARU ID
  aru.id <- bat.arus[i]
  
  # Get list of all files in that ARU's folder
  # Search recursively (there are subfolders for each sd card)
  list.of.files <- list.files(aru.id, recursive=TRUE,full.names=TRUE)
  # We only care about audio (.wav) files
  list.of.wav.files <- list.of.files[str_ends(list.of.files,"wav")]
  # We only want bird (acoustic) recordings, which are kept in "Data2" sufolder
  is.acoustic <- grepl("Data2", list.of.wav.files, fixed = TRUE)
  list.of.acoustic.wav.files <- list.of.wav.files[is.acoustic]
  
  
  # Turn this list of audio files into dataframe we can work with
  files <- list.of.acoustic.wav.files %>% as.data.frame()
  
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
  files$to_extract <- ifelse(files$clean.date >= start.summer & files$clean.date <= 
                               end.summer,1,0)
  
  # Make list of files to extract
  list.of.files.to.extract <- filter(files, to_extract==1) %>% 
    select("file_name") %>% as.list()
  
  # Store that list
  masterlist.summer.bat[[i]] <- list.of.files.to.extract
  
  
}

# Copy files from bird ARUs to summer working folder

# Iterating through all bird-only ARUs
for(i in 1:length(masterlist.summer.bird)){
  
  # Get the list of files we want to copy to working folder
  files.to.copy <- masterlist.summer.bird[[i]] %>% unlist()
  
  for(file in 1:length(files.to.copy)){
    file.copy(files.to.copy[[file]],"working-folder-summer")
  }
}

# Let's check how many files we should actually have

# Counter
summer.bird.files <- 0

for(i in 1:length(masterlist.summer.bird)){
  
  # Get the list of files we want to copy to working folder
  files.to.copy <- masterlist.summer.bird[[i]] %>% unlist()
  
  # Get the number of files to copy for that ARU
  summer.bird.files <- summer.bird.files + length(files.to.copy)
}
# Check output
summer.bird.files


# Copy files from bird/bat ARUs to summer working folder
for(i in 1:length(masterlist.summer.bat)){
  
  # Get the list of files we want to copy to working folder
  files.to.copy <- masterlist.summer.bat[[i]] %>% unlist()
  
  for(file in 1:length(files.to.copy)){
    file.copy(files.to.copy[[file]],"working-folder-summer")
  }
}

# Let's check how many files we should actually have - now for bats
summer.bat.files <- 0

for(i in 1:length(masterlist.summer.bat)){
  
  # Get the list of files we want to copy to working folder
  files.to.copy <- masterlist.summer.bat[[i]] %>% unlist()
  
  # Get the number of files to copy for that ARU
  summer.bat.files <- summer.bat.files + length(files.to.copy)
}
# Check output
summer.bat.files

total.summer.files <- summer.bird.files + summer.bat.files
total.summer.files # It matches! Phew!
