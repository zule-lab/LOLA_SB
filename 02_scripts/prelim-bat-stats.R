# Date Created: 2026-11-09
# Date last modified: 2026-11-09
# Script by: Siena Blier

###############################################################################
## Number of bat calls
###############################################################################


# 0. Load libraries -------------------------------------------------------


library(tidyverse)
library(dplyr)
library(ggplot2)

# Temporarily change working directory to hard drive where acoustic data is kept
setwd("H:\\acoustic-data\\2026\\MTL")

# List of bat ARUs
all.arus <- list.files()
is.bat.aru <- str_ends(all.arus,"1")
bat.arus <- all.arus[is.bat.aru]

# Make list of all file names (of bat detections) in bat ARUs
all_bat_aru_files <- vector("list", 18)

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
  list.of.ultrasonic.wav.files <- list.of.wav.files[!is.acoustic]
  
  
  # Turn this list of audio files into dataframe we can work with
  files <- list.of.ultrasonic.wav.files %>% as.data.frame()
  
  # Add appropriate column title
  names(files) <- "file_name"
  
  # Make list of files to extract
  list.of.files <- files %>% 
    select("file_name") %>% as.list()
  
  # Store that list
  all_bat_aru_files[[i]] <- list.of.files
  
  
}

# Make a data frame out of all file names

all_bat_aru_files_unlisted <- all_bat_aru_files %>% unlist() %>% as.data.frame()

names(all_bat_aru_files_unlisted) <- "filename"

# Add column with Site ID
all_bat_aru_files_unlisted$site.id <-  sub("/.*","",all_bat_aru_files_unlisted$filename)
  
# Add column with the date

all_bat_aru_files_unlisted$date <- all_bat_aru_files_unlisted$filename %>% 
  str_extract("2026.{4}") %>%   # https://www.statology.org/str_extract-in-r/
  as.Date("%Y%m%d")

# Add column for building density

all_bat_aru_files_unlisted$building.density <- all_bat_aru_files_unlisted$site.id %>% 
  str_extract("^(.{2})")

# Add column for hex id

all_bat_aru_files_unlisted$hex.id <- all_bat_aru_files_unlisted$site.id %>% 
  str_extract("^(.{4})")

# Calculate number of calls per date per site

summary_counts <- all_bat_aru_files_unlisted %>% # From Barbara's script
  group_by(site.id,date) %>% 
  summarise(n_recordings = n(), .groups = 'drop')


# Lil graph (from Ben's script) of # recordings per day per site

ggplot(data = summary_counts) + 
  geom_line(aes(x = date, y = n_recordings, color = site.id)) +
  geom_point(aes(x = date, y = n_recordings, color = site.id)) +
  labs(x = "Date", y = "Number of bat recordings") +
  ggtitle("Number of bat recordings per day by site") +
  theme_classic()
  
# Lil graph of # calls per day per building density

# Calculate number of recordings per date per building density

summary_counts_density <- all_bat_aru_files_unlisted %>% # From Barbara's script
  group_by(building.density,date) %>% 
  summarise(n_recordings = n(), .groups = 'drop')

ggplot(data = summary_counts_density) + 
  geom_line(aes(x = date, y = n_recordings, color = building.density)) +
  geom_point(aes(x = date, y = n_recordings, color = building.density)) +
  labs(x = "Date", y = "Number of bat recordings") +
  ggtitle("Number of bat recordings per day by density") +
  theme_classic()

# Lil graph of # recordings per day per hex

summary_counts_hex <- all_bat_aru_files_unlisted %>% # From Barbara's script
  group_by(hex.id,date) %>% 
  summarise(n_recordings = n(), .groups = 'drop')

ggplot(data = summary_counts_hex) + 
  geom_line(aes(x = date, y = n_recordings, color = hex.id)) +
  geom_point(aes(x = date, y = n_recordings, color = hex.id)) +
  labs(x = "Date", y = "Number of bat recordings") +
  ggtitle("Number of bat recordings per day by hex") +
  theme_classic()


