# Date created : 24th August 2026
# Date last modified: 24th August 2026
# Created by: Siena Blier
# Purpose: Extracting coverage dates from ARU-generated metadata files


# 0. Load libraries -------------------------------------------------------

library(tidyverse)
library(lubridate)

# 1. Load data ------------------------------------------------------------

# Temporarily change working directory to hard drive where acoustic data is kept
setwd("H:\\acoustic-data\\2026\\MTL")


# 2. Check coverage of bird-only ARUs -------------------------------------


# Test out for one folder
recording_dates <- list.files(path = "D2SHPL-2\\sd-2\\Data", full.names = TRUE) %>% 
  sub(".*?_","", .) %>% sub("_.*","", .) 
recording_dates
n_recs_per_day <- recording_dates %>% table() %>% as.data.frame()
names(n_recs_per_day) <- c("date","n_recs")
n_recs_per_day

# Make a list of the bird ARU IDs
all.arus <- list.files()
is.bird.aru <- str_ends(all.arus,"2")
bird.arus <- all.arus[is.bird.aru]
bird.arus

#Would need to add code for D2SHPL-2 => check second sd card
for(i in 1:length(bird.arus)){
  
  file.path <- paste0(bird.arus[i],"\\sd-1\\Data")

  recording_dates <- list.files(path = file.path, full.names = TRUE) %>% 
    sub(".*?_","", .) %>% sub("_.*","", .) 
  n_recs_per_day <- recording_dates %>% table() %>% as.data.frame()
  names(n_recs_per_day) <- c("date","n_recs")
  print(bird.arus[i])
  print(n_recs_per_day)

}


# 3. Check coverage of bat ARUs -------------------------------------------

# Make a list of the bat ARU IDs
all.arus <- list.files()
is.bat.aru <- str_ends(all.arus,"1")
bat.arus <- all.arus[is.bat.aru]


# Iterate through folders
for(i in 1:length(bat.arus)){
  
  file.path <- paste0(bat.arus[i],"\\sd-1\\Data2")
  
  recording_dates <- list.files(path = file.path, full.names = TRUE) %>% 
    sub(".*?_","", .) %>% sub("_.*","", .) 
  n_recs_per_day <- recording_dates %>% table() %>% as.data.frame()
  names(n_recs_per_day) <- c("date","n_recs")
  print(bat.arus[i])
  print(n_recs_per_day)
  
}
