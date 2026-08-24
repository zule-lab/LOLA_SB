# Date created : 24th August 2026
# Date last modified: 24th August 2026
# Created by: Siena Blier
# Purpose: Extracting coverage dates from ARU-generated metadata files


# 0. Load libraries -------------------------------------------------------

library(tidyverse)


# 1. Load data ------------------------------------------------------------

# Temporarily change working directory to hard drive where acoustic data is kept
setwd("H:\\acoustic-data\\2026\\MTL")

# Read one ARU metadata file as an example 
exfile <- read.table("D1CCPL-1\\sd-1\\D1CCPL-1_Summary.txt", header=TRUE)

# Check out structure
str(exfile)
names(exfile)

# The file doesn't separate neatly into columns/rows 
# because of commas within the column names
# we just need dates, so we'll do this a ratchet way

# Make empty data frame
truncated.df <- matrix(ncol = 1, nrow = nrow(exfile)) %>% as.data.frame()

# Truncate each row to just include the date (11 characters long)
for(row in 1:nrow(exfile)){
  truncated.df[row,1] <- str_sub(exfile[row,1],1,11)
}

# Aesthetics
names(truncated.df) <- "Date"
str(truncated.df)

# We want to see all the days for which we have recordings
unique(truncated.df$Date)

# How many recordings per day
truncated.df %>% group_by(Date) %>% count()

