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

# Test out for one folder
recording_dates <- list.files(path = "D1CCPL-1\\sd-1\\Data2", full.names = TRUE) %>% 
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

for(i in 1:length(bird.arus)){
  
  
  print(bird.arus[i])
}




# CUTS

# Read one ARU metadata file as an example 
exfile <- read.table("D1CCPL-1\\sd-1\\D1CCPL-1_Summary.txt", header=TRUE)

# Check out structure
str(exfile)
names(exfile)


# The file doesn't separate neatly into columns/rows 
# because of commas within the column names
# we just need dates and times, so we'll do this a ratchet way

# Make empty data frame
truncated.df <- matrix(ncol = 1, nrow = nrow(exfile)) %>% as.data.frame()

# Truncate each row to just include the dates and times (20 characters long)
for(row in 1:nrow(exfile)){
  truncated.df[row,1] <- str_sub(exfile[row,1],1,20)
}

# When the ARU dies and recording stops, a new header row appears. 
# Keep only non-header rows (ie rows that start with a date)

# Test on one row
str_starts(truncated.df[1,],"2026")

# Apply to all the rows

# Create a function
is.not.header = function(thing2check){
  str_starts(thing2check, "2026")
}

# Test
is.not.header(truncated.df[1,])

# Apply to all rows
is.not.header.matrix <- apply(truncated.df,1,is.not.header)

# Only keep rows that aren't headers
truncated.df.1 <- truncated.df[is.not.header.matrix,1] %>% as.data.frame()

# Aesthetics
names(truncated.df.1) <- "DateTime"


# Now we can separate into a date and time column
truncated.df.2 <- separate(truncated.df.1, DateTime, into=c("Date", "Time"), sep=",")


# We want to see all the days for which we have recordings
unique(truncated.df.2$Date)



# We can see that this metadata sheet includes the number of bird AND bat recordings

# Let's separate those out

bird.recordings <- filter(truncated.df.2, Time)

#as.Date()


# Format as date/time

as.POSIXct(truncated.df.1[1,1],format="%Y-%m-%d %H:%M:%S")

# Let's look at only bird recordings for now

# Filter by hour of recording
truncated.df.3$Hour <- as.numeric(truncated.df.3$Hour)
str(truncated.df.3)
truncated.df.bird <- truncated.df.3 %>% filter(Hour >= 4 & Hour <= 11)

# How many recordings per day
truncated.df.bird %>% group_by(Date) %>% count()

# Separate the times into hours/minutes/seconds (this will be useful later)
truncated.df.3 <- separate(truncated.df.2, Time,into=c("Hour","Min","Sec"),sep=":")
str(truncated.df.3)
