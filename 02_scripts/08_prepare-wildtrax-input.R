# Date Created: 2026-10-30
# Date last modified: 2026-10-30
# Original script from Ana Morales, modified by Siena Blier

###############################################################################
## Preparing tags to upload to WildTrax.
###############################################################################


# 0. Load libraries -------------------------------------------------------


library(tidyverse)
library(dplyr)


# 1. Summarize HawkEars results -------------------------------------------

# Get a list of all text files in the directory
file_list <- list.files(path = "03_outdata/HawkEars_output_spring_bats", pattern = "\\.txt$", full.names = TRUE)

# Define the column names you want to assign. Since hawkears txt files don't have column names
column_names <- c("Start (s)", "End (s)", "Common name")  # Adjust as per your file's structure


# make a list of species you want to extract data on. 

species_list_df <- read.csv("01_rawdata/acoustic/species_list.csv", sep = "_") # Check with Barbara
species_list <- c(species_list_df$Common.name)

#temporarily set working directory to the folder with all hawkears txt files
setwd("C:/Users/MoralesA/Documents/My Repositories/Mtl_ARU/HawkEars_raw")


# Read and combine all files into one dataframe, adding the filename as a column
# It will skip files that have no detections

all_HEdata <- lapply(file_list, function(file) {
  # Check if the file is empty
  if (file.info(file)$size == 0) {
    message("Skipping empty file: ", file)
    return(NULL)
  }
  # Read the current file, without headers, assuming columns are space/tab separated
  temp_data <- read.table(file, header = FALSE, sep = "\t", fill = TRUE, quote = "", stringsAsFactors = FALSE)
  
  # Assign the specified column names
  colnames(temp_data) <- column_names
  
  # Split the "Common name" column (3rd column) into two new columns based on the ";" separator
  # Create two new columns: "Common name part 1" and "Common name part 2"
  temp_data <- temp_data %>%
    tidyr::separate("Common name", into = c("Common name", "Confidence"), sep = ";", extra = "merge", fill = "right")
  
  # Add the filename as a new column
  temp_data$filename <- basename(file)  # Extract just the file name (without path)
  
  return(temp_data)
}) %>%
  bind_rows() %>%
  mutate(location=sub("_.*", "", filename)) %>% #this substracts only the portion of the filename that comes before the underscore
  mutate(datetime_software = sub("^[^_]*_", "", filename))  %>% #this substracts only the portion of the filename that comes after the underscore
  ##date formatting
  mutate(year=substr(datetime_software,1,4))%>% #this is based on the length of file names which is why I transformed first everything to standard 4 letter site names
  mutate(month=substr(datetime_software,5,6))%>%
  mutate(day=substr(datetime_software,7,8))%>%
  mutate(date=as.Date(paste0(year,"/",month,"/",day)))%>%
  mutate(rec_time=paste0(substr(datetime_software,10,11), ":", substr(datetime_software,12,13), ":", substr(datetime_software,14,15)))%>%
  mutate(recordingDate= paste0(date, " ", rec_time)) %>%
  dplyr::rename(startTime='Start (s)',end='End (s)',
                common='Common name',confidence=Confidence)  %>%
  filter(common%in%species_list)

# View the combined data with the filename column
head(all_HEdata)

#adding location codes (4 letter codes for each location, aka each individual ARU deployment) 
#May not need this if you already named your ARU locations with 4 letter codes when deploying the devices

#if not needed, just run this first line and skip the rest until the next #
all_HEdata$location_code <- all_HEdata$location


all_HEdata$location_code <- sub(pattern = "ANGELL", "ANWO", all_HEdata$location_code)
all_HEdata$location_code <- sub(pattern = "ANGRIGNON", "ANGR", all_HEdata$location_code)
all_HEdata$location_code <- sub(pattern = "ANJOU", "ANJO", all_HEdata$location_code)
all_HEdata$location_code <- sub(pattern = "FALAISE", "FALA", all_HEdata$location_code)
all_HEdata$location_code <- sub(pattern = "LIESSE", "BDLI-", all_HEdata$location_code)
all_HEdata$location_code <- sub(pattern = "MONTIGNY", "MONT", all_HEdata$location_code)
all_HEdata$location_code <- sub(pattern = "PRAIRIES", "PRAI", all_HEdata$location_code)
all_HEdata$location_code <- sub(pattern = "TECHNO", "TECH", all_HEdata$location_code)
all_HEdata$location_code <- sub(pattern = "THOMAS", "THOM", all_HEdata$location_code)
all_HEdata$location_code <- sub(pattern = "TIMBERLEA", "TIMB", all_HEdata$location_code)
all_HEdata$location_code <- sub(pattern = "TIOHTIAKE", "TIOH", all_HEdata$location_code)


#make sure it worked
head(all_HEdata)

#creating a column only with general site name (greenspace name)
#important for our analyses, but maybe not for yours

all_HEdata <- all_HEdata %>%
  mutate(site = substr(location_code, 1,4))


#go back to our working directory
setwd("C:/Users/MoralesA/Documents/My Repositories/Mtl_ARU")


#### Get data from each site per year ####
#TIOHTIAKE example

TIOH2024_HE <- all_HEdata %>%
  filter(site == "TIOH", year== 2024)%>%
  mutate(tagLength = end - startTime) %>%
  filter(date < "2024-05-30")

c
banding_codes <- read.csv("species_codes_WT.csv")

TIOH2024_HE <- TIOH2024_HE %>%
  left_join(banding_codes, by = "common")

#see if it worked and if all species were matched to a banding code. This can change with updates to species names (splits)
unique(TIOH2024_HE$species)

#for our analyses we will only use detections above 0.9 confidence
TIOH2024_HEsub <- subset(TIOH2024_HE, confidence >= 0.9)

#keep only the 3 highest confidence detections for each species per ARU location. but remove repeated detections of species within the same recording
TIOH2024TOP3_HE <- TIOH2024_HEsub %>% arrange(desc(confidence)) %>% distinct(filename, common, .keep_all = TRUE)%>%
  group_by(location_code, common) %>%
  slice(1:3)

#edit dataframe to try to match Wildtrax tag upload file

TIOH2024TOP3_HE$method = "1SPM"
TIOH2024TOP3_HE$taskLength = "599.85"
TIOH2024TOP3_HE$transcriber = "Not Assigned"
TIOH2024TOP3_HE$speciesIndividualNumber = "1"
TIOH2024TOP3_HE$vocalization = "Song"
TIOH2024TOP3_HE$abundance = ""
TIOH2024TOP3_HE$minFreq = "0"
TIOH2024TOP3_HE$maxFreq = "12000"
TIOH2024TOP3_HE$speciesIndividualComment =""
TIOH2024TOP3_HE$internal_tag_id = ""

TIOH2024_WT_upload <- TIOH2024TOP3_HE
TIOH2024_WT_upload$end <- NULL
TIOH2024_WT_upload$common <- NULL
TIOH2024_WT_upload$confidence <- NULL
TIOH2024_WT_upload$filename <- NULL
TIOH2024_WT_upload$datetime_software <- NULL
TIOH2024_WT_upload$year <- NULL
TIOH2024_WT_upload$month <- NULL
TIOH2024_WT_upload$day <- NULL
TIOH2024_WT_upload$date <- NULL
TIOH2024_WT_upload$rec_time <- NULL
TIOH2024_WT_upload$location_code <- NULL
TIOH2024_WT_upload$site <- NULL

setwd("C:/Users/MoralesA/Documents/My Repositories/Mtl_ARU/WT_uploads")

write.csv(TIOH2024_WT_upload, "TIOH2024_HE_tags.csv", row.names=FALSE )





#### Get data from all ARU locations per year selected ####

HE_2024 <- all_HEdata %>%
  filter(year== 2024)%>%
  mutate(tag_duration = end - startTime) %>%
  filter(date < "2024-05-30") #filter to your dates of interest. I did it here so that only spring detections are included


#add banding codes since Wildtrax upload works with banding codes only. Banding code will be called "species" to match WT
banding_codes <- read.csv("species_codes_WT.csv")

HE_2024 <- HE_2024 %>%
  left_join(banding_codes, by = "common")

unique(HE_2024$species) #check for NAs for missing banding code matches. This may happen if your species changed name or banding code, and thus my banding codes are out of date


#for our analyses we will only use detections above 0.9 confidence. I found this threshold worked well with Hawkears
HE_2024_sub <- subset(HE_2024, confidence >= 0.9)

#keep only the 3 highest confidence detections for each species per ARU location, and remove repeated detections of species within the same recording
HE_2024_TOP3 <- HE_2024_sub %>% arrange(desc(confidence)) %>% distinct(filename, common, .keep_all = TRUE)%>%
  group_by(site, location_code, common) %>%
  slice(1:3)

write.csv(HE_2024_TOP3, file = "HE_2024_TOP3.csv", row.names=FALSE)


#edit dataframe to match Wildtrax tag upload file. This no longer works! modify code according to script #6

HE_2024_TOP3 <- HE_2024_TOP3 %>%
  rename(tag_start_time = startTime, recording_date_time = recordingDate, species_code = species )

HE_2024_TOP3$task_method = "1SPT"
HE_2024_TOP3$task_duration = "599.85"
HE_2024_TOP3$observer = "Not Assigned"
HE_2024_TOP3$individual_number = "1"
HE_2024_TOP3$vocalization = "Song"
HE_2024_TOP3$abundance = "1"
HE_2024_TOP3$min_tag_freq = "0"
HE_2024_TOP3$max_tag_freq = "12000"
HE_2024_TOP3$species_individual_comments =""
HE_2024_TOP3$internal_tag_id = ""
HE_2024_TOP3$tag_is_hidden_for_verification = "f"
HE_2024_TOP3$recording_sample_frequency = "44100"

WT_ALLupload_2024 <- HE_2024_TOP3
WT_ALLupload_2024$end <- NULL
WT_ALLupload_2024$common <- NULL
WT_ALLupload_2024$confidence <- NULL
WT_ALLupload_2024$filename <- NULL
WT_ALLupload_2024$datetime_software <- NULL
WT_ALLupload_2024$year <- NULL
WT_ALLupload_2024$month <- NULL
WT_ALLupload_2024$day <- NULL
WT_ALLupload_2024$date <- NULL
WT_ALLupload_2024$rec_time <- NULL
WT_ALLupload_2024$location_code <- NULL
WT_ALLupload_2024$site <- NULL

#Wildtrax now only accepts recordings of more than 30 seconds, so any detection with a tagLength of more than 30 seconds
#will be converted to 29 seconds. This won't be needed for Birdnet tags, as they are always 3s each

WT_ALLupload_2024 <- WT_ALLupload_2024%>%
  mutate(tagLength = ifelse(tagLength > 30, 29, tagLength))


write.csv(WT_ALLupload_2024, "WT_uploads/WT_2024_ALLtags_upload.csv", row.names=FALSE )

# Then upload that file to Wildtrax! 