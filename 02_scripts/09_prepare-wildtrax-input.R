# Date Created: 2026-10-30
# Date last modified: 2026-11-02
# Original script from Ana Morales, modified by Siena Blier

###############################################################################
## Preparing tags to upload to WildTrax.
###############################################################################


# 0. Load libraries -------------------------------------------------------


library(tidyverse)
library(dplyr)
library(ggplot2)


# 1. Wrangle HawkEars output -------------------------------------------

# Get a list of all text files in the directory
file_list_spring_bats <- list.files(path = "03_outdata/HawkEars_output_spring_bats", pattern = "\\.txt$", full.names = TRUE)
file_list_spring_birds <- list.files(path = "03_outdata/HawkEars_output_spring_birds", pattern = "\\.txt$", full.names = TRUE)

# Combine
mega_file_list_spring <- c(file_list_spring_bats,file_list_spring_birds)


# Define the column names you want to assign. Since hawkears txt files don't have column names
column_names <- c("Start (s)", "End (s)", "Common name")  # Adjust as per your file's structure


# make a list of species you want to extract data on. 

species_list_df <- read.csv("01_rawdata/acoustic/species_list.csv", sep = "_") # Check with Barbara
species_codes <- read.csv("01_rawdata/acoustic/species_codes_WT.csv")
target_species_df <- left_join(species_list_df,species_codes,by=join_by(Common.name==common))
target_list <- c(target_species_df$species)



# Read and combine all files into one dataframe, adding the filename as a column
# It will skip files that have no detections

all_HEdata <- lapply(mega_file_list_spring, function(file) {
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
    tidyr::separate("Common name", into = c("Common.name", "Confidence"), sep = ";", extra = "merge", fill = "right")
  
  # Add the filename as a new column
  temp_data$filename <- basename(file)  # Extract just the file name (without path)
  
  return(temp_data)
}) %>%
  bind_rows() 

View(all_HEdata)

# Extract site name from filename

all_HEdata.1 <- all_HEdata %>% 
  mutate(filename.sans.MTL=sub("MTL_", "", filename)) %>% # removes the first part of filename
  mutate(location=sub("_.*", "", filename.sans.MTL)) %>%  #this substracts only the portion of the filename that comes before the underscore
  mutate(filename.sans.site=sub("^[^_]*_","",filename.sans.MTL)) %>% # removes everything before first underscore
  mutate(date=sub("_.*", "", filename.sans.site)) %>% # remove everything after underscore
  select(-c(filename.sans.site, filename.sans.MTL)) %>% 
  mutate(date=as.Date(date, "%Y%m%d")) %>% 
  filter(Common.name %in% target_list)
  
str(all_HEdata.1) # looks good!


# 2. Get SR per site ------------------------------------------------------

# Quickly getting overall SR

setdiff(target_list, all_HEdata.1$Common.name)
# Output: [1] "BITH" "BWWA" NA     "AMGO" "DICK"

# Species richness per site 

spp_per_site <- all_HEdata.1 %>% 
  group_by(location) %>% 
  summarize(SR = n_distinct(Common.name)) # from: https://r-statistics.co/dplyr-n_distinct-in-R.html

spp_per_site

# Make cute bar graph

# Add column to data that indicates the type of ARU
spp_per_site_1 <- mutate(spp_per_site, sampling_fr = ifelse(str_ends(location, "-1"), "24,000", "44,100"))

# From: https://github.com/zule-lab/MontrealTrees_LandUseTypes/blob/main/scripts/1.TreeCleaningRows.R
# And: https://r-statistics.co/stringr-str_ends-in-R.html

ggplot(spp_per_site_1, aes(x=location, y=SR, fill=sampling_fr)) +
  geom_col() +
  labs(x = "Site", y = "SR") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  ggtitle("Species richness per site - SPRING")
  

# From: https://www.datanovia.com/learn/data-visualization/ggplot2/barplot
# And: https://www.statology.org/rotate-axis-labels-ggplot2/
# And: https://r-charts.com/ggplot2/titles/

# 3. Get species list per site --------------------------------------------

# This part of the script was based on a script from Barbara Frei 

summary_counts_long <- all_HEdata.1 %>%
  group_by(location, Common.name) %>%
  summarise(Count = n(), .groups = 'drop') 


list_of_tables <- split(summary_counts_long, summary_counts_long$location)

# Export using lapply
lapply(names(list_of_tables), function(x) {
  write.csv(list_of_tables[[x]], file = paste0("03_outdata/prelim-HawkEars-summary/SPRING/", x, "_SPRING.csv"), row.names = FALSE)
})



# 4. Repeat steps 1, 2 & 3 on summer data ------------------------------------

file_list_summer_bats <- list.files(path = "03_outdata/HawkEars_output_summer_bats", pattern = "\\.txt$", full.names = TRUE)
file_list_summer_birds <- list.files(path = "03_outdata/HawkEars_output_summer_birds", pattern = "\\.txt$", full.names = TRUE)

# Combine
mega_file_list_summer <- c(file_list_summer_bats,file_list_summer_birds)


# Read and combine all files into one dataframe, adding the filename as a column
# It will skip files that have no detections

all_HEdata <- lapply(mega_file_list_summer, function(file) {
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
    tidyr::separate("Common name", into = c("Common.name", "Confidence"), sep = ";", extra = "merge", fill = "right")
  
  # Add the filename as a new column
  temp_data$filename <- basename(file)  # Extract just the file name (without path)
  
  return(temp_data)
}) %>%
  bind_rows() 

View(all_HEdata)

# Extract site name from filename

all_HEdata.1 <- all_HEdata %>% 
  mutate(filename.sans.MTL=sub("MTL_", "", filename)) %>% # removes the first part of filename
  mutate(location=sub("_.*", "", filename.sans.MTL)) %>%  #this substracts only the portion of the filename that comes before the underscore
  mutate(filename.sans.site=sub("^[^_]*_","",filename.sans.MTL)) %>% # removes everything before first underscore
  mutate(date=sub("_.*", "", filename.sans.site)) %>% # remove everything after underscore
  select(-c(filename.sans.site, filename.sans.MTL)) %>% 
  mutate(date=as.Date(date, "%Y%m%d")) %>% 
  filter(Common.name %in% target_list)

str(all_HEdata.1) # looks good!


# 2. Get SR per site ------------------------------------------------------

# This part of the script was based on a script from Barbara Frei 

# Species richness per site 

spp_per_site <- all_HEdata.1 %>% 
  group_by(location) %>% 
  summarize(SR = n_distinct(Common.name)) # from: https://r-statistics.co/dplyr-n_distinct-in-R.html

spp_per_site

# Make cute bar graph

# Add column to data that indicates the type of ARU
spp_per_site_1 <- mutate(spp_per_site, sampling_fr = ifelse(str_ends(location, "-1"), "24,000", "44,100"))

# From: https://github.com/zule-lab/MontrealTrees_LandUseTypes/blob/main/scripts/1.TreeCleaningRows.R
# And: https://r-statistics.co/stringr-str_ends-in-R.html

ggplot(spp_per_site_1, aes(x=location, y=SR, fill=sampling_fr)) +
  geom_col() +
  labs(x = "Site", y = "SR") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  ggtitle("Species richness per site - SUMMER")


# From: https://www.datanovia.com/learn/data-visualization/ggplot2/barplot
# And: https://www.statology.org/rotate-axis-labels-ggplot2/
# And: https://r-charts.com/ggplot2/titles/

# 3. Get species list per site --------------------------------------------

# This part of the script was based on a script from Barbara Frei 

summary_counts_long <- all_HEdata.1 %>%
  group_by(location, Common.name) %>%
  summarise(Count = n(), .groups = 'drop') 


list_of_tables <- split(summary_counts_long, summary_counts_long$location)

# Export using lapply
lapply(names(list_of_tables), function(x) {
  write.csv(list_of_tables[[x]], file = paste0("03_outdata/prelim-HawkEars-summary/SUMMER/", x, "_SUMMER.csv"), row.names = FALSE)
})












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