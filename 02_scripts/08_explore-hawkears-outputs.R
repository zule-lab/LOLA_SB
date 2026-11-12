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


# 1. Check output is as expected ------------------------------------------

# Check what files don't have hawkears outputs

hawkears.output.files <-list.files(path = "03_outdata/HawkEars_output", pattern = "\\.txt$", full.names = TRUE)
str(hawkears.output.files)
ames(hawkears.output.files) <- "filename"

# Extract core file name
hawkears.output.files$core.name <- sub("_scores.txt", "", hawkears.output.files$filename) %>% 
  sub("03_outdata/HawkEars_output/","",.)

# Get list of all files 

setwd("H:\\acoustic-data\\2026\\MTL\\working-folder")
audio.files <-list.files(pattern = ".wav$", full.names = FALSE, recursive=TRUE) %>% 
  as.data.frame()
names(audio.files) <- "filename"
audio.files$core.name <- sub(".*/", "", audio.files$filename) %>% 
  sub(".wav","", .)

missing.files <- setdiff(audio.files$core.name,hawkears.output.files$core.name)

nrow(audio.files) - nrow(hawkears.output.files)


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

all_HEdata_spring <- lapply(mega_file_list_spring, function(file) {
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

View(all_HEdata_spring)

# Extract site name from filename

all_HEdata_spring.1 <- all_HEdata_spring %>% 
  mutate(filename.sans.MTL=sub("MTL_", "", filename)) %>% # removes the first part of filename
  mutate(location=sub("_.*", "", filename.sans.MTL)) %>%  #this substracts only the portion of the filename that comes before the underscore
  mutate(filename.sans.site=sub("^[^_]*_","",filename.sans.MTL)) %>% # removes everything before first underscore
  mutate(date=sub("_.*", "", filename.sans.site)) %>% # remove everything after underscore
  select(-c(filename.sans.site, filename.sans.MTL)) %>% 
  mutate(date=as.Date(date, "%Y%m%d")) %>% 
  filter(Common.name %in% target_list)
  
str(all_HEdata_spring.1) # looks good!


# 2. Get SR per site ------------------------------------------------------

# Quickly getting overall SR

setdiff(target_list, unique(all_HEdata_spring.1$Common.name))
# Output: [1] "BITH" "BWWA" NA     "AMGO" "DICK"

# Species richness per site 

spp_per_site <- all_HEdata_spring.1 %>% 
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

summary_counts_long <- all_HEdata_spring.1 %>%
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

all_HEdata_summer <- lapply(mega_file_list_summer, function(file) {
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

View(all_HEdata_summer)

# Extract site name from filename

all_HEdata_summer.1 <- all_HEdata_summer %>% 
  mutate(filename.sans.MTL=sub("MTL_", "", filename)) %>% # removes the first part of filename
  mutate(location=sub("_.*", "", filename.sans.MTL)) %>%  #this substracts only the portion of the filename that comes before the underscore
  mutate(filename.sans.site=sub("^[^_]*_","",filename.sans.MTL)) %>% # removes everything before first underscore
  mutate(date=sub("_.*", "", filename.sans.site)) %>% # remove everything after underscore
  select(-c(filename.sans.site, filename.sans.MTL)) %>% 
  mutate(date=as.Date(date, "%Y%m%d")) %>% 
  filter(Common.name %in% target_list)

str(all_HEdata_summer.1) # looks good!


# 2. Get SR per site ------------------------------------------------------

# This part of the script was based on a script from Barbara Frei 

# Species richness per site 

spp_per_site <- all_HEdata_summer.1 %>% 
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

summary_counts_long <- all_HEdata_summer.1 %>%
  group_by(location, Common.name) %>%
  summarise(Count = n(), .groups = 'drop') 


list_of_tables <- split(summary_counts_long, summary_counts_long$location)

# Export using lapply
lapply(names(list_of_tables), function(x) {
  write.csv(list_of_tables[[x]], file = paste0("03_outdata/prelim-HawkEars-summary/SUMMER/", x, "_SUMMER.csv"), row.names = FALSE)
})


# Heatmap -----------------------------------------------------------------


