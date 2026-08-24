# Siena Blier
# 24APRIL2026
# Last modified: 24APRIL2026

###############################################################################
## Creating a spreadsheet to organize my permit stuff. 
###############################################################################


# 0. Load files + libraries -----------------------------------------------

library(dplyr)
library(tidyr)

original.sheet <- read.csv("100m-selection.csv")
data_split <- data %>%
  separate(Text, into = c("Name", "Age"), sep = 10)

# 1. Re-organize sheet ----------------------------------------------------


str(original.sheet)

new.sheet <- original.sheet %>% pivot_longer(
  cols = Public.mini.hex..1:Private.mini.hex..2,
  names_to = "mini.hex.type",
  values_to = "mini.hex.id"
)

str(new.sheet)

new.sheet.1 <- new.sheet %>% 
  select(Density, Context, mini.hex.type, landscape_, Borough, mini.hex.id, 
         CARLY...BARBARA.NOTES)

str(new.sheet)

unique(new.sheet.1$mini.hex.type)
new.sheet.2 <- new.sheet.1 %>% separate(mini.hex.type, into=c("mh.tenure", "mn.number"), sep=".mini.hex..") 
str(new.sheet.2)

# Export!

write.csv(new.sheet.2, "permit-org-sheet.csv")
