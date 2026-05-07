


library(tidyverse)
library(readxl)



## Setup ####

# GeoWiki data directory
dir_geowiki <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_Second/geowiki_2026/"

dir_geowiki <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_Second/kk/"


# 1st Assessment data directory
dir_1stAssessment <- "/Users/xavi_rp/Documents/JRC_D1/copy_SharePoint_kk/validation/Results/final_v2/"



## Data ingestion ####

## GeoWiki 2026
data_geowiki_clean <- read.csv(paste0(dir_geowiki, "secondary_data_latest_adjusted.csv"))
data_geowiki <- data_geowiki_clean

names(data_geowiki)

data_geowiki %>% 
  select(validation_id, sample_id, starts_with("GFC")) %>%
  head()


## data 1st Assessment
data_1stAssessment <- read_excel(paste0(dir_1stAssessment, "GFC_v2_accuracy-assessment_gaul.xlsx"), 
                                 sheet = "5_combined")

head(data.frame(data_1stAssessment))
names(data_1stAssessment)
nrow(data_1stAssessment)  # 21612


unique(data_1stAssessment$forest_class_3)   # "Forest" / "Non-forest"
unique(data_1stAssessment$forest_class_num_3)   # 1 / 0
unique(data_1stAssessment$confidence_level_3)   # "high confidence" / "low confidence"
unique(data_1stAssessment$class_issues_3)   # "no issues" / "no response data" / "low resolution" / "Multiple land uses" / "forest to be regrown" "Open treed land use"  "Other issues"         "cloud cover"
unique(data_1stAssessment$type_class_3)   # "Naturally regenerating forest" / "no trees or shrubs present" / "trees outside forest" / "trees inside forest" / "other wooded land" / "trees for agricultural use" / "Planted or plantation forest" / "trees in urban areas" 
unique(data_1stAssessment$GFC_v2)   # 1 / 0
unique(data_1stAssessment$GFC_v2_noMMU)   # 1 / 0





## Selection criteria ####






## Data for re-ingestion ####

cols_included <- c("groupid",	"sample_id",	"location_id")




## Dataset for tie callers ####

# Prepare a file which shows the tie caller the class selection in the first assessment and in the ongoing assessment


#




