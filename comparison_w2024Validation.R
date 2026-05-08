


library(tidyverse)
library(readxl)



## Setup ####

# GeoWiki data directory
dir_geowiki <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_Second/geowiki_2026/"


# 1st Assessment data directory
dir_1stAssessment <- "/Users/xavi_rp/Documents/JRC_D1/copy_SharePoint_kk/validation/Results/final_v2/"

# Extract date (YYYYMMDD)
date_part <- "20260507"



## Data ingestion ####

## GeoWiki 2026

data_geowiki_clean <- read.csv(paste0(dir_geowiki, date_part, "_data_latest_adjusted.csv"))
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
nrow(data_1stAssessment)  # 21612, this is correct


unique(data_1stAssessment$forest_class_3)   # "Forest" / "Non-forest"
unique(data_1stAssessment$forest_class_num_3)   # 1 / 0
unique(data_1stAssessment$confidence_level_3)   # "high confidence" / "low confidence"
unique(data_1stAssessment$class_issues_3)   # "no issues" / "no response data" / "low resolution" / "Multiple land uses" / "forest to be regrown" "Open treed land use"  "Other issues"         "cloud cover"
unique(data_1stAssessment$type_class_3)   # "Naturally regenerating forest" / "no trees or shrubs present" / "trees outside forest" / "trees inside forest" / "other wooded land" / "trees for agricultural use" / "Planted or plantation forest" / "trees in urban areas" 
unique(data_1stAssessment$GFC_v2)   # 1 / 0
unique(data_1stAssessment$GFC_v2_noMMU)   # 1 / 0





## Merging datasets ####
names(data_1stAssessment)
names(data_geowiki)

sort(unique(data_1stAssessment$sample_id))
sort(unique(data_geowiki$sample_id))

sum(data_1stAssessment$sample_id %in% data_geowiki$sample_id)   # 'sample_id' is not the same in both datasets. Not useful for merging
sum(data_geowiki$sample_id %in% data_1stAssessment$sample_id)

range(data_1stAssessment$sample_id)  # 1950942 1972693
range(data_geowiki$sample_id)        # 2215688 2229451



# merging by 'location_id'

range(data_1stAssessment$location_id)  # 1906318 2758358
range(data_geowiki$location_id)        # 1906318 2758358

sum(data_1stAssessment$location_id %in% data_geowiki$location_id)   # 13703
sum(data_geowiki$location_id %in% data_1stAssessment$location_id)   # 13703

13764 - 13703  # 61 missing points


# rows from data_geowiki with no match in data_1stAssessment
missing_points <- anti_join(data_geowiki, data_1stAssessment, by = "location_id")

nrow(missing_points)
View(missing_points)

table(missing_points$GFC.validation...forest)


write.csv(missing_points, 
          paste0(dir, date_part, "_comparison_2026_2024_missing_points.csv"), 
          row.names = FALSE)





# merging both datasets

#data_2026_2024 <- full_join(data_geowiki, data_1stAssessment, by = "location_id")  # keep all rows from both
data_2026_2024 <- left_join(data_geowiki, data_1stAssessment, by = "location_id")

head(data_2026_2024)
nrow(data_2026_2024) # 13764
View(data_2026_2024)

names(data_2026_2024)

write.csv(data_2026_2024, 
          paste0(dir, date_part, "_comparison_2026_2024.csv"), 
          row.names = FALSE)


# Cleaning the table

data_2026_2024_clean <- data_2026_2024 %>%
  select(1:21, 
         "sample_id.y",
         "forest_class_3",                               
         "forest_class_num_3", 
         "confidence_level_3",                           
         "class_issues_3",     
         "type_class_3",                                 
         "continent",          
         "GEZ_CODE",                                     
         "strata",
         "GFC_v2",
         "GFC_v2_noMMU",
         "location_i",                                   
         "strata_gaul",
         "gaul_opt1",
         "continent_gaul"
         ) %>% 
  rename(
    "2024_sample_id.y" = "sample_id.y",
    "2024_forest_class_3" = "forest_class_3",
    "2024_forest_class_num_3" = "forest_class_num_3",
    "2024_confidence_level_3" = "confidence_level_3",
    "2024_class_issues_3" = "class_issues_3",
    "2024_type_class_3" = "type_class_3",
    "2024_continent" = "continent",
    "2024_GEZ_CODE" = "GEZ_CODE",
    "2024_strata" = "strata",
    "2024_GFC_v2" = "GFC_v2",
    "2024_GFC_v2_noMMU" = "GFC_v2_noMMU",
    "2024_location_i" = "location_i",
    "2024_strata_gaul" = "strata_gaul",
    "2024_gaul_opt1" = "gaul_opt1",
    "2024_continent_gaul" = "continent_gaul"
  ) #%>% names()



write.csv(data_2026_2024_clean, 
          paste0(dir, date_part, "_comparison_2026_2024_clean.csv"), 
          row.names = FALSE)









## Selection criteria ####


## "Forest" / "Non-forest"

diff_FOR_NONFOR <- data_2026_2024 %>%
  filter(GFC.validation...forest != forest_class_3)

nrow(diff_FOR_NONFOR) # 882
View(diff_FOR_NONFOR)

FOR_NONFOR_comp <- table(diff_FOR_NONFOR$GFC.validation...forest) 
# Forest    no assignment    Non-forest 
#  343            12           527 

FOR_NONFOR_comp
sum(FOR_NONFOR_comp)  # 882 samples that have been differently assigned;
                      # Not considered those NAs in 2024 (missing points)


pivot_tab_FOR_NONFOR <- data_2026_2024 %>%
  count(GFC.validation...forest, forest_class_3) %>%
  pivot_wider(
    names_from = GFC.validation...forest,
    values_from = n,
    values_fill = 0
  ) %>% as.data.frame()

pivot_tab_FOR_NONFOR

#                   GFC.validation...forest
# forest_class_3    Forest Non-forest   no assignment
#         Forest      3863        527               3
#     Non-forest       343       8958               9
#           <NA>         7         53               1




## "Forest" / "Non-forest" + Confidence (in 2026)

diff_FOR_NONFOR

table(diff_FOR_NONFOR$GFC.validation...confidence)

# high confidence    low confidence     no assignment 
#      562               310              10 




## "Forest" / "Non-forest" + Strata

diff_FOR_NONFOR

table(diff_FOR_NONFOR$groupid) 

# strata:  363  364  365  366  367  368  369  370  371 
#   freq:  110  101   74   92   92   51   57  266   39 

check_completeness <- read.csv(paste0(dir, date_part, "_check_completeness.csv"))
str(check_completeness)

diff_FOR_NONFOR_strata <- table(diff_FOR_NONFOR$groupid) %>%
  data.frame() %>% #str()
  mutate(Var1 = as.integer(as.character(Var1))) %>% #str()
  right_join(check_completeness, 
             by = c("Var1" = "groupid_2026")) %>%
  select(Var1, "Region...Countries", Sample_units_validated_2026, Freq) %>%
  rename(Strata = Var1, Differently_Assigned = Freq)


diff_FOR_NONFOR_strata_ft <- flextable::flextable(diff_FOR_NONFOR_strata)
flextable::save_as_image(diff_FOR_NONFOR_strata_ft, 
                         path = paste0(dir, date_part, "_diff_FOR_NONFOR_strata_ft.png"))



#







## Data for re-ingestion ####

cols_included <- c("groupid",	"sample_id",	"location_id")




## Dataset for tie callers ####

# Prepare a file which shows the tie caller the class selection in the first assessment and in the ongoing assessment


#




