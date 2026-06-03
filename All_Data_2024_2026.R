




library(tidyverse)



## Setup ####

# GeoWiki data directory
dir_geowiki <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_Second/geowiki_2026/"

# Extract date (YYYYMMDD)
date_part_1 <- "20260507"



# 1st Assessment data directory
dir_old_results <- "/Users/xavi_rp/Documents/JRC_D1/copy_SharePoint_kk/validation/Results/"

dir_1stAssessment <- "/Users/xavi_rp/Documents/JRC_D1/copy_SharePoint_kk/validation/Results/final_v2/"





## 2024 first round ####
primary_data_latest <- read.csv(paste0(dir_old_results, "primary_data_latest.csv"))

nrow(primary_data_latest) # 21752
names(primary_data_latest) 
View(primary_data_latest[1:10, ]) # 21752


valid_2024_1st <- primary_data_latest %>% 
  select(userid, email, 
         groupid,
         pixel_center_x, pixel_center_y,
         sample_id,
         location_id,
         name.1,
         name.3,
         name.9,
         name.7,
         comment) %>% 
  rename(
    "X2024_1st_userid" = "userid",
    "X2024_1st_email" = "email",
    "X2024_1st_groupid" = "groupid",
    "X2024_1st_pixel_center_x" = "pixel_center_x",
    "X2024_1st_pixel_center_y" = "pixel_center_y",
    "X2024_1st_sample_id" = "sample_id",
    #"X2024_1st_location_id" = "location_id",
    "X2024_1st_forest_class" = "name.1",
    "X2024_1st_confidence_level" = "name.3",
    "X2024_1st_type_class" = "name.9",
    "X2024_1st_class_issues" = "name.7",
    "X2024_1st_comment" = "comment"
    ) #%>% 

nrow(valid_2024_1st)  # 21752
View(valid_2024_1st[1:10, ]) # 21752

apply(valid_2024_1st, 2, function(x) sum(is.na(x)))
#unique(valid_2024_1st$X2024_1st_comment)




## 2024 second round ####
secondary_data_latest <- read.csv(paste0(dir_old_results, "secondary_data_latest_adjusted.csv"))

nrow(secondary_data_latest) # 4000
names(secondary_data_latest) 
View(secondary_data_latest[1:10, ]) 


valid_2024_2nd <- secondary_data_latest %>% 
  select(userid, email, 
         groupid,
         pixel_center_x, pixel_center_y,
         sample_id,
         location_id,
         name.1,
         name.3,
         name.7,
         name.5,
         comment.3) %>% 
  rename(
    "X2024_2nd_userid" = "userid",
    "X2024_2nd_email" = "email",
    "X2024_2nd_groupid" = "groupid",
    "X2024_2nd_pixel_center_x" = "pixel_center_x",
    "X2024_2nd_pixel_center_y" = "pixel_center_y",
    "X2024_2nd_sample_id" = "sample_id",
    #"X2024_2nd_location_id" = "location_id",
    "X2024_2nd_forest_class" = "name.1",
    "X2024_2nd_confidence_level" = "name.3",
    "X2024_2nd_type_class" = "name.7",
    "X2024_2nd_class_issues" = "name.5",
    "X2024_2nd_comment" = "comment.3"
  ) #%>% 

nrow(valid_2024_2nd)  # 4000
View(valid_2024_2nd[1:10, ]) 

apply(valid_2024_2nd, 2, function(x) sum(is.na(x)))





## 2024 final decision ####
full_2024_recreated <- read.csv(paste0(dir_1stAssessment, "final_2024_recreated.csv")) 

nrow(full_2024_recreated) # 21728
nrow(primary_data_latest) - nrow(full_2024_recreated)  # 24 not assigned after 2024 
