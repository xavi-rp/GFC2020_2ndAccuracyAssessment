




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
#View(primary_data_latest[1:10, ]) # 21752


valid_2024_1st <- primary_data_latest %>% 
  select(userid, email, 
         groupid,
         pixel_center_x, pixel_center_y,  # 85 do not have coordinates (missing coordinates already in 'primary_data_latest.csv')
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
#View(valid_2024_1st[1:10, ]) # 21752

apply(valid_2024_1st, 2, function(x) sum(is.na(x)))
#unique(valid_2024_1st$X2024_1st_comment)

#valid_2024_1st %>% filter(is.na(X2024_1st_pixel_center_x)) %>% View()



## 2024 second round ####
secondary_data_latest <- read.csv(paste0(dir_old_results, "secondary_data_latest_adjusted.csv"))

nrow(secondary_data_latest) # 4000
names(secondary_data_latest) 
#View(secondary_data_latest[1:10, ]) 


valid_2024_2nd <- secondary_data_latest %>% 
  select(userid, email, 
         groupid,
         #pixel_center_x, pixel_center_y,
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
    #"X2024_2nd_pixel_center_x" = "pixel_center_x",
    #"X2024_2nd_pixel_center_y" = "pixel_center_y",
    "X2024_2nd_sample_id" = "sample_id",
    #"X2024_2nd_location_id" = "location_id",
    "X2024_2nd_forest_class" = "name.1",
    "X2024_2nd_confidence_level" = "name.3",
    "X2024_2nd_type_class" = "name.7",
    "X2024_2nd_class_issues" = "name.5",
    "X2024_2nd_comment" = "comment.3"
  ) #%>% 

nrow(valid_2024_2nd)  # 4000
#View(valid_2024_2nd[1:10, ]) 

apply(valid_2024_2nd, 2, function(x) sum(is.na(x)))





## 2024 final decision ####
full_2024_recreated <- read.csv(paste0(dir_1stAssessment, "final_2024_recreated.csv")) 

nrow(full_2024_recreated) # 21728
nrow(primary_data_latest) - nrow(full_2024_recreated)  # 24 not assigned after 2024 

head(full_2024_recreated)
apply(full_2024_recreated, 2, function(x) sum(is.na(x)))


valid_2024_final <- full_2024_recreated %>%
  select(starts_with(c("loc", "X2024"))) %>%
  rename_with(~ str_replace(.x, "^X2024", "X2024_final")) 

head(valid_2024_final)
nrow(valid_2024_final) # 21728 

sort(unique(valid_2024_final$X2024_final_groupid))
apply(valid_2024_final, 2, function(x) sum(is.na(x)))






## 2026 first round ####

date_part_2 <- 20260622
#data_2026_2024_clean <- read.csv(paste0(dir_geowiki, date_part_1, "_comparison_2026_2024_clean.csv"))
data_2026_all <- read.csv(paste0(dir_geowiki, date_part_2, "_data_latest.csv"))

data_2026_all[1:2, ]
names(data_2026_all)
nrow(data_2026_all)  # 17825

range(data_2026_all$timestamp)  #  "2026-03-02 13:46:20.698629" "2026-06-18 16:09:47.510572"


data_2026_all %>% 
  filter(timestamp < as.Date("2026/05/19") | timestamp >= as.Date("2026/06/18")) %>%  #nrow() # 13764
  summarise("1st_1st" = sum(timestamp < as.Date("2026/05/19")),
            "1st_revision" = sum(timestamp >= as.Date("2026/06/18")),
            "all" = nrow(.))
#   1st_1st   1st_revision     all
#   13553              211   13764



valid_2026_1st <- data_2026_all %>% 
  filter(timestamp < as.Date("2026/05/19") | timestamp >= as.Date("2026/06/18")) %>% #names()
  select(location_id, userid, email, groupid, sample_id,  
         pixel_center_x, pixel_center_y,
         starts_with("GFC."), comment) %>%    # pull(groupid) %>% unique() %>% sort()
  mutate(type_class = coalesce(GFC.validation...forest.type, GFC.validation...other.land.use.type)) %>% #head()
  select(-GFC.validation...forest.type, -GFC.validation...other.land.use.type) %>% 
  rename(
    "X2026_1st_userid" = "userid",
    "X2026_1st_email" = "email",
    "X2026_1st_groupid" = "groupid",
    "X2026_1st_pixel_center_x" = "pixel_center_x",
    "X2026_1st_pixel_center_y" = "pixel_center_y",
    "X2026_1st_sample_id" = "sample_id",
    "X2026_1st_forest_class" = "GFC.validation...forest",
    "X2026_1st_confidence_level" = "GFC.validation...confidence",
    "X2026_1st_type_class" = "type_class",
    "X2026_1st_class_issues" = "GFC.validation...Issues.with.class.assignment",
    "X2026_1st_comment" = "comment"
  ) %>%
  relocate(X2026_1st_type_class, .before = X2026_1st_class_issues) #%>% head()



valid_2026_1st[1:2, ]
nrow(valid_2026_1st)   # 13764
apply(valid_2026_1st, 2, function(x) sum(is.na(x))) # 0
sort(unique(valid_2026_1st$X2026_1st_forest_class))     # no 'no assignment'
sort(unique(valid_2026_1st$X2026_1st_confidence_level)) # no 'no assignment'
sort(unique(valid_2026_1st$X2026_1st_type_class))       # no 'no assignment'
sort(unique(valid_2026_1st$X2026_1st_class_issues))     # no 'no assignment'
sum(str_detect(as.matrix(valid_2026_1st), regex("no assignment", ignore_case = TRUE)), na.rm = TRUE) # 0
#valid_2026_1st %>% filter(if_any(everything(), ~ str_detect(as.character(.x), regex("ass", ignore_case = TRUE))))


sum(!valid_2026_1st$location_id %in% valid_2024_final$location_id)  # 11 which couldn't be assigned in the 2024 final round
sum(!valid_2026_1st$location_id %in% valid_2024_1st$location_id)  # 0






## 2026 Tie Call ####

data_2026_all %>% 
  filter(timestamp >= as.Date("2026/05/19") & timestamp < as.Date("2026/06/18")) %>%  #nrow()   # 4061
  summarise("Tie_1st" = sum(timestamp < as.Date("2026/06/15")),
            "Tie_revision" = sum(timestamp >= as.Date("2026/06/15")),
            "all" = nrow(.))
#   Tie_1st   Tie_revision     all
#      3997             64    4061


# samples that shouldn't have been selected for the Tie Call

samples_reingestion_2026_wrong <- read.csv(paste0(dir_geowiki, date_part_1, "_samples_reingestion_2026_wrong.csv"))

nrow(samples_reingestion_2026_wrong) # 86
samples_reingestion_2026_wrong %>% #head()
  pull(location_id) 


valid_2026_Tie <- data_2026_all %>% 
  filter(timestamp >= as.Date("2026/05/19") & timestamp < as.Date("2026/06/18")) %>%  #nrow()   # 4061
  #names()
  filter_out(location_id %in% (samples_reingestion_2026_wrong %>% pull(location_id))) %>% #nrow()  # 3975 (4061 - 86)
  #names()
  select(location_id, userid, email, groupid, sample_id,  
         pixel_center_x, pixel_center_y,
         starts_with("GFC."), comment) %>%    # pull(groupid) %>% unique() %>% sort()
  mutate(type_class = coalesce(GFC.validation...forest.type, GFC.validation...other.land.use.type)) %>% #head()
  select(-GFC.validation...forest.type, -GFC.validation...other.land.use.type) %>% 
  rename(
    "X2026_TieCall_userid" = "userid",
    "X2026_TieCall_email" = "email",
    "X2026_TieCall_groupid" = "groupid",
    "X2026_TieCall_pixel_center_x" = "pixel_center_x",
    "X2026_TieCall_pixel_center_y" = "pixel_center_y",
    "X2026_TieCall_sample_id" = "sample_id",
    "X2026_TieCall_forest_class" = "GFC.validation...forest",
    "X2026_TieCall_confidence_level" = "GFC.validation...confidence",
    "X2026_TieCall_type_class" = "type_class",
    "X2026_TieCall_class_issues" = "GFC.validation...Issues.with.class.assignment",
    "X2026_TieCall_comment" = "comment"
  ) %>%
  relocate(X2026_TieCall_type_class, .before = X2026_TieCall_class_issues) #%>% head()






## All rounds together ####

valid_2024_1st %>% head()
valid_2024_2nd %>% head()
valid_2024_1st %>% nrow()  #  21752
valid_2024_2nd %>% nrow()  #   4000
valid_2026_1st %>% nrow()  #  13764
valid_2026_Tie %>% nrow()  #   3975
unique(valid_2024_1st$location_id) %>% length()
unique(valid_2024_2nd$location_id) %>% length()
unique(valid_2026_1st$location_id) %>% length()
unique(valid_2026_Tie$location_id) %>% length()


valid_all <- valid_2024_1st %>% 
  full_join(valid_2024_2nd, by = "location_id") %>%
  relocate(location_id) %>% 
  full_join(valid_2024_final, by = "location_id") %>% 
  full_join(valid_2026_1st, by = "location_id") %>% 
  full_join(valid_2026_Tie, by = "location_id")


valid_all %>% head()
valid_all %>% nrow()  # 21752
apply(valid_all, 2, function(x) sum(is.na(x)))

# I need to fix 85 missing coordinates from 2024_1st
# I saw in Astrid's notes that at some point Martina sent these 85 missing coordinates
# Most probably I can get them from 2024 final


apply(valid_all, 2, function(x) sum(!is.na(x)))

valid_all %>% 
  select(contains("sample_id")) %>% #head()
  apply(., 2, function(x) sum(!is.na(x)))
 
# X2024_1st_sample_id     X2024_2nd_sample_id   X2024_final_sample_id     X2026_1st_sample_id     X2026_TieCall_sample_id 
#       21752                    4000                   21728                   13764                    3975 




## Save data set
write.csv(valid_all, 
          #paste0(dir_geowiki, "../AllValidations_2024_2026.csv"), 
          paste0(dir_geowiki, "AllValidations_2024_2026.csv"), 
          row.names = FALSE)




## Check that callers (users) have the same user_id along the calls ####

user_check_1 <- valid_all %>% 
  select(X2024_1st_userid, X2024_1st_email) %>% 
  distinct() %>%
  arrange(X2024_1st_userid)

user_check_2 <- valid_all %>% 
  select(X2024_2nd_userid, X2024_2nd_email) %>% 
  distinct() %>%
  arrange(X2024_2nd_userid)

user_check_3 <- valid_all %>% 
  select(X2026_1st_userid, X2026_1st_email) %>% 
  distinct() %>%
  arrange(X2026_1st_userid)

user_check_4 <- valid_all %>% 
  select(X2026_TieCall_userid, X2026_TieCall_email) %>% 
  distinct() %>%
  arrange(X2026_TieCall_userid)



user_check_1 %>% 
  full_join(user_check_2, by = c("X2024_1st_userid" = "X2024_2nd_userid")) %>% 
  full_join(user_check_3, by = c("X2024_1st_userid" = "X2026_1st_userid")) %>% 
  full_join(user_check_4, by = c("X2024_1st_userid" = "X2026_TieCall_userid")) #%>% View()

## Note that some of the users email corresponds to another actual caller 
## e.g. 'xavi.rotllan.puig@gmail.com' strata was made by 'silvia.carboni@ext.ec.europa.eu'
## using Xavi's credentials.
## The rest should be checked with Rene




## Check that the coordinates are consistent ####

valid_all %>% 
  select(location_id, contains("pixel")) %>% #head()
  #View()
  filter(
    !is.na(X2024_1st_pixel_center_x),
    !is.na(X2026_1st_pixel_center_x),
    #round(X2024_1st_pixel_center_x, 5) != round(X2026_1st_pixel_center_x, 5)   # 30
    round(X2024_1st_pixel_center_x, 3) != round(X2026_1st_pixel_center_x, 3)    #  1
  ) %>% #nrow()
  View()   ## There's only one sample with different coordinates (150km apart, mostly due to )
           ## It's location_id = 1955078. I've checked and the error comes from
           ## the original file shared by IIASA

## I've also realised that pixel_center_x/y and sample_x/y are slightly different,
## a few meters (approx 20m)


#valid_all %>% 
#  select(location_id, contains("pixel")) %>%
#  slice({
#    i <- which(location_id == 1955078)
#    (i - 1):(i + 1)
#  })
  

valid_all %>% 
  select(contains("pixel")) %>% #head()
  #View()
  filter(
    is.na(X2024_1st_pixel_center_x),
    is.na(X2026_1st_pixel_center_x)
  ) %>% nrow()   ## 0  If needed, the missing coordinates in 2024_1st could be retrieved from the 
                 ##    2026_1st round
  


apply(valid_all, 2, function(x) sum(is.na(x)))


  
  
  
  
  
  
