




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

sort(unique(valid_2024_final$X2024_final_groupid))
apply(valid_2024_final, 2, function(x) sum(is.na(x)))



## 2026 first round ####

data_2026_2024_clean <- read.csv(paste0(dir_geowiki, date_part_1, "_comparison_2026_2024_clean.csv"))


data_2026_2024_clean[1:2, ]


valid_2026_1st <- data_2026_2024_clean %>% 
  select(location_id, userid, email, groupid, sample_id,  
         pixel_center_x.x, pixel_center_y.x,
         starts_with("GFC."), comment) %>%    # pull(groupid) %>% unique() %>% sort()
  mutate(type_class = coalesce(GFC.validation...forest.type, GFC.validation...other.land.use.type)) %>% #head()
  select(-GFC.validation...forest.type, -GFC.validation...other.land.use.type) %>% 
  rename(
    "X2026_1st_userid" = "userid",
    "X2026_1st_email" = "email",
    "X2026_1st_groupid" = "groupid",
    "X2026_1st_pixel_center_x" = "pixel_center_x.x",
    "X2026_1st_pixel_center_y" = "pixel_center_y.x",
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
apply(valid_2026_1st, 2, function(x) sum(is.na(x))) # 13 that have missing values in type_class, but that are reingested in 2026 2nd round

#sum(valid_2026_1st %>% filter(is.na(X2026_1st_type_class)) %>% pull(location_id) %in%
#  scenario2_plusMissing$location_id)  # 13 that have missing values in type_class, but that are reingested in 2026 2nd round

sum(!valid_2026_1st$location_id %in% valid_2024_final$location_id)  # 11 which couldn't be assigned in the 2024 final round
sum(!valid_2026_1st$location_id %in% valid_2024_1st$location_id)  # 0





## All rounds together ####

valid_2024_1st %>% head()
valid_2024_2nd %>% head()
valid_2024_1st %>% nrow()  # 21752
valid_2024_2nd %>% nrow()  #  4000
valid_2026_1st %>% nrow()  #  13764
unique(valid_2024_1st$location_id) %>% length()
unique(valid_2024_2nd$location_id) %>% length()
unique(valid_2026_1st$location_id) %>% length()


valid_all <- valid_2024_1st %>% 
  full_join(valid_2024_2nd, by = "location_id") %>%
  relocate(location_id) %>% 
  full_join(valid_2024_final, by = "location_id") %>% 
  full_join(valid_2026_1st, by = "location_id")


valid_all %>% head()
valid_all %>% nrow()  # 21752
apply(valid_all, 2, function(x) sum(is.na(x)))


## Save data set
write.csv(valid_all, 
          paste0(dir_geowiki, "../AllValidations_2024_2026.csv"), 
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



user_check_1 %>% 
  full_join(user_check_2, by = c("X2024_1st_userid" = "X2024_2nd_userid")) %>% 
  full_join(user_check_3, by = c("X2024_1st_userid" = "X2026_1st_userid"))

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
    #round(X2024_1st_pixel_center_x, 5) != round(X2026_1st_pixel_center_x, 5)
    round(X2024_1st_pixel_center_x, 3) != round(X2026_1st_pixel_center_x, 3)
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


  
  
  
  
  
  
