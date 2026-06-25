
#-------------------------------------------------------------#
#-------------------------------------------------------------#
#------------    Script to produce the final    --------------#
#------------   GFC validation dataset (2026)   --------------#
#-------------------------------------------------------------#
#-------------------------------------------------------------#






library(tidyverse)



## Setup ####

# GeoWiki data directory
dir_geowiki <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_Second/geowiki_2026/"



valid_all <- read.csv(paste0(dir_geowiki, "AllValidations_2024_2026.csv"))

names(valid_all)



## Check differences along Calls ####

valid_all %>% 
  filter_out(is.na(X2026_TieCall_forest_class)) %>% #nrow() # 3975
  filter(X2024_final_forest_class == X2026_1st_forest_class &
           X2024_final_forest_class != X2026_TieCall_forest_class) %>% #nrow()   # 81 that the Tie Call has changed For/NonFor
  #View()
  select("ID", "Region", "location_id",
         "X2024_final_forest_class", "X2024_final_confidence_level", "X2024_final_type_class", "X2024_final_class_issues", "X2024_final_comment",
         "X2026_1st_forest_class", "X2026_1st_confidence_level", "X2026_1st_type_class", "X2026_1st_class_issues", "X2026_1st_comment",
         "X2026_TieCall_forest_class", "X2026_TieCall_confidence_level", "X2026_TieCall_type_class", "X2026_TieCall_class_issues", "X2026_TieCall_comment") %>% 
  writexl::write_xlsx(paste0(dir_geowiki, "TieCall_diff_ForNonFor.xlsx"))




valid_all %>% 
  filter_out(is.na(X2026_TieCall_forest_class)) %>% #nrow() # 3975
  filter_out(X2024_final_forest_class == X2026_1st_forest_class &
               X2024_final_forest_class != X2026_TieCall_forest_class)  %>%
  filter_out(X2026_TieCall_forest_class == X2024_final_forest_class |
           X2026_TieCall_forest_class == X2026_1st_forest_class) %>% #nrow()   # 2 that the Tie Call has changed For/NonFor from 2026-1st, and have NAs in 2024-Final
  #View()
  select("ID", "Region", "location_id",
         "X2024_final_forest_class", "X2024_final_confidence_level", "X2024_final_type_class", "X2024_final_class_issues", "X2024_final_comment",
         "X2026_1st_forest_class", "X2026_1st_confidence_level", "X2026_1st_type_class", "X2026_1st_class_issues", "X2026_1st_comment",
         "X2026_TieCall_forest_class", "X2026_TieCall_confidence_level", "X2026_TieCall_type_class", "X2026_TieCall_class_issues", "X2026_TieCall_comment") %>% #View()
  writexl::write_xlsx(paste0(dir_geowiki, "NA_2024Final_TieCall_diffForNonFor_20261st.xlsx"))




valid_all %>% 
  filter_out(is.na(X2026_TieCall_forest_class)) %>% #nrow() # 3975
  filter_out(X2024_final_forest_class == X2026_1st_forest_class &
               X2024_final_forest_class != X2026_TieCall_forest_class)  %>%
  filter_out(X2026_TieCall_forest_class == X2024_final_forest_class |
           X2026_TieCall_forest_class == X2026_1st_forest_class) %>% #nrow()   # 2 that the Tie Call has changed For/NonFor from 2026-1st, and have NAs in 2024-Final
  #View()
  select("ID", "Region", "location_id",
         "X2024_final_forest_class", "X2024_final_confidence_level", "X2024_final_type_class", "X2024_final_class_issues", "X2024_final_comment",
         "X2026_1st_forest_class", "X2026_1st_confidence_level", "X2026_1st_type_class", "X2026_1st_class_issues", "X2026_1st_comment",
         "X2026_TieCall_forest_class", "X2026_TieCall_confidence_level", "X2026_TieCall_type_class", "X2026_TieCall_class_issues", "X2026_TieCall_comment") %>% #View()
  writexl::write_xlsx(paste0(dir_geowiki, "NA_2024Final_TieCall_diffForNonFor_20261st.xlsx"))





valid_all %>% 
  filter_out(is.na(X2026_TieCall_forest_class)) %>% #nrow() # 3975
  filter(X2026_TieCall_forest_class == X2024_final_forest_class |
           X2026_TieCall_forest_class == X2026_1st_forest_class) %>% #nrow()   # 3892 that the differences are not in For/Non-For (3892 + 2 + 81 = 3975)
  filter(X2024_final_type_class != X2026_TieCall_type_class &
           X2026_1st_type_class != X2026_TieCall_type_class) %>% #nrow()   # 493 that the difference is in the Type Class
  #View()
  select("ID", "Region", "location_id",
         "X2024_final_forest_class", "X2024_final_confidence_level", "X2024_final_type_class", "X2024_final_class_issues", "X2024_final_comment",
         "X2026_1st_forest_class", "X2026_1st_confidence_level", "X2026_1st_type_class", "X2026_1st_class_issues", "X2026_1st_comment",
         "X2026_TieCall_forest_class", "X2026_TieCall_confidence_level", "X2026_TieCall_type_class", "X2026_TieCall_class_issues", "X2026_TieCall_comment") %>% #View()
  writexl::write_xlsx(paste0(dir_geowiki, "TieCall_diff_TypeClass.xlsx"))


kk$X2024_final_type_class != kk$X2026_TieCall_type_class &
  kk$X2026_1st_type_class != kk$X2026_TieCall_type_class



valid_all %>% 
  filter_out(is.na(X2026_TieCall_forest_class)) %>% #nrow() # 3975
  filter(X2026_TieCall_forest_class == X2024_final_forest_class |
           X2026_TieCall_forest_class == X2026_1st_forest_class) %>% #nrow()   # 3892 that the differences are not in For/Non-For (3892 + 2 + 81 = 3975)
  filter(is.na(X2024_final_type_class != X2026_TieCall_type_class) &
           X2026_1st_type_class != X2026_TieCall_type_class) %>% #nrow()   # 1 that has NA in 2024-Final and the difference is in the Type Class
  #View()
  select("ID", "Region", "location_id",
         "X2024_final_forest_class", "X2024_final_confidence_level", "X2024_final_type_class", "X2024_final_class_issues", "X2024_final_comment",
         "X2026_1st_forest_class", "X2026_1st_confidence_level", "X2026_1st_type_class", "X2026_1st_class_issues", "X2026_1st_comment",
         "X2026_TieCall_forest_class", "X2026_TieCall_confidence_level", "X2026_TieCall_type_class", "X2026_TieCall_class_issues", "X2026_TieCall_comment") %>% #View()
  writexl::write_xlsx(paste0(dir_geowiki, "NA_2024Final_TieCall_diff_TypeClass.xlsx"))





## Criteria for selection ####
## Creating the final GFC Validation dataset 2026 by selecting 'non-problematic' samples after 2024 and 2026: criteria 1 to 3.
## For the 'problematic samples', see criteria 4 and subsequent.

names(valid_all)

### 1) Those samples not included in the 2026 exercise ####
### Then we pick 2024-Final

crit1 <- valid_all %>% 
  filter(is.na(X2026_1st_forest_class)) %>% #nrow() # 7988  (21752 - 7988 = 13764)
  select("ID", "Region", "location_id",
         "X2024_1st_sample_id",   # this is the reference for most of the spatial layers
         "X2024_final_groupid",   # this allows to know from which call they come
         "X2024_final_forest_class", "X2024_final_confidence_level", "X2024_final_type_class", "X2024_final_class_issues", "X2024_final_comment") %>% #head()
  rename_with( ~ str_remove(.x, "^X2024_final_")) #%>% tail()


nrow(crit1)
head(crit1)



### 2) Those samples from the 2026-1stCall not selected for the TieCall  ####
### Same assignment than 2024-Final, and the other criteria for the Tie Call
### We pick 2026-1st call

crit2 <- valid_all %>% 
  filter_out(location_id %in% crit1$location_id) %>% #nrow()  # 13764
  filter(is.na(X2026_TieCall_forest_class)) %>% #nrow() # 9789  (13764 - 3975 = 9789)
  select("ID", "Region", "location_id",
         "X2024_1st_sample_id",   # this is the reference for most of the spatial layers
         "X2026_1st_groupid",   # this allows to know from which call they come
         "X2026_1st_forest_class", "X2026_1st_confidence_level", "X2026_1st_type_class", "X2026_1st_class_issues", "X2026_1st_comment") %>% #head()
  rename_with( ~ str_remove(.x, "^X2026_1st_")) #%>% tail()

nrow(crit2)
head(crit2)




### 3) Validations of the Tie Call ####
## These samples have different For/NonFor in 2024-Final and 2026-1st. Therefore TieCall used for selection


#### 3a) Tie call certifies 2024-Final decision ####
## For/NonFor and Type class coincide with 2024-Final
 

crit3a <- valid_all %>% 
  #filter_out(is.na(X2026_TieCall_forest_class)) %>% #nrow() # 3975
  filter_out(location_id %in% crit1$location_id) %>%
  filter_out(location_id %in% crit2$location_id) %>% #nrow()  # 3975
  filter(
    (X2026_TieCall_forest_class == X2024_final_forest_class &
       X2026_TieCall_type_class == X2024_final_type_class)) %>%    # Tie call certifies 2024-Final decision
  #nrow()  # 1336
  select("ID", "Region", "location_id",
         "X2024_1st_sample_id",   # this is the reference for most of the spatial layers
         "X2024_final_groupid",   # this allows to know from which call they come
         "X2024_final_forest_class", "X2024_final_confidence_level", "X2024_final_type_class", "X2024_final_class_issues", "X2024_final_comment") %>% #head()
  rename_with( ~ str_remove(.x, "^X2024_final_")) #%>% tail()


      
#### 3b) Tie call certifies 2026-1st decision ####
## For/NonFor and Type class coincide with 2026-1st


crit3b <- valid_all %>% 
  #filter_out(is.na(X2026_TieCall_forest_class)) %>% #nrow() # 3975
  filter_out(location_id %in% crit1$location_id) %>%
  filter_out(location_id %in% crit2$location_id) %>% #nrow()  # 3975
  filter(
    (X2026_TieCall_forest_class == X2026_1st_forest_class &
         X2026_TieCall_type_class == X2026_1st_type_class)) %>%     # Tie call certifies 2026-1st decision
  #nrow()  # 2062
  select("ID", "Region", "location_id",
         "X2024_1st_sample_id",   # this is the reference for most of the spatial layers
         "X2026_1st_groupid",   # this allows to know from which call they come
         "X2026_1st_forest_class", "X2026_1st_confidence_level", "X2026_1st_type_class", "X2026_1st_class_issues", "X2026_1st_comment") %>% #head()
  rename_with( ~ str_remove(.x, "^X2026_1st_")) #%>% tail()



1336 + 2062  # 3398
81 + 2 + 493 # 576
3398 + 576 # 3974 There's 1 missing ( 1998834: 2024-Final = NA; 2026-1st = Non-For, no trees; 2026-Tie = Non-For, other wooded )
3398 + (576 + 1) # 3975, OK


#### 3) Merging 3a and 3b ####

crit3 <- rbind(crit3a, crit3b) #%>% 

nrow(crit3) # 3398
head(crit3)

unique(crit3$class_issues)

### 4) Problematic samples ####
## Disagreement between the TieCall and all the previous (2024-Final and 2026-1st)
## Some rules (criteria) need to be established


## Case 1: 2024-Final and 2026-1st agree on For/Non-For, but TieCall disagrees   # 81 that the Tie Call has changed For/NonFor:
## For     For      Non-For   -->  8  Differences in Natural vs planted
## NonFor  NonFor   For       --> 73  Differences in 6 subclasses
##
## 2024-Final has additional weight because it's already the result of a previous multi-round (2024-1st, 2024-2nd, 2024-Final) interpretation process rather than a single interpretation (2026-1st).
## Unless 2024-Final has issues like "no response data", "low resolution" or "cloud cover"; in that case choose 2026-1st (or at least flag it for potential manual review) 




## case 2: 2024-Final and/or 2026-1st and TieCall agree on For/Non-For,              
##         2024-Final, 2026-1st and TieCall disagree on Type Class    # 493 that the difference is in the Type Class
##         The 3 Type Classes are different (no combination of 2 equal)
##
## NonFor  NonFor   NonFor      --> 351 *  
## For     For      For         -->   0
## For     NonFor   For         -->   9 **  
## For     NonFor   NonFor      -->  61 **     
## NonFor  For      For         -->  16 **
## NonFor  For      NonFor      -->  56 **
351 + 9 + 61 + 16 + 56

## ** Although 2024-Final has additional weight, because the TieCall was initiated specifically because the original interpretations disagreed. 
## Its purpose is to provide an independent adjudication. Once accepted its class decision (For/NonFor), 
## it is reasonable to accept its subclass as well, since the TieCall interpreter may have considered both simultaneously.
## Unless it has issues like "no response data", "low resolution" or "cloud cover"; in that case choose 2024-Final/2026-1st 
## (or at least flag it for potential manual review) 

## * 



## case 3: 2024-Final has NA, and 2026-1st and TieCall disagree on For/Non-For   # 2 samples
## case 4: 2024-Final has NA, and 2026-1st and TieCall disagree on Type Class    # 1 sample

81 + 493 + 2 + 1  # 577



 

#













### Merging all criteria ####

crit_all 
rbind(crit1, crit2, crit3) %>% nrow()   # 21175
21752 - (rbind(crit1, crit2, crit3) %>% nrow())   # 577  # problematic samples
21175 + 577  # 21752


crit_all <- rbind(crit1, crit2, crit3) 

crit_all %>% filter(location_id == 1998834)
nrow(crit_all)
length(unique(crit_all$location_id))
#





## Add GAUL information ####









