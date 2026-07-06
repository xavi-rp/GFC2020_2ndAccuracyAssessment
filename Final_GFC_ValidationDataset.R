
#-------------------------------------------------------------#
#-------------------------------------------------------------#
#------------    Script to produce the final    --------------#
#------------   GFC validation dataset (2026)   --------------#
#-------------------------------------------------------------#
#-------------------------------------------------------------#






library(tidyverse)
library(ggplot2)
library(openxlsx)





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


nrow(crit1)  # 7988
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

nrow(crit2)  # 9789
head(crit2)




### 3) Validations of the Tie Call ####
## These samples have different For/NonFor in 2024-Final and 2026-1st. Therefore, TieCall used for selection


#### 3a) Tie call certifies 2024-Final decision ####
## Both For/NonFor and Type class coincide with 2024-Final
 
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


nrow(crit3a)   # 1336


      
#### 3b) Tie call certifies 2026-1st decision ####
## Both For/NonFor and Type class coincide with 2026-1st


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


nrow(crit3b)   # 2062

1336 + 2062  # 3398
81 + 2 + 494 # 577
3398 + 576 # 3974 There's 1 missing ( 1998834: 2024-Final = NA; 2026-1st = Non-For, no trees; 2026-Tie = Non-For, other wooded )
3398 + (576 + 1) # 3975, OK


#### 3) Merging 3a and 3b ####

crit3 <- rbind(crit3a, crit3b) #%>% 

nrow(crit3) # 3398
head(crit3)




### 4) Problematic samples ####
## Disagreement between the TieCall and all the previous (2024-Final and 2026-1st)      # 577 samples
## Some rules (criteria) need to be established


## Case 1: 2024-Final and 2026-1st agree on For/Non-For, but TieCall disagrees     # 81 that the Tie Call has changed For/NonFor:    ¡¡¡ check if territorial bias !!!
## For     For      Non-For   -->  8  Differences in Natural vs planted                                           ¡¡¡ check the images and make a "block" decision if we go for 2024-1st always 
## NonFor  NonFor   For       --> 73  Differences in 6 subclasses (no trees/shrubs, etc)                              or for Tie Call always !!!
##
## TieCall is not used to determine the parent class (For/NonFor) because it exists only 
## to arbitrate disagreements between the first two interpretations.
##
## 2024-Final subclass is retained. 
## 2024-Final has additional weight because it's already the result of a previous multi-round (2024-1st, 2024-2nd, 2024-Final) 
## interpretation process rather than a single interpretation (2026-1st).
## Unless 2024-Final has issues like "no response data", "low resolution" or "cloud cover"; in that case 2026-1st 
## should be preferred (or at least flag it for potential manual review). If both interpretations are affected 
## by these issues, the sample should be flagged for manual review.


valid_all %>% 
  filter_out(location_id %in% crit1$location_id) %>%
  filter_out(location_id %in% crit2$location_id) %>%
  filter_out(location_id %in% crit3$location_id) %>% #nrow()
  filter(X2024_final_forest_class == X2026_1st_forest_class &
           X2024_final_forest_class != X2026_TieCall_forest_class) %>% #nrow()
  pull(X2024_final_class_issues) %>% 
  table() %>% {.[names(.) %in% c("no response data", "low resolution", "cloud cover")]}

#  low resolution   no response data 
#         2                2               #  4 samples flagged and checked if 2026-1st also report these


case1_manualRevision <- valid_all %>% 
  filter_out(location_id %in% crit1$location_id) %>%
  filter_out(location_id %in% crit2$location_id) %>%
  filter_out(location_id %in% crit3$location_id) %>% #nrow()
  filter(X2024_final_forest_class == X2026_1st_forest_class &
           X2024_final_forest_class != X2026_TieCall_forest_class) %>% 
  filter(X2024_final_class_issues %in% c("no response data", "low resolution", "cloud cover")) 

case1_manualRevision %>%  #nrow()
  pull(X2026_1st_class_issues) %>% 
  table() %>% {.[names(.) %in% c("no response data", "low resolution", "cloud cover")]}


#   low resolution    no response data 
#         1                1               # 2 flagged for manual revision


case1_manualRevision %>%  
  filter(X2026_1st_class_issues %in% c("no response data", "low resolution", "cloud cover")) %>% 
  select("ID", "Region", "location_id",
         "X2024_1st_sample_id",   # this is the reference for most of the spatial layers
         "X2026_1st_groupid",   # this allows to know from which call they come
         "X2024_final_forest_class", "X2024_final_confidence_level", "X2024_final_type_class", "X2024_final_class_issues", "X2024_final_comment",
         "X2026_1st_forest_class", "X2026_1st_confidence_level", "X2026_1st_type_class", "X2026_1st_class_issues", "X2026_1st_comment") #%>% View()

#




## Case 2: 2024-Final and/or 2026-1st and TieCall agree on For/Non-For,              
##         but they disagree on Type Class.                             # 493 that the difference is in the Type Class
##         The 3 Type Classes are different (no combination of 2 equal)
##
## NonFor  NonFor   NonFor      --> 351 *  
## For     For      For         -->   0 *
##
## For     NonFor   For         -->   9 **  
## For     NonFor   NonFor      -->  61 **     
## NonFor  For      For         -->  16 **
## NonFor  For      NonFor      -->  56 **

351 + 9 + 61 + 16 + 56 
9 + 61 + 16 + 56   # 142

## * Same as Case1: use 2024-Final as it has additional weight
##
## ** The TieCall is used determine the final For/NonFor class, and its corresponding subclass is also retained. Rationale:
## The TieCall was initiated specifically because the original interpretations disagreed, and its purpose is to provide an 
## independent adjudication. Once accepted its class decision (For/NonFor), although 2024-Final has additional weight, it is reasonable
## (and methodologically consistent) to accept TieCall subclass as well, since its interpreter may have considered the previous 
## class/subclass assignment simultaneously.
## Unless it has issues like "no response data", "low resolution" or "cloud cover"; in that case 2024-Final/2026-1st is preferred
## (or at least flag it for potential manual review) 


case2_manualRevision <- valid_all %>% 
  filter_out(location_id %in% crit1$location_id) %>%
  filter_out(location_id %in% crit2$location_id) %>%
  filter_out(location_id %in% crit3$location_id) %>% #nrow()
  filter(X2024_final_type_class != X2026_TieCall_type_class &
           X2026_1st_type_class != X2026_TieCall_type_class) %>% #nrow()
  filter(X2024_final_forest_class == X2026_1st_forest_class &
           X2026_1st_forest_class == X2026_TieCall_forest_class) #%>%  #nrow()  # 351

case2_manualRevision %>% 
  pull(X2024_final_class_issues) %>% 
  table() %>% {.[names(.) %in% c("no response data", "low resolution", "cloud cover")]}


#   low resolution    no response data 
#        1                   8 


case2_manualRevision <- case2_manualRevision %>%  
  filter(X2024_final_class_issues %in% c("no response data", "low resolution", "cloud cover")) #%>%  #nrow()

case2_manualRevision %>% 
  pull(X2026_1st_class_issues) %>% 
  table() %>% {.[names(.) %in% c("no response data", "low resolution", "cloud cover")]}

#   low resolution    no response data 
#          1                1                 # 2 flagged for manual revision

case2_manualRevision %>% 
  filter(X2026_1st_class_issues %in% c("no response data", "low resolution", "cloud cover")) %>% 
  select("ID", "Region", "location_id",
         "X2024_1st_sample_id",   # this is the reference for most of the spatial layers
         "X2026_1st_groupid",   # this allows to know from which call they come
         "X2024_final_forest_class", "X2024_final_confidence_level", "X2024_final_type_class", "X2024_final_class_issues", "X2024_final_comment",
         "X2026_1st_forest_class", "X2026_1st_confidence_level", "X2026_1st_type_class", "X2026_1st_class_issues", "X2026_1st_comment") #%>% View()



#

#     ¡¡¡ check all the 24 images and see if the new 2026 validations make sense or are too optimistic !!!

## case 3: 2024-Final has NA (missing information), and 2026-1st and TieCall disagree on For/Non-For   # 2 samples
## These should be flagged for potential manual review


## case 4: 2024-Final has NA (missing information), and 2026-1st and TieCall disagree on Type Class    # 1 sample
## These should be flagged for potential manual review


81 + 493 + 2 + 1  # 577



## In general terms:
## 
## The selection process is hierarchical:
##    1) First determine the Forest/Non-Forest class.
##    2) Once the parent class has been established, determine the subclass within that class.
##       The rationale is that the subclass is only meaningful once the parent class has been decided.
## 
## In other words, the interpretation that determines the parent class (Forest/Non-Forest) should also determine the subclass, 
## unless its reliability is compromised by observation-quality issues:
##    - If the parent class comes from the 2024-Final + 2026-1st consensus, the TieCall does not influence the subclass.
##    - If the parent class comes from the TieCall, the TieCall also determines the subclass.
##    - The only exceptions are when the deciding interpretation is based on inadequate imagery ("no response data", 
##      "low resolution", or "cloud cover"), in which case the sample should either revert to the best available alternative
##      or be flagged for manual review.
 

#













### Merging all criteria ####

rbind(crit1, crit2, crit3) %>% nrow()   # 21175
21752 - (rbind(crit1, crit2, crit3) %>% nrow())   # 577  # problematic samples
21175 + 577  # 21752


crit_all <- rbind(crit1, crit2, crit3) 

crit_all %>% filter(location_id == 1998834)
nrow(crit_all)
length(unique(crit_all$location_id))
#







## TieCall vs 2024-Final as the Final 2026 assignment ####

## This section is to assess if Tie Call can be used as the Final option when it disagrees with 2024-Final and 2026-1st.
##
## It's the Case 1 above: 2024-Final and 2026-1st agree on For/Non-For, but TieCall disagrees     # 81 that the Tie Call has changed For/NonFor:
## For     For      Non-For   -->  8  Differences in Natural vs planted                                          
## NonFor  NonFor   For       --> 73  Differences in 6 subclasses (no trees/shrubs, etc)                             



### Is there a territorial/tie caller bias? ####

valid_all_subset <- valid_all %>%
  filter_out(is.na(X2026_TieCall_groupid)) %>% #nrow()  # 3975
  filter(
    X2024_final_forest_class == X2026_1st_forest_class) #%>%  nrow()  # 3088

nrow(valid_all_subset)   # 3088    For  For  ??, or NonFor  NonFor ??
names(valid_all_subset)   # 3088    For  For  ??, or NonFor  NonFor ??
table(valid_all_subset$X2026_TieCall_forest_class) 
#     Forest Non-forest 
#        428       2660 


valid_all_subset <- valid_all %>%
  filter_out(is.na(X2026_TieCall_groupid)) %>% #nrow()  # 3975
  filter(X2024_final_forest_class == X2026_1st_forest_class) %>%
  select("ID", "Region", "location_id",
         "X2024_1st_sample_id",   # this is the reference for most of the spatial layers
         "X2026_1st_groupid",   # this allows to know from which call they come
         "X2026_TieCall_email", # this allows to know who was the Tie Caller
         "X2024_final_forest_class", "X2024_final_confidence_level", "X2024_final_type_class", "X2024_final_class_issues", "X2024_final_comment",
         "X2026_1st_forest_class", "X2026_1st_confidence_level", "X2026_1st_type_class", "X2026_1st_class_issues", "X2026_1st_comment",
         "X2026_TieCall_forest_class", "X2026_TieCall_confidence_level", "X2026_TieCall_type_class", "X2026_TieCall_class_issues", "X2026_TieCall_comment") %>% 
  mutate(TieCall_changes_class = X2026_TieCall_forest_class != X2024_final_forest_class) 
  

names(valid_all_subset)
nrow(valid_all_subset)  # 3088:     For  For  ??, or NonFor  NonFor ??

valid_all %>%
  filter_out(is.na(X2026_TieCall_groupid)) %>% #nrow()  # 3975
  filter(X2024_final_forest_class != X2026_1st_forest_class) %>% nrow()  # 876:     For    NonFor   ??,  or   NonFor    For   ??
  #filter(is.na(X2024_final_forest_class)) %>% nrow()                     # 11:      NA    NonFor   ??,  or       NA    For   ??

table(valid_all_subset$TieCall_changes_class)
# FALSE  TRUE(TieCall disagrees) 
#  3007    81 


### How often does the TieCall contradict a consensus?  ####

valid_all_subset %>%
  summarise(
    n_total = n(),
    n_changes = sum(TieCall_changes_class),
    pct_changes = 100 * mean(TieCall_changes_class)
  )

#   n_total   n_changes   pct_changes
#     3088        81       2.62          # 97.38% of the time, the TieCall confirmed the previous consensus For/NonFor.


# Does the TieCall tend to change Forest into Non-Forest more often than the opposite?
valid_all_subset %>%
  filter(TieCall_changes_class) %>%
  count(X2024_final_forest_class, X2026_TieCall_forest_class)

#   X2024_final_forest_class   X2026-1st   X2026_TieCall_forest_class    n
#                   Forest        Forest                   Non-forest    8
#               Non-forest    Non-forest                   Forest       73      # The direction of the changes is highly asymmetric



# contradictory cases:

TieCall_changes <- valid_all_subset %>%
  filter(TieCall_changes_class)


### Are those 81 cases concentrated in samples that were already considered difficult? ####

compare_groups <- function(data, var) {
  tab <- data %>%
    count(TieCall_changes_class, {{ var }}) %>%
    group_by(TieCall_changes_class) %>%
    mutate(
      pct = round(100 * n / sum(n), 1)
    ) %>%
    ungroup()
  
  res <- full_join(
    tab %>%
      filter(!TieCall_changes_class) %>%
      select(
        value = {{ var }},
        n_nochange = n,
        pct_nochange = pct
      ),
    tab %>%
      filter(TieCall_changes_class) %>%
      select(
        value = {{ var }},
        n_change = n,
        pct_change = pct
      ),
    by = "value"
  )
  bind_rows(
    res,
    tibble(
      value = "Total",
      n_nochange = sum(res$n_nochange, na.rm = TRUE),
      pct_nochange = round(sum(res$pct_nochange, na.rm = TRUE), 1),
      n_change = sum(res$n_change, na.rm = TRUE),
      pct_change = round(sum(res$pct_change, na.rm = TRUE), 1)
    )
  ) %>%  rename_with(~ rlang::as_name(rlang::ensym(var)), .cols = value)
}


compare_groups(valid_all_subset, X2024_final_class_issues)
compare_groups(valid_all_subset, X2024_final_class_issues)
compare_groups(valid_all_subset, X2026_TieCall_class_issues)

comp_groups <- compare_groups(valid_all_subset, X2024_final_class_issues) %>% 
  full_join(compare_groups(valid_all_subset, X2026_1st_class_issues), by = c("X2024_final_class_issues" = "X2026_1st_class_issues"), keep = TRUE) %>% 
  full_join(compare_groups(valid_all_subset, X2026_TieCall_class_issues), by = c("X2024_final_class_issues" = "X2026_TieCall_class_issues"), keep = TRUE)

comp_groups %>% select(matches("^(X|pct_c)"))
comp_groups %>% select(matches("^(X|pct_noc|pct_c)"))

## Most of the contradictory cases are not concentrated in the already considered difficult
## ('low resolution', 'no response data' or 'cloud cover'), but in 'Multiple land uses' and 
## 'Open treed land use'. Therefore, the contradictory assignments are most likely due to the 
## difficulty of the observer





### Fixing errors in the TieCaller email (who actually performed the validation) ####
valid_all_subset %>% 
  select(Region, X2026_TieCall_email) %>% 
  unique()
table()
 
# "Northern, Central and Eastern Europe" --> Silvia (using ReneB credentials except for the 3 revised samples)

valid_all_subset <- valid_all_subset %>%
  mutate(
    X2026_TieCall_email = case_when(
      Region == "Northern, Central and Eastern Europe" &
        X2026_TieCall_email == "rene.beuchle@ec.europa.eu" ~
          "silvia.carboni@ext.ec.europa.eu",
      TRUE ~ X2026_TieCall_email
    )
  )  

valid_all_subset %>% 
  select(Region, X2026_TieCall_email) %>% 
  unique()

###



### The differences are due to the tie callers or to the regions they worked in? ####

#### To the Tie Callers? ####

compare_groups(valid_all_subset, X2026_TieCall_email)
#value                                n_nochange pct_nochange n_change pct_change
#<chr>                                     <int>        <dbl>    <int>      <dbl>
#1 Joao.Carreiras@ext.ec.europa.eu             644         21.4       21       25.9
#2 Xavier.ROTLLAN-PUIG@ext.ec.europa.eu        703         23.4       13       16  
#3 frederic.achard@ec.europa.eu                337         11.2       15       18.5
#4 rene.beuchle@ec.europa.eu                   207          6.9        8        9.9
#5 rene.colditz@ec.europa.eu                   269          8.9       17       21  
#6 silvia.carboni@ext.ec.europa.eu             847         28.2        7        8.6
#7 Total                                      3007        100         81       99.9



## Some of these users produce more changes, in average, than the others? And is this statistically significant?

tab <- table(
  valid_all_subset$X2026_TieCall_email,
  valid_all_subset$TieCall_changes_class)

tab

chisq.test(tab)
# 	Pearson's Chi-squared test
#
#data:  tab
#X-squared = 30.508, df = 5, p-value = 1.171e-05  
# The result is significant (the null hypothesis is rejected). 
# There is statistical evidence that the probability of changing the agreed Forest/Non-Forest class differs among tie callers.



chisq.test(tab)$expected

#                                           FALSE      TRUE
#    frederic.achard@ec.europa.eu         342.7668  9.233161
#    Joao.Carreiras@ext.ec.europa.eu      647.5567 17.443329
#    rene.beuchle@ec.europa.eu            209.3604  5.639573
#    rene.colditz@ec.europa.eu            278.4981  7.501943
#    silvia.carboni@ext.ec.europa.eu      831.5991 22.400907
#    Xavier.ROTLLAN-PUIG@ext.ec.europa.eu 697.2189 18.781088

#All expected counts are above 5 (the smallest is 5.64), so the assumptions of the Pearson chi-square test are satisfied.


chisq.test(tab)$stdres

#                                            FALSE       TRUE
#  frederic.achard@ec.europa.eu         -2.0432203  2.0432203  unusually high/low (Slightly more changes than expected)
#  Joao.Carreiras@ext.ec.europa.eu      -0.9742331  0.9742331
#  rene.beuchle@ec.europa.eu            -1.0442641  1.0442641
#  rene.colditz@ec.europa.eu            -3.6891338  3.6891338  very strong deviation (Much more changes than expected)
#  silvia.carboni@ext.ec.europa.eu       3.8768811 -3.8768811  very strong deviation (Much fewer changes than expected)
#  Xavier.ROTLLAN-PUIG@ext.ec.europa.eu  1.5424193 -1.5424193

#As a rule of thumb:
## |Residual| < 2 → consistent with chance (as expected)
## |Residual| > 2 → unusually high/low
## |Residual| > 3 → very strong deviation



valid_all_subset %>%
  group_by(X2026_TieCall_email) %>%
  summarise(
    n = n(),
    changes = sum(TieCall_changes_class),
    pct = round(100 * mean(TieCall_changes_class), 2)
  ) %>%
  arrange(desc(pct))

#   X2026_TieCall_email                     n changes   pct
#1 rene.colditz@ec.europa.eu              286      17  5.94
#2 frederic.achard@ec.europa.eu           352      15  4.26
#3 rene.beuchle@ec.europa.eu              215       8  3.72
#4 Joao.Carreiras@ext.ec.europa.eu        665      21  3.16
#5 Xavier.ROTLLAN-PUIG@ext.ec.europa.eu   716      13  1.82
#6 silvia.carboni@ext.ec.europa.eu        854       7  0.82


# As the Tie Caller was associated to particular regions, the variables Tie caller and Region are confounded.
# Each tie caller worked in one (or a fixed set of) regions, and each region was assigned to a single tie caller.
# Then, the variables Tie caller and Region are confounded, and therefore, 
# the differences can't be explained only by the different behavior of the Tie Caller



#### By region ####
## Another approach: After accounting for Region, is there still an effect of the tie caller?

mod <- glm(
  TieCall_changes_class ~ Region + X2026_TieCall_email,
  data = valid_all_subset,
  family = binomial
)

anova(mod)

# Analysis of Deviance Table
# 
# Model: binomial, link: logit
# 
# Response: TieCall_changes_class
# 
# Terms added sequentially (first to last)
# 
# 
# Df Deviance Resid. Df Resid. Dev Pr(>Chi)    
# NULL                                 3087     749.67             
# Region               8   45.839      3079     703.83 2.55e-07 ***
#   X2026_TieCall_email  0    0.000      3079     703.83             
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


## X2026_TieCall_email has 0 DF and 0 Deviance, therefore, Region and Tie Caller are perfectly confounded (no independent 
## information in the data to separate the two variables).
## Therefore, the effect of the tie caller cannot be estimated separately from the effect of Region because the design matrix is rank-deficient.


summary(mod)

# Same than before: perfect collinearity (or aliasing), and R removes the redundant variable automatically, which is why all the tie-caller coefficients are NA.
# Australia is the reference level


## Conclusion:
## Since all tie callers were experienced and trained using the same protocol, and because tie caller and region are completely 
## confounded, the observed heterogeneity is more plausibly explained by regional differences in the difficulty of image interpretation 
## than by systematic differences among interpreters.

## Or:
## The TieCall confirmed the existing consensus (For-For-For or NonFor-NonFor-NonFor) in 97.4% of the cases. Although the rate of
## contradictory classifications varied significantly among regions, the assignment of tie callers to regions was
## completely confounded, preventing a formal separation of interpreter and regional effects. Given that all tie callers were experienced  
## interpreters trained under a common protocol, the observed regional differences are more likely to reflect varying levels 
## of classification difficulty across landscapes than systematic differences in interpreter behaviour.


region_summary <- valid_all_subset %>%
  group_by(Region) %>%
  summarise(
    n = n(),
    changes = sum(TieCall_changes_class)
  ) %>%
  rowwise() %>%
  mutate(
    rate = 100 * changes / n,
    lower = 100 * prop.test(changes, n)$conf.int[1],
    upper = 100 * prop.test(changes, n)$conf.int[2]
  ) %>%
  ungroup()

region_summary
round(range(region_summary$rate), 2)  # The contradiction rate is generally low (0.72 to 7.94)
100 - round(range(region_summary$rate), 2)  # The agreement rate is high (99.28 to 92.06)

sum(region_summary$n)  # 3088

region_summary %>% 
  arrange(., rate)

# Region                                   n changes    rate  lower  upper
# Northern, Central and Eastern Europe   695       5   0.719  0.265   1.77
# Australia                              653       8   1.23   0.571   2.50
# Eastern Asia                           159       2   1.26   0.218   4.94
# Northern Africa                        395       7   1.77   0.779   3.78
# South and Southeast Asia               215       8   3.72   1.74    7.47
# Eastern South America                  352      15   4.26   2.49    7.08
# Southern Africa                        270      14   5.19   2.97    8.74
# Mediterranean Europe                   286      17   5.94   3.61    9.52
# New Zealand and Pacific Islands         63       5   7.94   2.96   18.3 




## There is clear regional variability. This is consistent with the logistic regression above, 
## where Region was highly significant.

## Not all differences are equally reliable:
## | Region                        | Rate | 95% CI    |
## | ----------------------------- | ---- | --------- |
## | New Zealand & Pacific Islands | 7.9% | 3.0–18.3% |
## | Mediterranean Europe          | 5.9% | 3.6–9.5%  |
  
## New Zealand and Pacific Islands had the highest estimated disagreement rate, although the estimate is 
## imprecise because of the relatively small sample size (n=63).

## 'Northern, Central and Eastern Europe' has the largest sample size (695) and the lowest disagreement rate (0.72%).
## TieCall almost never overturned the previous consensus there.

## The likelihood of overturning a previous consensus varies among regions, suggesting that the Forest/Non-Forest
## distinction is more challenging in some landscapes than in others.


## Final conclusion:
## The TieCall contradicted the consensus between the 2024-Final and 2026-1st interpretations in only 81 of 3,088 
## cases (2.6%). However, the frequency of such contradictions varied significantly among regions (logistic regression, 
## p < 0.001). Estimated contradiction rates ranged from 0.7% in 'Northern, Central and Eastern Europe' to approximately 
## 6–8% in 'Mediterranean Europe', 'Southern Africa' and 'New Zealand/Pacific Islands'. Because tie callers were assigned to 
## specific regions, regional and interpreter effects could not be separated. Given the standardized interpretation 
## protocol and the high overall agreement between interpretation rounds, these regional differences are more plausibly 
## explained by varying landscape complexity and Forest/Non-Forest ambiguity than by systematic differences among 
## interpreters.

## The probability of the TieCall contradicting the consensus differed significantly among regions 
## (binomial GLM, likelihood ratio test, χ² = 45.84, df = 8, p < 0.001).


# Figure 1. Question: Does the TieCall contradict the previous consensus equally often in all regions?

region_summary2 <- region_summary %>%
  mutate(
    Region = paste0(Region, " (n=", n, ")")
  )

p <- ggplot(
  region_summary2,
  aes(
    x = reorder(Region, rate),
    y = rate
  )
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    width = 0.2
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Tie Call contradiction rate (%)"
  ) +
  theme_bw(base_size = 12)

ggsave(filename = paste0(dir_geowiki, "TieCall_contradiction_rate_by_region.png"),
       plot = p, width = 9, height = 6, dpi = 300, bg = "white")

## Caption:
## Figure 1. Percentage of samples for which the TieCall assigned a different Forest/Non-Forest class than the consensus reached by the 2024-Final
## and 2026-1st interpretations, by region. Points represent the estimated contradiction rate and horizontal error bars indicate the 95% 
## confidence intervals. Although the overall contradiction rate was low (81 of 3,088 samples; 2.6%), the frequency of contradictory 
## classifications varied significantly among regions.


#

### What kind of disagreement occur? ####

## Are the contradictory cases associated with specific land-cover situations (secondary class) that are intrinsically difficult to classify? 
contradictory_typeClass <- TieCall_changes %>%
  count(Region, X2026_TieCall_type_class) %>%
  group_by(Region) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(Region, desc(pct)) %>% data.frame() #%>% #pull(n) %>% sum() # 81

contradictory_typeClass

reg2check <- unique(contradictory_typeClass$Region)


## And, are issues overrepresented?

contradictory_issues <- TieCall_changes %>%
  count(Region, X2026_TieCall_class_issues) %>%
  group_by(Region) %>%
  mutate(pct = round(100 * n / sum(n), 1))  %>% 
  arrange(Region, desc(pct)) %>% data.frame()

contradictory_issues


disagrr_region <- function(r){
  print(TieCall_changes %>% filter(Region == reg2check[r]) %>% select(Region, X2024_final_forest_class, X2026_1st_forest_class, X2026_TieCall_forest_class))
  cat("\n")
  print(contradictory_typeClass %>% filter(Region == reg2check[r]))
  cat("\n")
  print(contradictory_issues %>% filter(Region == reg2check[r]))
}

disagrr_region_2024 <- function(r){
  
  contradictory_typeClass_2024 <- TieCall_changes %>%
    count(Region, X2024_final_type_class) %>%
    group_by(Region) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    arrange(Region, desc(pct)) %>% data.frame() #%>% #pull(n) %>% sum() # 81
  
  contradictory_issues_2024 <- TieCall_changes %>%
    count(Region, X2024_final_class_issues) %>%
    group_by(Region) %>%
    mutate(pct = round(100 * n / sum(n), 1))  %>% 
    arrange(Region, desc(pct)) %>% data.frame()
  
  print(TieCall_changes %>% filter(Region == reg2check[r]) %>% select(Region, X2024_final_forest_class, X2026_1st_forest_class, X2026_TieCall_forest_class))
  cat("\n")
  print(contradictory_typeClass_2024 %>% filter(Region == reg2check[r]))
  cat("\n")
  print(contradictory_issues_2024 %>% filter(Region == reg2check[r]))
  
}

disagrr_region_2026_1st <- function(r){
  
  contradictory_typeClass_2026_1st <- TieCall_changes %>%
    count(Region, X2026_1st_type_class) %>%
    group_by(Region) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    arrange(Region, desc(pct)) %>% data.frame() #%>% #pull(n) %>% sum() # 81
  
  contradictory_issues_2026_1st <- TieCall_changes %>%
    count(Region, X2026_1st_class_issues) %>%
    group_by(Region) %>%
    mutate(pct = round(100 * n / sum(n), 1))  %>% 
    arrange(Region, desc(pct)) %>% data.frame()
  
  print(TieCall_changes %>% filter(Region == reg2check[r]) %>% select(Region, X2024_final_forest_class, X2026_1st_forest_class, X2026_TieCall_forest_class))
  cat("\n")
  print(contradictory_typeClass_2026_1st %>% filter(Region == reg2check[r]))
  cat("\n")
  print(contradictory_issues_2026_1st %>% filter(Region == reg2check[r]))
  
}



r <- 1  # Australia
disagrr_region(r=r)
## The disagreements in Australia are not random. They all correspond to the same interpretation problem: 
## Non-Forest --> Forest (Naturally regenerating forest), with mostly 'no issues'. If we check Type class and issues in 2024 and 2026-1st:
disagrr_region_2024(r=r)
## They are mostly classified as 'other wooded land' (n=6).
## And for 2026-1st:
disagrr_region_2026_1st(r=r)
## They are 'trees outside forest' (n=5) and 'trees inside forest' (n=3)


r <- 2  # Eastern Asia
disagrr_region(r=r)
## The disagreements in Eastern Asia are only 2. They all correspond to the same interpretation problem: 
## Forest --> Non-Forest, with 'no issues'. If we check Type class and issues in 2024 and 2026-1st:
disagrr_region_2024(r=r)
## 
## And for 2026-1st:
disagrr_region_2026_1st(r=r)


r <- 3  # Eastern South America (n=15)
disagrr_region(r=r)
## The disagreements in Eastern South America are 15. They all correspond to the same interpretation problem: 
## Non-Forest --> Forest, with 40% 'forest to be regrown' (only 1 case 'no issues'). If we check Type class and issues in 2024 and 2026-1st:
disagrr_region_2024(r=r)
## 40% (other wooded land), 
## And for 2026-1st:
disagrr_region_2026_1st(r=r)
## 'trees inside forest', 60%; 'no issues' only 20.0%
#


r <- 4  # Mediterranean Europe 
disagrr_region(r=r)
## The disagreements in Mediterranean Europe are 17. They all correspond to the same interpretation problem: 
## Non-Forest --> Forest, but 1, with only 2 case 'no issues' (11.8%). If we check Type class and issues in 2024 and 2026-1st:
disagrr_region_2024(r=r)
## trees outside forest 35.3%, trees inside forest 23.5%; and only 4 no issues (23.5%), Multiple land uses 41.2%, Open treed land use 29.4%
## And for 2026-1st:
disagrr_region_2026_1st(r=r)
## trees outside forest (7) 41.2%, trees inside forest (5) 29.4%, other wooded land (3) 17.6%; 'no issues' 41.2%
#



## If we caracterise the 81 cases together:

TieCall_changes %>%
  count(
    X2024_final_type_class,
    X2026_TieCall_type_class,
    sort = TRUE
  )
#                     From        ---->        To
#           X2024_final_type_class        X2026_TieCall_type_class   n
#                other wooded land   Naturally regenerating forest  30
#             trees outside forest   Naturally regenerating forest  15
#       no trees or shrubs present   Naturally regenerating forest  10
#              trees inside forest   Naturally regenerating forest   7
#       no trees or shrubs present    Planted or plantation forest   3
#    Naturally regenerating forest               other wooded land   2
#     Planted or plantation forest      trees for agricultural use   2
#     Planted or plantation forest             trees inside forest   2
#                other wooded land    Planted or plantation forest   2
#             trees in urban areas   Naturally regenerating forest   2
#             trees outside forest    Planted or plantation forest   2
#    Naturally regenerating forest             trees inside forest   1
#    Naturally regenerating forest            trees outside forest   1
#       trees for agricultural use   Naturally regenerating forest   1
#              trees inside forest    Planted or plantation forest   1

TieCall_changes %>%   # Same, but from 2026-1st to the TieCall
  count(
    X2026_1st_type_class,
    X2026_TieCall_type_class,
    sort = TRUE)




fig2_data <- TieCall_changes %>%
  filter(X2024_final_forest_class == "Non-forest") %>%
  count(
    X2024_final_type_class,
    sort = TRUE
  ) %>% #pull(n) %>% sum()  # 73, Non-For --> For
  mutate(
    pct = round(100 * n / sum(n), 1)
  )

fig2_data

#       X2024_final_type_class   n   pct    --> went to forest
#            other wooded land  32   43.8
#         trees outside forest  17   23.3
#   no trees or shrubs present  13   17.8   --> these ones are more difficult to interpret; the rest is quite clear that went from 'treed' 
#          trees inside forest   8   11.0       land uses (or at least wooded) to forests
#         trees in urban areas   2    2.7
#   trees for agricultural use   1    1.4

TieCall_changes %>%
  filter(X2026_TieCall_forest_class == "Forest") %>%
  count(
    X2026_TieCall_type_class,
    sort = TRUE
  )

#        X2026_TieCall_type_class   n  pct  
#   Naturally regenerating forest  65   89   --> Most of the changes went to Naturally regenerating forest
#    Planted or plantation forest   8   11

#


p2 <- ggplot(
  fig2_data,
  aes(
    x = reorder(X2024_final_type_class, n),
    y = n
  )
) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = n),
    hjust = -0.2,
    size = 4
  ) +
  coord_flip() +
  expand_limits(y = max(fig2_data$n) * 1.10) +
  labs(
    x = NULL,
    y = "Number of samples"
  ) +
  theme_bw(base_size = 12)

p2

ggsave(filename = paste0(dir_geowiki, "TieCall_NonForest_to_Forest_subclasses.png"), 
  plot = p2, width = 8, height = 5, dpi = 300, bg = "white")
## Caption:
## Figure 2. Distribution of the original Non-Forest subclasses among the 73 samples that were reclassified as Forest by the TieCall. Most
## contradictory classifications originated from samples previously interpreted as other wooded land (30 samples) or trees outside forest 
## (15 samples), suggesting that disagreements were concentrated in specific land-cover situations rather than being randomly distributed 
## among Non-Forest subclasses.


forest_classes <- c(
  "Naturally regenerating forest",
  "Planted or plantation forest"
)

nonforest_classes <- c(
  "other wooded land",
  "trees outside forest",
  "trees inside forest",
  "trees in urban areas",
  "trees for agricultural use",
  "no trees or shrubs present"
)

valid_all_subset %>%
  group_by(X2024_final_type_class) %>%
  summarise(
    total = n(),
    changed = sum(TieCall_changes_class),
    pct_changed = round(100 * changed / total, 2),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(
      X2024_final_type_class %in% forest_classes,
      "Forest",
      "Non-Forest"
    ),
    X2024_final_type_class = factor(
      X2024_final_type_class,
      levels = c(forest_classes, nonforest_classes)
    )
  ) %>%
  arrange(group, desc(pct_changed)) %>%
  select(-group) %>%
  bind_rows(
    summarise(
      .,
      X2024_final_type_class = "Total",
      total = sum(total),
      changed = sum(changed),
      pct_changed = round(100 * changed / total, 2)
    )
  )

#   X2024_final_type_class        total changed pct_changed(% over their class)
# Planted or plantation forest     31       4       12.9 
# Naturally regenerating forest   332       4        1.2 
# trees inside forest             115       8        6.96     # From this one, NonFor --> For
# other wooded land               500      32        6.4 
# trees outside forest            502      17        3.39
# trees for agricultural use       83       1        1.2 
# no trees or shrubs present     1298      13        1   
# trees in urban areas            227       2        0.88
# Total                          3088      81        2.62



TieCall_changes %>%
  filter(
    X2024_final_forest_class == "Non-forest",
    X2026_TieCall_forest_class == "Forest"
  ) %>%
  count(
    X2024_final_type_class,
    sort = TRUE
  ) %>%
  mutate(
    pct = round(100 * n / sum(n), 1)
  )




### Visual validation of the 81 samples ####

TieCall_changes
names(TieCall_changes)

#### Spreadsheet for data collection #### 
TieCall_review <- TieCall_changes %>%
  mutate(
    XRP_forest_class = NA_character_,
    XRP_type_class = NA_character_,
    XRP_confidence_level = NA_character_,
    XRP_class_issues = NA_character_,
    XRP_comments = NA_character_
  )

names(TieCall_review)

write.xlsx(TieCall_review, file = paste0(dir_geowiki, "TieCall_review.xlsx"), overwrite = FALSE)


#### KML preparation #### 

library(sf)
dir_kml_orig <- "/Users/xavi_rp/Documents/JRC_D1/copy_SharePoint_kk/"

IDs <- sort(unique(TieCall_review$ID))

target_ids <- TieCall_review$X2024_1st_sample_id

kml_list <- list()

for (id in sprintf("%02d", IDs)) {
  print(id)
  if(id == "10"){
    kml_i <- st_read(paste0(dir_kml_orig, "IIASA_Primary_regions_", "11", ".kml"),
                     quiet = TRUE)
  }else{
    kml_i <- st_read(paste0(dir_kml_orig, "IIASA_Primary_regions_", id, ".kml"),
                     quiet = TRUE)
  }
  
  kml_list[[id]] <- kml_i %>%
    filter(round(as.numeric(Name), 0) %in% target_ids)
  
}

kml_validation_XRP <- bind_rows(kml_list)

nrow(kml_validation_XRP)   # 81

st_write(kml_validation_XRP, paste0(dir_geowiki, "kml_validation_XRP.kml"), delete_dsn = TRUE, quiet = TRUE)




#### New validation table #### 

TieCall_review <- read.xlsx(paste0(dir_geowiki, "TieCall_review.xlsx"))

head(TieCall_review)
nrow(TieCall_review)

colSums(is.na(TieCall_review[, c(
  "XRP_forest_class",
  "X2024_final_forest_class",
  "X2026_1st_forest_class",
  "X2026_TieCall_forest_class"
)]))

unique(TieCall_review$XRP_forest_class)
unique(TieCall_review$X2024_final_forest_class)
unique(TieCall_review$X2026_1st_forest_class)
unique(TieCall_review$X2026_TieCall_forest_class)
table(TieCall_review$XRP_forest_class)
table(TieCall_review$X2024_final_forest_class)
table(TieCall_review$X2026_1st_forest_class)
table(TieCall_review$X2026_TieCall_forest_class)




TieCall_review <- TieCall_review %>%
  mutate(
    agree2024_class = XRP_forest_class == X2024_final_forest_class,
    agree2026_class = XRP_forest_class == X2026_1st_forest_class,
    agreeTie_class  = XRP_forest_class == X2026_TieCall_forest_class
  )

table(TieCall_review$agree2024_class)
table(TieCall_review$agree2026_class)
table(TieCall_review$agreeTie_class)



TieCall_review <- TieCall_review %>%
  mutate(
    agree2024_type = XRP_type_class == X2024_final_type_class,
    agree2026_type = XRP_type_class == X2026_1st_type_class,
    agreeTie_type  = XRP_type_class == X2026_TieCall_type_class
  )

TieCall_review <- TieCall_review %>%
  mutate(
    agree2024_all = agree2024_class & agree2024_type,
    agree2026_all = agree2026_class & agree2026_type,
    agreeTie_all  = agreeTie_class  & agreeTie_type
  )


table(TieCall_review$agree2024_class)
table(TieCall_review$agree2026_class)
table(TieCall_review$agreeTie_class)



## Forest / Non-Forest Agreement

tibble(
  Interpretation = c("2024-Final", "2026-1st", "TieCall"),
  Agreement = c(
    sum(TieCall_review$agree2024_class),
    sum(TieCall_review$agree2026_class),
    sum(TieCall_review$agreeTie_class)
  )
) %>%
  mutate(
    Percent = round(100 * Agreement / nrow(TieCall_review), 1)
  ) 

# Interpretation Agreement Percent
# 2024-Final            32    39.5
# 2026-1st              32    39.5
# TieCall               49    60.5



## Type class

tibble(
  Interpretation = c("2024-Final", "2026-1st", "TieCall"),
  Agreement = c(
    sum(TieCall_review$agree2024_type),
    sum(TieCall_review$agree2026_type),
    sum(TieCall_review$agreeTie_type)
  )
) %>%
  mutate(
    Percent = round(100 * Agreement / nrow(TieCall_review), 1)
  ) #%>% pull(Percent) %>% sum()


# Interpretation Agreement Percent
# 2024-Final            20    24.7
# 2026-1st              12    14.8
# TieCall               49    60.5
#   sum                 81    100


## Complete agreement
tibble(
  Interpretation = c("2024-Final", "2026-1st", "TieCall"),
  Agreement = c(
    sum(TieCall_review$agree2024_all),
    sum(TieCall_review$agree2026_all),
    sum(TieCall_review$agreeTie_all)
  )
) %>%
  mutate(
    Percent = round(100 * Agreement / nrow(TieCall_review), 1)
  ) #%>% pull(Agreement) %>% sum()

#  Interpretation Agreement Percent
# 2024-Final            20    24.7
# 2026-1st              12    14.8
# TieCall               49    60.5
#   sum                 81     100    # All the cases are equal to one of the 3 pregious assignments



#### Should the final dataset use the 2024-Final or the TieCall for the 81 disputed samples? ####

## 2024-Final vs TieCall
## The 12 cases agreeing with 2026-1st are not relevant

TieCall_review <- TieCall_review %>%
  mutate(
    winner = case_when(
      agree2024_all & !agreeTie_all ~ "2024",
      agreeTie_all & !agree2024_all ~ "TieCall",
      agree2024_all & agreeTie_all ~ "Both",
      !agree2024_all & !agreeTie_all ~ "Neither"
    )
  )

table(TieCall_review$winner) 
table(TieCall_review$winner) %>% sum()

#    2024   Neither   TieCall 
#     20      12        49 




## Binomial test
# null hypothesis: 2024 and TieCall are equally likely to agree with final review.

# The binomial test ignores the fact that both methods are evaluated on the same samples. Therefore, the McNemar test (done below) is more adequate.



binom.test(
  x = sum(TieCall_review$winner == "TieCall"),
  n = sum(TieCall_review$winner %in% c("2024","TieCall")),
  p = 0.5
)


# Exact binomial test
# 
# data:  sum(TieCall_review$winner == "TieCall") and sum(TieCall_review$winner %in% c("2024", "TieCall"))
# number of successes = 49, number of trials = 69, p-value = 0.0006362
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#   0.5884249  0.8131281
# sample estimates:
#   probability of success 
#   0.7101449 


## p-value = 0.0006362: significant, using 2024-Final or the Tie Call is not the same
## The estimated probability is 71.0%, with a 95% confidence interval of 58.8% – 81.3%
## The entire confidence interval is above 50%. That means the data are consistent with the TieCall 
## agreeing with the independent review in substantially more than half of the informative cases (69; 81-12).


## Effect size
# A p-value alone is not enough.

TieCall_review %>%
  filter(winner %in% c("2024","TieCall")) %>%
  count(winner) %>%
  mutate(
    pct = round(100*n/sum(n),1)
  )




## McNemar's test for paired data
# The binomial test ignores the fact that both methods are evaluated on the same samples.
# Among the samples where the two methods differ, does one method agree with the reference significantly more often than the other?

tab <- table(
  `2024-Final agrees with independent review` = factor(
    TieCall_review$agree2024_all,
    levels = c(TRUE, FALSE),
    labels = c("Yes", "No")
  ),
  `TieCall agrees with independent review` = factor(
    TieCall_review$agreeTie_all,
    levels = c(TRUE, FALSE),
    labels = c("Yes", "No")
  )
)

tab

#                                           TieCall agrees with independent review
# 2024-Final agrees with independent review      Yes       No
#                                       Yes       0        20
#                                       No       49        12

mcnemar.test(tab)

#	McNemar's Chi-squared test with continuity correction
# data:  tab
# McNemar's chi-squared = 11.362, df = 1, p-value = 0.0007495

# The TieCall agreed significantly (McNemar's test, p < 0.001) more often with an independent expert review than the 2024-Final interpretation.

# Conclusion:
# To support the selection of the final interpretation for the 81 samples where the TieCall disagreed with the consensus between the 2024-Final 
# and 2026-1st interpretations, an independent expert review was conducted. The reviewer was not constrained to choose between the previous
# interpretations and could assign a different Forest/Non-Forest class or subclass when considered appropriate. Of the 81 disputed samples, 
# the independent review agreed with the TieCall in 49 cases (60.5%), with the 2024-Final interpretation in 20 cases (24.7%), while neither
# interpretation matched the independent assessment in 12 cases (14.8%). Excluding these 12 inconclusive cases, the TieCall was supported in 
# 49 of the 69 informative samples (71.0%), compared with 20 samples (29.0%) supporting the 2024-Final interpretation. This difference was
# statistically significant (McNemar's test: χ² = 11.36, df = 1, p < 0.001), indicating that the TieCall was significantly more consistent 
# with the independent expert assessment than the 2024-Final interpretation. Based on these results, the TieCall interpretation was adopted 
# for all 81 disputed samples in the final dataset.






## Add GAUL information ####

