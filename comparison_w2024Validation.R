


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

full_2024_recreated <- read.csv(paste0(dir_1stAssessment, "final_2024_recreated.csv")) ## This is the good one


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

13764 - 13703  # 61 missing points  # 11 with the new data set


# rows from data_geowiki with no match in data_1stAssessment
missing_points <- anti_join(data_geowiki, data_1stAssessment, by = "location_id")
missing_points <- anti_join(data_geowiki, full_2024_recreated, by = "location_id")

nrow(missing_points)
View(missing_points)

table(missing_points$GFC.validation...forest)


write.csv(missing_points, 
          paste0(dir_geowiki, date_part, "_comparison_2026_2024_missing_points.csv"), 
          row.names = FALSE)





# merging both datasets

#data_2026_2024 <- full_join(data_geowiki, data_1stAssessment, by = "location_id")  # keep all rows from both
#data_2026_2024 <- left_join(data_geowiki, data_1stAssessment, by = "location_id")
data_2026_2024 <- left_join(data_geowiki, full_2024_recreated, by = "location_id")  # this is the good one

head(data_2026_2024)
nrow(data_2026_2024) # 13764 Correct
View(data_2026_2024)

names(data_2026_2024)

write.csv(data_2026_2024, 
          paste0(dir_geowiki, date_part, "_comparison_2026_2024.csv"), 
          row.names = FALSE)


# Cleaning the table

data_2026_2024_clean <- data_2026_2024 %>%
  select(1:21, 
         #"sample_id.y",
         #"forest_class_3",                               
         #"forest_class_num_3", 
         #"confidence_level_3",                           
         #"class_issues_3",     
         #"type_class_3",                                 
         #"continent",          
         #"GEZ_CODE",                                     
         #"strata",
         #"GFC_v2",
         #"GFC_v2_noMMU",
         #"location_i",                                   
         #"strata_gaul",
         #"gaul_opt1",
         #"continent_gaul"
         "X2024_email",            "X2024_groupid",          
         #"pixel_center_x",         "pixel_center_y", 
         "X2024_sample_id", "location_id" ,
         "X2024_forest_class",     "X2024_confidence_level", "X2024_type_class",       "X2024_class_issues",     "X2024_comment" 
         ) %>% 
  rename(
    "2024_sample_id.y" = "X2024_sample_id",
    "2024_forest_class_3" = "X2024_forest_class",
    #"2024_forest_class_num_3" = "forest_class_num_3",
    "2024_confidence_level_3" = "X2024_confidence_level",
    "2024_class_issues_3" = "X2024_class_issues",
    "2024_type_class_3" = "X2024_type_class",
    "2024_comment" = "X2024_comment",
    #"2024_GEZ_CODE" = "GEZ_CODE",
    #"2024_strata" = "strata",
    #"2024_GFC_v2" = "GFC_v2",
    #"2024_GFC_v2_noMMU" = "GFC_v2_noMMU",
    "location_id" = "location_id",
    #"2024_strata_gaul" = "strata_gaul",
    #"2024_gaul_opt1" = "gaul_opt1",
    #"2024_continent_gaul" = "continent_gaul"
    #"2024_sample_id.y" = "sample_id.y",
    #"2024_forest_class_3" = "forest_class_3",
    #"2024_forest_class_num_3" = "forest_class_num_3",
    #"2024_confidence_level_3" = "confidence_level_3",
    #"2024_class_issues_3" = "class_issues_3",
    #"2024_type_class_3" = "type_class_3",
    #"2024_continent" = "continent",
    #"2024_GEZ_CODE" = "GEZ_CODE",
    #"2024_strata" = "strata",
    #"2024_GFC_v2" = "GFC_v2",
    #"2024_GFC_v2_noMMU" = "GFC_v2_noMMU",
    #"2024_location_i" = "location_i",
    #"2024_strata_gaul" = "strata_gaul",
    #"2024_gaul_opt1" = "gaul_opt1",
    #"2024_continent_gaul" = "continent_gaul"
  ) #%>% names()



write.csv(data_2026_2024_clean, 
          paste0(dir_geowiki, date_part, "_comparison_2026_2024_clean.csv"), 
          row.names = FALSE)









## Selection criteria. Scenarios ####


### "Forest" / "Non-forest" ####

diff_FOR_NONFOR <- data_2026_2024 %>%
  #filter(GFC.validation...forest != forest_class_3)
  filter(GFC.validation...forest != X2024_forest_class)

nrow(diff_FOR_NONFOR) # 882 885
View(diff_FOR_NONFOR)

FOR_NONFOR_comp <- table(diff_FOR_NONFOR$GFC.validation...forest) 
FOR_NONFOR_comp
# Forest    no assignment    Non-forest 
#  343            12           527 530

sum(FOR_NONFOR_comp)  # 882 885 samples that have been differently assigned;
                      # Not considered those NAs in 2024 (missing points)


pivot_tab_FOR_NONFOR <- data_2026_2024 %>%
  #count(GFC.validation...forest, forest_class_3) %>%
  count(GFC.validation...forest, X2024_forest_class) %>%
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

#X2024_forest_class Forest Non-forest no assignment
#1             Forest   3864        530             3
#2         Non-forest    343       9004             9
#3               <NA>      6          4             1


## "Forest" / "Non-forest" + Confidence (in 2026)

diff_FOR_NONFOR

table(diff_FOR_NONFOR$GFC.validation...confidence)

# high confidence    low confidence     no assignment 
#      562               310              10 

#high confidence  low confidence   no assignment 
#      564             311              10 



## "Forest" / "Non-forest" + Strata

diff_FOR_NONFOR

table(diff_FOR_NONFOR$groupid) 

# strata:  363  364  365  366  367  368  369  370  371 
#   freq:  110  101   74   92   92   51   57  266   39 

# strata:  363 364 365 366 367 368 369 370 371 
#   freq:  110 101  74  92  92  51  57 269  39 

check_completeness <- read.csv(paste0(dir_geowiki, date_part, "_check_completeness.csv"))
str(check_completeness)

diff_FOR_NONFOR_strata <- table(diff_FOR_NONFOR$groupid) %>%
  data.frame() %>% #str()
  mutate(Var1 = as.integer(as.character(Var1))) %>% #str()
  right_join(check_completeness, 
             by = c("Var1" = "groupid_2026")) %>%
  select(Var1, "Region...Countries", Sample_units_validated_2026, Freq) %>%
  rename(Strata = Var1, FOR_NonFOR = Freq) 

diff_FOR_NONFOR_strata


miss_p <- table(missing_points$groupid) %>%
  data.frame()  %>% #str()
  mutate(Var1 = as.integer(as.character(Var1))) %>%
  rename(Strata = Var1, Missing_Points = Freq) #%>% str()

miss_p

diff_FOR_NONFOR_strata <- left_join(diff_FOR_NONFOR_strata, miss_p, by = "Strata") %>%
  relocate(Missing_Points, .before = FOR_NonFOR)
diff_FOR_NONFOR_strata

table_final <- diff_FOR_NONFOR_strata
table_final


diff_FOR_NONFOR_strata_ft <- flextable::flextable(diff_FOR_NONFOR_strata)
flextable::save_as_image(diff_FOR_NONFOR_strata_ft, 
                         path = paste0(dir_geowiki, date_part, "_diff_FOR_NONFOR_strata_ft.png"))

##






### Data for re-ingestion ####

data_2026_2024_clean <- read.csv(paste0(dir_geowiki, date_part, "_comparison_2026_2024_clean.csv"))
names(data_2026_2024_clean)
length(unique(data_2026_2024_clean$location_id))   # 13764
length(unique(data_2026_2024_clean$sample_id))   # 13764
unique(data_2026_2024_clean$groupid)   




## Selecting sample units to be re-ingested for 2nd validation

## Scenario 1 (Disagreement in FOR/NonFOR + NA + Disagreement in FOR-Type)

diff_FOR_NONFOR <- data_2026_2024_clean %>%
  filter(GFC.validation...forest != X2024_forest_class_3)   # criterion: differently assigned FOR vs Non-FOR

nrow(diff_FOR_NONFOR) # 882  885
head(diff_FOR_NONFOR)
View(diff_FOR_NONFOR)

table(diff_FOR_NONFOR$email)
table(diff_FOR_NONFOR$groupid)

#diff_FOR_NONFOR %>% select(sample_id.x, X2024_sample_id.y) %>% View()  # 'sample_id' differs between 2024 and 2026




unique(data_2026_2024_clean$GFC.validation...forest)
unique(data_2026_2024_clean$GFC.validation...forest.type)
unique(data_2026_2024_clean$X2024_type_class_3)

diff_FOR_type <- data_2026_2024_clean %>%
  filter(GFC.validation...forest == "Forest" & X2024_forest_class_3 == "Forest") %>% #nrow()  # 3863 3864 (Forest in both)
  #filter(X2024_type_class_3 %in% c("Naturally regenerating forest", "Planted or plantation forest")) %>% nrow()
  filter(GFC.validation...forest.type != X2024_type_class_3) #%>% nrow()  # 363 

nrow(diff_FOR_type)

885 + 363  # 1245 1248  Scenario 1 (not including the 61)
#882 + 363 + 61  # 1306  Scenario 1 (including the 61)
885 + 363 + 11  # 1306 1259 Scenario 1 (including the 11)

diff_FOR_type
nrow(diff_FOR_type)
View(diff_FOR_type)
sum(is.na(diff_FOR_type$X2024_forest_class_3))

scenario1 <- rbind(diff_FOR_NONFOR, diff_FOR_type)
nrow(scenario1)  # 1245 1248
View(scenario1)

sc1 <- table(scenario1$groupid)  %>%
  data.frame()  %>% #str()
  mutate(Var1 = as.integer(as.character(Var1))) %>%
  rename(Strata = Var1, Scenario1 = Freq)

table_final <- left_join(table_final, sc1, by = "Strata") 
table_final

#
## Scenario 2 (Scenario 1 + Disagreement in LU-Type)
unique(data_2026_2024_clean$GFC.validation...other.land.use.type)
unique(data_2026_2024_clean$X2024_type_class_3)

diff_NonFOR_LUType <- data_2026_2024_clean %>%
  filter(GFC.validation...forest == "Non-forest" & X2024_forest_class_3 == "Non-forest") %>% # nrow()  # 8958 9004 (Non-Forest in both)
  filter(GFC.validation...other.land.use.type != X2024_type_class_3) #%>% # nrow()  # 2717 2728

nrow(diff_NonFOR_LUType)  # 2717 2728
View(diff_NonFOR_LUType)  # 

1306 + 2717   # 4023 (Scenario 2)
1259 + 2728   # 3987 (Scenario 2 +11)

sum(diff_NonFOR_LUType$location_id %in% diff_FOR_type$location_id)
sum(diff_FOR_type$location_id %in% diff_NonFOR_LUType$location_id)


scenario2 <- rbind(scenario1, diff_NonFOR_LUType)
nrow(scenario2) # 3976 +11
length(unique(scenario2$location_id))


sc2 <- table(scenario2$groupid)  %>%
  data.frame()  %>% #str()
  mutate(Var1 = as.integer(as.character(Var1))) %>%
  rename(Strata = Var1, Scenario2 = Freq)

table_final <- left_join(table_final, sc2, by = "Strata") 
table_final



## Scenario 3: Scenario 2 + disagreement in confidence
# Disagreement in confidence: FOR_FOR and diff in confidence
#                             Non-FOR_Non-FOR and diff in confidence


scenario3 <- data_2026_2024_clean %>%  
  filter(GFC.validation...confidence != X2024_confidence_level_3) %>%
  rbind(., scenario2) %>%
  distinct() #%>% nrow()

nrow(scenario3)  # 4485  4500 (Scenario 3)
length(unique(scenario3$location_id))
View(scenario3)  # 4500 (Scenario 3)

sc3 <- table(scenario3$groupid)  %>%
  data.frame()  %>% #str()
  mutate(Var1 = as.integer(as.character(Var1))) %>%
  rename(Strata = Var1, Scenario3 = Freq)

table_final <- left_join(table_final, sc3, by = "Strata") 
table_final






## Scenario 4: Scenario 2 + disagreement in issues
#
unique(data_2026_2024_clean$GFC.validation...Issues.with.class.assignment)
unique(data_2026_2024_clean$X2024_class_issues_3)

scenario4 <- data_2026_2024_clean %>%  
  filter(GFC.validation...Issues.with.class.assignment != X2024_class_issues_3) %>% #nrow()
  rbind(., scenario2) %>% #nrow()
  distinct() #%>% nrow()

nrow(scenario4)  # 5690  5706
View(scenario4)


sc4 <- table(scenario4$groupid)  %>%
  data.frame()  %>% #str()
  mutate(Var1 = as.integer(as.character(Var1))) %>%
  rename(Strata = Var1, Scenario4 = Freq)

table_final <- left_join(table_final, sc4, by = "Strata") 
table_final

#




## Scenario 5: Scenario 2 + disagreement in confidence + disagreement in issues

scenario5 <- rbind(scenario2, scenario3, scenario4)  %>% #nrow()
  distinct() #%>% nrow()
  
nrow(scenario5)  # 5823  5839

sc5 <- table(scenario5$groupid)  %>%
  data.frame()  %>% #str()
  mutate(Var1 = as.integer(as.character(Var1))) %>%
  rename(Strata = Var1, Scenario5 = Freq)

table_final <- left_join(table_final, sc5, by = "Strata") 
table_final



totals <- table_final %>%
  summarise(
    across(1, ~ 0),
    across(2, ~ "Total"),
    across(-(1:2), ~ sum(.x, na.rm = TRUE))
  )

table_final <- bind_rows(table_final, totals)
table_final

totals_plus61 <- totals %>%
  mutate(
    #across(5:ncol(.), ~ .x + 61)
    across(5:ncol(.), ~ .x + 11)
  ) %>%
  mutate(
    across(2, ~ "TOTAL +11"),
    across(3, ~ NA),
    across(4, ~ NA)
  )

#table_final <- table_final %>%
#  slice(-c(11, 12))

table_final <- bind_rows(table_final, totals_plus61)
table_final



write.csv(table_final, 
          #paste0(dir_geowiki, date_part, "_scenarios_reingestion_2026.csv"), 
          paste0(dir_geowiki, date_part, "_scenarios_reingestion_2026_new.csv"), 
          row.names = FALSE)

#





## Scenario 2 is the one chosen

### Checks for addding relevant samples ####

nrow(scenario2)
View(scenario2)

# checks: 
scenario2 %>% filter(location_id == 1994994) %>% View()   # FOR vs Non-FOR
scenario2 %>% filter(location_id == 2106213) %>% View()   # Non-FOR vs FOR
scenario2 %>% filter(location_id == 1954876) %>% View()   # FOR vs FOR; disagreement FOR-Type
scenario2 %>% filter(location_id == 1953576) %>% View()   # Non-FOR vs Non-FOR; disagreement LU-Type



## Check (1) that all samples with "no assignemnt" in FOR/Non-FOR are included in scenario 2

unique(data_2026_2024_clean$GFC.validation...forest)

check1 <- data_2026_2024_clean %>% 
  filter(`GFC.validation...forest` == "no assignment") %>%   #nrow() # 13
  filter(!location_id %in% scenario2$location_id)  # 1

#sum(check1$location_id %in% scenario2$location_id)  # 

View(check1)
check1$location_id  # 1998834
missing_points %>% filter(location_id == 1998834)
scenario2 %>% filter(location_id == 1998834)

## it's one of the no assigned in 2026 and is a missing_point from 2024 (not in 'GFC_v2_accuracy-assessment_gaul.xlsx')
## However, it has assignment, e.g. in 'primary_data_latest.csv'
## It should be re-ingested with the 11 missing_points

data_2026_2024_clean %>% 
  filter(`GFC.validation...forest` == "no assignment") %>%
  pull(location_id)

# 1953624 1953733 1954792 1956114 1971889 1995327 1996303 1998590 1998834 2749904 2749985 2750076 2751011 


## Check (2) that all samples with "no assignemnt" in 'confidence' are included in scenario 2
unique(data_2026_2024_clean$GFC.validation...confidence)

check2 <- data_2026_2024_clean %>% 
  filter(`GFC.validation...confidence` == "no assignment") %>%  # nrow() # 113
  #filter(location_id %in% scenario2$location_id)   # 38 that are already in scenario 2 and do not need to be re-ingested
  filter(!location_id %in% scenario2$location_id)   # 75 that are not in scenario 2 and need to be re-ingested

View(check2) 
## the 113 samples that have confidence = "no assignment" in 2026, they are not included in scenario 2, 
## but they are all "high confidence" in 2024 (excepte one, which is already re-ingested by another rule). 
## None of them have disagreement in Forest/Non-Forest. 

check2 %>% filter(location_id == 1998834)


check2
nrow(check2)

# Adding them to scenario 2 data set
nrow(scenario2)
scenario2 <- rbind(scenario2, check2) #%>% nrow()  # 4037  4051
#scenario2 <- rbind(scenario2, check1, check2)# %>% nrow()  # 
nrow(scenario2) # 4051
length(unique(scenario2$location_id))

scenario2 <- scenario2 %>%
  distinct(location_id, .keep_all = TRUE) #%>% nrow()

nrow(scenario2) # 4051
scenario2 %>% filter(location_id == 1998834)


## updating samples ssummary table 

table_final <- table_final %>%
  slice(-c(10, 11))

table_final
table(scenario2$groupid) %>% data.frame()
sum(table(scenario2$groupid))


Sc2_missing_confidence <- table(scenario2$groupid)  %>%
  data.frame()  %>% #str()
  mutate(Var1 = as.integer(as.character(Var1))) %>%
  rename(Strata = Var1, Sc2_missing_confidence = Freq)

table_final <- left_join(table_final, Sc2_missing_confidence, by = "Strata") %>%
  relocate("Sc2_missing_confidence", .after = "Scenario2")
table_final


totals <- table_final %>%
  summarise(
    across(1, ~ 0),
    across(2, ~ "Total"),
    across(-(1:2), ~ sum(.x, na.rm = TRUE))
  )

table_final <- bind_rows(table_final, totals)
table_final

totals_plus11 <- totals %>%
  mutate(
    across(5:ncol(.), ~ .x + 11)
  ) %>%
  mutate(
    across(2, ~ "TOTAL +11"),
    across(3, ~ NA),
    across(4, ~ NA)
  )

#table_final <- table_final %>%
#  slice(-c(11, 12))

table_final <- bind_rows(table_final, totals_plus11)
table_final


## Check (3) that all samples with NA both in 'Forest type' and 'other LU type' are included in scenario 2.

## They are the same 13 samples detected in check 1


## Check (4): that there are no samples with NA in 'issues'

sum(is.na(unique(data_2026_2024_clean$GFC.validation...Issues.with.class.assignment)))  # 0





### Adding missing samples (11 samples in 2026) to scenario 2 + missing confidence ####

names(missing_points)
View(missing_points)

missing_points
  

names(scenario2)
View(scenario2)
nrow(scenario2)

missing_points %>% 
  filter(!location_id %in% scenario2$location_id)  # 10, because 1 has already been selected in the previous checks (missing confidence)

full_2024_recreated %>% 
  filter(location_id %in% missing_points$location_id)  # 0


names(scenario2) 
names(missing_points) 
names(scenario2)[!names(scenario2) %in% names(missing_points)]

missing_points_1 <- missing_points %>% 
  rename(
    pixel_center_x.x = pixel_center_x,
    pixel_center_y.x = pixel_center_y)
  
missing_points_1

missing_cols <- setdiff(names(scenario2), names(missing_points_1))
missing_points_1[missing_cols] <- NA

missing_points_1 <- missing_points_1 %>%
  select(all_of(names(scenario2)))


#
nrow(scenario2)

scenario2_plusMissing <- rbind(scenario2, missing_points_1) %>% 
  distinct(location_id, .keep_all = TRUE) #%>% nrow()

#scenario2_plusMissing <- scenario2

nrow(scenario2_plusMissing) # 4061
View(scenario2_plusMissing)



### splitting X2024_type_class_3 into forest_type and other LU type ####

unique(scenario2_plusMissing$X2024_type_class_3)

forest_classes <- c("Naturally regenerating forest", "Planted or plantation forest")

scenario2_plusMissing <- scenario2_plusMissing %>%
  mutate(
    X2024_forest_type = if_else(
      X2024_type_class_3 %in% forest_classes,
      X2024_type_class_3,
      NA
    ),
    X2024_other_LU_type = if_else(
      !(X2024_type_class_3 %in% forest_classes),
      X2024_type_class_3,
      NA
    )
  ) %>%
  relocate(c(X2024_forest_type, X2024_other_LU_type),
           .after = X2024_confidence_level_3) %>% 
  select(-X2024_type_class_3)


#

length(scenario2_plusMissing$location_id)
table(scenario2_plusMissing$groupid) %>% data.frame()
nrow(scenario2_plusMissing)
View(tail(scenario2_plusMissing))


# Saving scenario

write.csv(scenario2_plusMissing, 
          paste0(dir_geowiki, date_part, "_samples_reingestion_2026.csv"), 
          row.names = FALSE)










## File to be sent to IIASA ####
## selecting columns for re-ingestion and saving file to be sent to IIASA

## Column names to be exported for re-ingestion
names(scenario2_plusMissing)

cols_included_export <- c("groupid",	"sample_id",	"location_id",
                          "X2024_groupid", "X2024_sample_id.y")  # included 2024 columns of groupid (strata) and sample_id, just for information, can be removed
#cols_included <- c("groupid",	"sample_id.x",	"location_id")



samples_reingestion <- scenario2_plusMissing %>%
  select(all_of(cols_included_export)) #%>% head()

head(samples_reingestion)
nrow(samples_reingestion)  # 4061 rows


write.csv(samples_reingestion, 
          paste0(dir_geowiki, date_part, "_samples_reingestion_2026_toIIASA.csv"), 
          row.names = FALSE)








## Dataset for tie callers ####

# Prepare a file which shows the tie caller the class selection in the first assessment and in the ongoing assessment

scenario2_plusMissing
View(scenario2_plusMissing)
nrow(scenario2_plusMissing)  # 4061
names(scenario2_plusMissing)

table_for_tie_callers <- scenario2_plusMissing %>% 
  select(
    groupid,
    pixel_center_x.x, pixel_center_y.x,
    sample_id, 
    location_id,
    GFC.validation...forest, GFC.validation...confidence, GFC.validation...forest.type,
    GFC.validation...other.land.use.type, GFC.validation...Issues.with.class.assignment, comment,
    X2024_groupid, X2024_sample_id.y,                            
    X2024_forest_class_3, X2024_confidence_level_3, X2024_forest_type,
    X2024_other_LU_type, X2024_class_issues_3,
    X2024_comment
    ) %>%
  rename_with( ~ str_replace(.x, "GFC", "X2026_GFC")) %>%
  rename(pixel_center_x = pixel_center_x.x,  pixel_center_y = pixel_center_y.x,
         X2026_groupid = groupid,
         X2026_sample_id = sample_id) %>%
  relocate(starts_with("X2024"), .after = location_id)

head(table_for_tie_callers)
View(table_for_tie_callers)
nrow(table_for_tie_callers)  # 4061

#


write.csv(table_for_tie_callers, 
          paste0(dir_geowiki, date_part, "_samples_reingestion_2026_forTieCallers.csv"), 
          row.names = FALSE)




#






## Interpretation 2024 assessment ####

dir_old_results <- "/Users/xavi_rp/Documents/JRC_D1/copy_SharePoint_kk/validation/Results/"

primary_data_latest <- read.csv(paste0(dir_old_results, "primary_data_latest.csv"))

View(primary_data_latest[1, ]) # 21752
nrow(primary_data_latest) # 21752


secondary_data_latest <- read.csv(paste0(dir_old_results, "secondary_data_latest_adjusted.csv"))
View(secondary_data_latest[1:10, ])

sum(names(primary_data_latest) == names(secondary_data_latest))
names(primary_data_latest)[!names(primary_data_latest) %in% names(secondary_data_latest)]


## Criterion 1
crit1 <- primary_data_latest %>% 
  filter(name.3 == "high confidence") # %>% nrow()   # 20379

nrow(crit1)
names(crit1)

crit1 <- crit1 %>% 
  select(email, 
         groupid,
         pixel_center_x, pixel_center_y,
         sample_id,
         location_id,
         name.1,
         name.3,
         #name.5,
         name.9,
         name.7,
         comment) %>% 
  rename(
    "X2024_email" = "email",
    "X2024_groupid" = "groupid",
    "X2024_sample_id" = "sample_id",
    #"X2024_location_id" = "location_id",
    "X2024_forest_class" = "name.1",
    #"2024_forest_class_num" = "forest_class_num_3",
    "X2024_confidence_level" = "name.3",
    "X2024_type_class" = "name.9",
    "X2024_class_issues" = "name.7",
    "X2024_comment" = "comment"
    #"2024_continent" = "continent",
    #"2024_GEZ_CODE" = "GEZ_CODE",
    #"2024_strata" = "strata",
    #"2024_GFC_v2" = "GFC_v2",
    #"2024_GFC_v2_noMMU" = "GFC_v2_noMMU",
    #"2024_strata_gaul" = "strata_gaul",
    #"2024_gaul_opt1" = "gaul_opt1",
    #"2024_continent_gaul" = "continent_gaul"
  ) #%>% 
View(crit1)


## Criterion 2

crit2 <- secondary_data_latest %>% 
  filter(name.3 == "high confidence")  %>%   #nrow()   # 3542
  filter_out(location_id %in% crit1$location_id) #%>%   nrow()  # 1093


nrow(crit2)  # 1093
sum(crit2$location_id %in% crit1$location_id)  # 0

crit2 <- crit2 %>% 
  select(email, 
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
    "X2024_email" = "email",
    "X2024_groupid" = "groupid",
    "X2024_sample_id" = "sample_id",
    #"X2024_location_id" = "location_id",
    "X2024_forest_class" = "name.1",
    #"2024_forest_class_num" = "forest_class_num_3",
    "X2024_confidence_level" = "name.3",
    "X2024_type_class" = "name.7",
    "X2024_class_issues" = "name.5",
    "X2024_comment" = "comment.3"
  ) #%>% 

View(crit2)



## Criterion 3

remaining <- primary_data_latest %>% 
  filter_out(location_id %in% crit1$location_id) %>% 
  filter_out(location_id %in% crit2$location_id)  #%>%  nrow() # 280

nrow(remaining)


prim_For_low <- primary_data_latest %>% 
  filter(name.1 == "Forest") %>% 
  filter(name.3 == "low confidence") %>% 
  pull(location_id)

sec_For_low <- secondary_data_latest %>% 
  filter(name.1 == "Forest") %>% 
  filter(name.3 == "low confidence") %>% 
  pull(location_id)

keep_For_low <- prim_For_low[prim_For_low %in% sec_For_low]
length(keep_For_low)

prim_NonFor_low <- primary_data_latest %>% 
  filter(name.1 == "Non-forest") %>% 
  filter(name.3 == "low confidence") %>% 
  pull(location_id)

sec_NonFor_low <- secondary_data_latest %>% 
  filter(name.1 == "Non-forest") %>% 
  filter(name.3 == "low confidence") %>% 
  pull(location_id)

keep_NonFor_low <- prim_NonFor_low[prim_NonFor_low %in% sec_NonFor_low]
length(keep_NonFor_low)

sum(keep_For_low %in% keep_NonFor_low) # 0
sum(keep_NonFor_low %in% keep_For_low)


keep_low <- c(keep_For_low, keep_NonFor_low)
length(keep_low)


crit3 <- primary_data_latest %>% 
  filter(location_id %in% keep_low) # %>% nrow()  # 153

nrow(crit3)

crit3 <- crit3 %>% 
  select(email, 
         groupid,
         pixel_center_x, pixel_center_y,
         sample_id,
         location_id,
         name.1,
         name.3,
         name.9,
         name.7,
         comment.1) %>% 
  rename(
    "X2024_email" = "email",
    "X2024_groupid" = "groupid",
    "X2024_sample_id" = "sample_id",
    #"X2024_location_id" = "location_id",
    "X2024_forest_class" = "name.1",
    #"2024_forest_class_num" = "forest_class_num_3",
    "X2024_confidence_level" = "name.3",
    "X2024_type_class" = "name.9",
    "X2024_class_issues" = "name.7",
    "X2024_comment" = "comment.1"
  ) #%>% 

View(crit3)


## Criterion 4: From the remaining sample, make a decision which interpretation to use (103)

ref_for_crit4 <- primary_data_latest %>% 
  filter_out(location_id %in% crit1$location_id) %>% 
  filter_out(location_id %in% crit2$location_id) %>% 
  filter_out(location_id %in% crit3$location_id) %>% # nrow()  # 127
  pull(location_id) %>%  sort()

length(ref_for_crit4)  # 127
  
nrow(primary_data_latest)
sum(primary_data_latest$name.1 == "no assignment")  # 3
primary_data_latest %>% filter(name.1 == "no assignment") %>% pull(location_id) %>% sort()      # 2130589 2131101 2131108

sum(secondary_data_latest$name.1 == "no assignment")  # 4
secondary_data_latest %>% filter(name.1 == "no assignment") %>% pull(location_id) %>%  sort()   # 1996944 2130378 2130449 2130501


ref_for_crit4_final <- data_1stAssessment %>% 
  filter(location_id %in% ref_for_crit4) %>%  # nrow()   # 103
  pull(location_id) %>%  sort()


crit4 <- data_1stAssessment %>% 
  filter(location_id %in% ref_for_crit4)

nrow(crit4) # 103

crit4 <- crit4 %>% 
  select(email_3, 
         #groupid,
         #pixel_center_x, pixel_center_y,
         sample_id,
         location_id,
         forest_class_3,
         confidence_level_3,
         type_class_3,
         class_issues_3,
         #comment.1
         ) %>% 
  rename(
    "X2024_email" = "email_3",
    #"X2024_groupid" = "groupid",
    "X2024_sample_id" = "sample_id",
    #"X2024_location_id" = "location_id",
    "X2024_forest_class" = "forest_class_3",
    #"2024_forest_class_num" = "forest_class_num_3",
    "X2024_confidence_level" = "confidence_level_3",
    "X2024_type_class" = "type_class_3",
    "X2024_class_issues" = "class_issues_3",
    #"X2024_comment" = "comment.1"
  ) #%>%
  

missing_cols <- setdiff(names(crit1), names(crit4))
crit4[missing_cols] <- NA

crit4 <- crit4 %>%
  select(all_of(names(crit1)))

View(crit4)

## Remaining after 4 criteria: no assignation possible

nrow(primary_data_latest)  # 21752
nrow(crit1) + nrow(crit2) + nrow(crit3) + nrow(crit4)  # 21728
nrow(primary_data_latest) - (nrow(crit1) + nrow(crit2) + nrow(crit3) + nrow(crit4))  # 24


unassigned_samples_refs <- primary_data_latest %>% 
  filter_out(location_id %in% crit1$location_id) %>% 
  filter_out(location_id %in% crit2$location_id) %>% 
  filter_out(location_id %in% crit3$location_id) %>%
  filter_out(location_id %in% crit4$location_id) %>%   #nrow() # 24
  pull(location_id) %>%  sort()

unassigned_samples_refs
# 1974009 1993220 1993332 1996816 1996944 1997072 1997129 1998687 1998834 1998855 1998864 2129075 
# 2129688 2129888 2130227 2130378 2130406 2130501 2130570 2130589 2130647 2130846 2131101 2131108


missing_points %>% 
  filter(location_id %in% unassigned_samples_refs) %>%  nrow()  # 11

data_geowiki %>% 
  filter(location_id %in% unassigned_samples_refs) %>%  nrow()  # 11

input_file_df %>% 
  filter(location_id %in% unassigned_samples_refs) %>%  #nrow()
  pull(location_id) %>%  unique() %>% sort() #%>% length()                 # 11

# Assessed in 2026:
# 1998687 1998834 1998855 1998864 2129075 2129688 2129888 2130227 2130378 2130501 2130846


## Full 2024 final file ####

full_2024_recreated <- rbind(crit1, crit2, crit3, crit4) #%>% nrow()

nrow(full_2024_recreated) # 21728
View(full_2024_recreated)

nrow(primary_data_latest) # 21752
nrow(secondary_data_latest) # 4000

primary_data_latest %>% 
  filter(location_id %in% unassigned_samples_refs) %>% nrow()

secondary_data_latest %>% 
  filter(location_id %in% unassigned_samples_refs) %>% nrow()



write.csv(full_2024_recreated, 
          paste0(dir_1stAssessment, "final_2024_recreated.csv"), 
          row.names = FALSE)

