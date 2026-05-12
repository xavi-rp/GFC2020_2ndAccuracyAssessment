


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
          paste0(dir_geowiki, date_part, "_comparison_2026_2024_missing_points.csv"), 
          row.names = FALSE)





# merging both datasets

#data_2026_2024 <- full_join(data_geowiki, data_1stAssessment, by = "location_id")  # keep all rows from both
data_2026_2024 <- left_join(data_geowiki, data_1stAssessment, by = "location_id")

head(data_2026_2024)
nrow(data_2026_2024) # 13764
View(data_2026_2024)

names(data_2026_2024)

write.csv(data_2026_2024, 
          paste0(dir_geowiki, date_part, "_comparison_2026_2024.csv"), 
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
          paste0(dir_geowiki, date_part, "_comparison_2026_2024_clean.csv"), 
          row.names = FALSE)









## Selection criteria ####


## "Forest" / "Non-forest"

diff_FOR_NONFOR <- data_2026_2024 %>%
  filter(GFC.validation...forest != forest_class_3)

nrow(diff_FOR_NONFOR) # 882
View(diff_FOR_NONFOR)

FOR_NONFOR_comp <- table(diff_FOR_NONFOR$GFC.validation...forest) 
FOR_NONFOR_comp
# Forest    no assignment    Non-forest 
#  343            12           527 

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

check_completeness <- read.csv(paste0(dir_geowiki, date_part, "_check_completeness.csv"))
str(check_completeness)

diff_FOR_NONFOR_strata <- table(diff_FOR_NONFOR$groupid) %>%
  data.frame() %>% #str()
  mutate(Var1 = as.integer(as.character(Var1))) %>% #str()
  right_join(check_completeness, 
             by = c("Var1" = "groupid_2026")) %>%
  select(Var1, "Region...Countries", Sample_units_validated_2026, Freq) %>%
  #rename(Strata = Var1, Differently_Assigned = Freq)
  rename(Strata = Var1, FOR_NonFOR = Freq) #%>%
  #mutate(Total = rowSums(across(-c(1, 2)), na.rm = TRUE))

#totals <- diff_FOR_NONFOR_strata %>%
#  summarise(
#    across(1, ~ 0),
#    across(2, ~ "Total"),
#    across(-(1:2), ~ sum(.x, na.rm = TRUE))
#  )
#
#diff_FOR_NONFOR_strata <- bind_rows(diff_FOR_NONFOR_strata, totals)
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






## Data for re-ingestion ####

data_2026_2024_clean <- read.csv(paste0(dir_geowiki, date_part, "_comparison_2026_2024_clean.csv"))
names(data_2026_2024_clean)
length(unique(data_2026_2024_clean$location_id))   # 13764
length(unique(data_2026_2024_clean$sample_id))   # 13764
unique(data_2026_2024_clean$groupid)   


## Column names to be exported for re-ingestion
cols_included_export <- c("groupid",	"sample_id",	"location_id")
cols_included <- c("groupid",	"sample_id.x",	"location_id")





## Selecting sample units to be re-ingested for 2nd validation

## Scenario 1 (Disagreement in FOR/NonFOR + NA + Disagreement in FOR-Type)

diff_FOR_NONFOR <- data_2026_2024_clean %>%
  filter(GFC.validation...forest != X2024_forest_class_3)   # criterion: differently assigned FOR vs Non-FOR

nrow(diff_FOR_NONFOR) # 882  
head(diff_FOR_NONFOR)
View(diff_FOR_NONFOR)

table(diff_FOR_NONFOR$email)
table(diff_FOR_NONFOR$groupid)

#diff_FOR_NONFOR %>% select(sample_id.x, X2024_sample_id.y) %>% View()  # 'sample_id' differs between 2024 and 2026




unique(data_2026_2024_clean$GFC.validation...forest)
unique(data_2026_2024_clean$GFC.validation...forest.type)
unique(data_2026_2024_clean$X2024_type_class_3)

diff_FOR_type <- data_2026_2024_clean %>%
  filter(GFC.validation...forest == "Forest" & X2024_forest_class_3 == "Forest") %>% #nrow()  # 3863 (Forest in both)
  #filter(X2024_type_class_3 %in% c("Naturally regenerating forest", "Planted or plantation forest")) %>% nrow()
  filter(GFC.validation...forest.type != X2024_type_class_3) #%>% nrow()  # 363 

nrow(diff_FOR_type)

882 + 363  # 1245  Scenario 1 (not including the 61)
882 + 363 + 61  # 1306  Scenario 1 (including the 61)

diff_FOR_type
nrow(diff_FOR_type)
View(diff_FOR_type)
sum(is.na(diff_FOR_type$X2024_forest_class_3))

scenario1 <- rbind(diff_FOR_NONFOR, diff_FOR_type)
nrow(scenario1)  # 1245
View(scenario1)

sc1 <- table(scenario1$groupid)  %>%
  data.frame()  %>% #str()
  mutate(Var1 = as.integer(as.character(Var1))) %>%
  rename(Strata = Var1, Scenario1 = Freq)

table_final <- left_join(table_final, sc1, by = "Strata") 


#
## Scenario 2 (Scenario 1 + Disagreement in LU-Type)
unique(data_2026_2024_clean$GFC.validation...other.land.use.type)
unique(data_2026_2024_clean$X2024_type_class_3)

diff_NonFOR_LUType <- data_2026_2024_clean %>%
  filter(GFC.validation...forest == "Non-forest" & X2024_forest_class_3 == "Non-forest") %>% # nrow()  # 8958 (Non-Forest in both)
  filter(GFC.validation...other.land.use.type != X2024_type_class_3) #%>% # nrow()  # 2717

nrow(diff_NonFOR_LUType)  # 2717
View(diff_NonFOR_LUType)  # 2717

1306 + 2717   # 4023 (Scenario 2)

sum(diff_NonFOR_LUType$location_id %in% diff_FOR_type$location_id)
sum(diff_FOR_type$location_id %in% diff_NonFOR_LUType$location_id)


scenario2 <- rbind(scenario1, diff_NonFOR_LUType)
nrow(scenario2) #+ 61
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


#diff_FORFOR_confidence <- data_2026_2024_clean %>%
#  filter(GFC.validation...forest == "Forest" & X2024_forest_class_3 == "Forest") %>%
#  filter(GFC.validation...confidence != X2024_confidence_level_3) #%>% nrow() #View()  # 375
#  
#diff_NonFORNonFOR_confidence <- data_2026_2024_clean %>%
#  filter(GFC.validation...forest ==  "Non-forest" & X2024_forest_class_3 ==  "Non-forest") %>%
#  filter(GFC.validation...confidence != X2024_confidence_level_3) #%>% nrow() #View()  # 405
#
#diff_confidence <- rbind(diff_FORFOR_confidence, diff_NonFORNonFOR_confidence)
#
#diff_confidence %>% nrow()  # 780 (disagreement in confidence)
#View(diff_confidence)



scenario3 <- data_2026_2024_clean %>%  
  filter(GFC.validation...confidence != X2024_confidence_level_3) %>%
  rbind(., scenario2) %>%
  distinct() #%>% nrow()

nrow(scenario3)  # 4485 (Scenario 3)
length(unique(scenario3$location_id))
View(scenario3)  # 4485 (Scenario 3)

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

nrow(scenario4)  # 5690
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
  
nrow(scenario5)  # 5823

sc5 <- table(scenario5$groupid)  %>%
  data.frame()  %>% #str()
  mutate(Var1 = as.integer(as.character(Var1))) %>%
  rename(Strata = Var1, Scenario5 = Freq)

table_final <- left_join(table_final, sc5, by = "Strata") 
table_final

#table_final %>%
#  mutate(Total = rowSums(across(-c(1, 2, 3)), na.rm = TRUE))


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
    across(5:ncol(.), ~ .x + 61)
  ) %>%
  mutate(
    across(2, ~ "TOTAL +61"),
    across(3, ~ NA),
    across(4, ~ NA)
  )

#table_final <- table_final %>%
#  slice(-c(11, 12))

table_final <- bind_rows(table_final, totals_plus61)
table_final



write.csv(table_final, 
          paste0(dir_geowiki, date_part, "_scenarios_reingestion_2026.csv"), 
          row.names = FALSE)

#




# Save scenario

#write.csv(diff_FOR_NONFOR, 
#          paste0(dir_geowiki, date_part, "_samples_reingestion_2026.csv"), 
#          row.names = FALSE)










## selecting columns for re-ingestion and saving file to be sent to IIASA

samples_reingestion <- diff_FOR_NONFOR %>%
  select(cols_included) %>%
  rename(set_names(cols_included, cols_included_export)) #%>%

head(samples_reingestion)
nrow(samples_reingestion)  # 882 rows


write.csv(samples_reingestion, 
          paste0(dir_geowiki, date_part, "_samples_reingestion_2026_toIIASA.csv"), 
          row.names = FALSE)








## Dataset for tie callers ####

# Prepare a file which shows the tie caller the class selection in the first assessment and in the ongoing assessment


diff_FOR_NONFOR
names(diff_FOR_NONFOR)

cols_to_save <- c(
  "groupid",
  "pixel_center_x", "pixel_center_y",
  "sample_id.x",                                   
  "location_id", 
  "X2024_continent",
  "X2024_forest_class_3", "X2024_forest_class_num_3", "X2024_confidence_level_3",
  "X2024_class_issues_3", "X2024_type_class_3",
  "GFC.validation...forest", "GFC.validation...confidence", "GFC.validation...forest.type",
  "GFC.validation...other.land.use.type", "GFC.validation...Issues.with.class.assignment",
  "comment")

diff_FOR_NONFOR_forTieCallers <- diff_FOR_NONFOR %>%
  select(all_of(cols_to_save)) %>% 
  rename_with(
    ~ str_replace(.x, "^GFC", "X2026_GFC"),
    starts_with("GFC")) %>% 
  rename(sample_id = sample_id.x) #%>% View()  #head()



write.csv(diff_FOR_NONFOR_forTieCallers, 
          paste0(dir_geowiki, date_part, "_samples_reingestion_2026_forTieCallers.csv"), 
          row.names = FALSE)




#




