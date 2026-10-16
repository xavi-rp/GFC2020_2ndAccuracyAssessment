
#______________________________________________________________________________________________________________________#
#______________________________________________________________________________________________________________________#
#________                   Global Forest Cover (GFC) Map 2020 Accuracy Assessment                                 ____#
#________                                                                                                          ____#
#________  Script based on the script used for the GFC 1st assessment, and shared in:                              ____#
#________  https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/FOREST/EUFO/GFC2020_validation/                        ____#
#________  And its revised version in:                                                                             ____#
#________  https://github.com/xavi-rp/GFC2020_2ndAccuracyAssessment/blob/main/accuracy_assessment_GFCV2_xavi.R     ____#
#________                                                                                                          ____#
#______________________________________________________________________________________________________________________#
#______________________________________________________________________________________________________________________#

## The main objective of this script is to run the accuracy assessment on the GFC2020 (any version)
## using the new version of the validation data set (Collection2 -c2-).


## Setup ####

## libraries
library(mapaccuracy)
library(tidyverse)
library(openxlsx)
library(flextable)
library(officer)
library(sf)
library(rnaturalearth)



# set directories

dir_assessment2024 <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_first/GFC2020_validation/"

dir_Valid_c2 <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_Second/geowiki_2026/"

#dir_GFTv1_assessment_c2 <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_GFT/GFTv1_assessment_c2/"
#dir_GFTv2_assessment_c2 <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_GFT/GFTv2_assessment_c2/"
dir_GFC_assessment_c2 <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_Second/2nd_Assessment/GFC2020_v1_Valid_c2"



## Loading datasets ####

#Valid_c2 <- read.csv(paste0(dir_Valid_c2, "Final_2026_GFC_Validation_Dataset.csv"))
#names(Valid_c2)
#head(Valid_c2)
#nrow(Valid_c2) # 21752
#
## I need the centroid coordinates
#valid_all <- read.csv(paste0(dir_Valid_c2, "AllValidations_2024_2026.csv"))
#names(valid_all)
#
#
#Valid_c2 <- valid_all %>% 
#  select(location_id, "X2024_1st_pixel_center_x", "X2024_1st_pixel_center_y") %>% 
#  #apply(2, function(x) sum(is.na))
#  right_join(Valid_c2, by = "location_id") %>% 
#  #nrow() # 21752
#  filter(UsedIn_1stAssessment == "yes") #%>% nrow()  # 21612
#
#
#writeFile <- "yes"
#writeFile <- "no"
#if(writeFile == "yes"){
#  write.csv(Valid_c2, 
#            paste0(dir_Valid_c2, "Valid_c2_coords.csv"), 
#            row.names = FALSE)
#}

  
Valid_c2 <- read.csv(paste0(dir_Valid_c2, "Valid_c2_coords.csv")) 

head(Valid_c2)
names(Valid_c2)
nrow(Valid_c2)  # 21612
table(Valid_c2$forest_class)
#     Forest   Non-forest 
#       6658        14954 


## _______________________________________
## This dataset is for GEE. There, I'll extract GFT values for each sample using the coordinates of the centroid
## _______________________________________


## Exploring "Valid_c2_coords_GFC2020_V??.csv" (exported from GEE)  ####

dwnld_it <- "yes"
dwnld_it <- "no"

if(dwnld_it == "yes"){
  library(googledrive)
  drive_auth()
  2
  drive_ls() %>% print(n = 30)
  
  drive_download("Valid_c2_coords_GFC2020_V1.csv", 
                 path = paste0(dir_GFC_assessment_c2, "/Valid_c2_coords_GFC2020_V1.csv"),
                 overwrite = TRUE) %>% with_drive_quiet()
}
2

## Exploring the dataset

#Valid_c2_GFT2020_V22 <- read.csv(paste0(dir_GFTv2_assessment_c2, "Valid_c2_coords_GFT2020_V22.csv"))
Valid_c2_GFC2020 <- read.csv(paste0(dir_GFC_assessment_c2, "/Valid_c2_coords_GFC2020_V1.csv"))

Valid_c2_GFC2020 <- Valid_c2_GFC2020 %>% 
  rename(GFC2020 = Map) #%>% head()

head(Valid_c2_GFC2020)
nrow(Valid_c2_GFC2020) # 6715
table(Valid_c2_GFC2020$GFC2020)  # 
#    1  
# 7278  

table(Valid_c2_GFC2020$forest_class)  # reference data (C2)
#     Forest  Non-forest 
#       6069       1209 (non-forest in the validation dataset, but forest in the map)


## Check samples that are forest in the validation dataset, but are not mapped as forest in GFC (758 points like that) 
Valid_c2 %>% 
  filter_out(location_id %in% Valid_c2_GFC2020$location_id) %>% 
  #nrow() # 14334   (14334 + 7278 = 21612)
  #pull(forest_class) %>% table()
#     Forest  Non-forest 
#        589      13745    (589 For in C2, Non-For in GFC)
  #select(location_id, X2024_1st_pixel_center_x, X2024_1st_pixel_center_y, forest_class, type_class) %>% 
  select(location_id, X2024_1st_pixel_center_x, X2024_1st_pixel_center_y, forest_class) %>% 
  filter(forest_class == "Forest") %>% #nrow  # 701
  tail(50)  ## With these coordinates, check in GEE
  


## Merging with those rows from the Validation dataset that are not exported from GEE, and therefore Non-forest in GFC

Valid_c2_GFC2020_full <- Valid_c2 %>%
  left_join(
    Valid_c2_GFC2020 %>%
      select(location_id, GFC2020),
    by = "location_id"
  ) 

head(Valid_c2_GFC2020_full)
table(Valid_c2_GFC2020_full$GFC2020)    # 
#    1
# 7278  
sum(table(Valid_c2_GFC2020_full$GFC2020))   # 7278  Forest in GFC
sum(is.na(Valid_c2_GFC2020_full$GFC2020))   # 14334 Non-forest in GFC


## Recoding GFC2020 into a binary variable Forest/Non-forest

Valid_c2_GFC2020_full <- Valid_c2_GFC2020_full %>%
  mutate(
    GFC_forest = if_else(
      is.na(GFC2020),
      "Non-Forest",
      "Forest"
    )
  ) #%>% 

head(Valid_c2_GFC2020_full)
table(Valid_c2_GFC2020_full$GFC2020)
table(Valid_c2_GFC2020_full$GFC_forest)
sum(is.na(Valid_c2_GFC2020_full$GFC_forest)) # 0

  
## 2 × 2 confusion matrix comparing validation reference (C2, forest_class) against the GFT-derived Forest/Non-Forest classification (Map)

cm <- Valid_c2_GFC2020_full %>% 
  count(forest_class, GFC_forest) %>%
  rename(ValidC2_forest_class = forest_class) #%>% 
  #pivot_wider(
  #  names_from = GFT_forest,
  #  values_from = n,
  #  values_fill = 0
  #)
cm

p <- ggplot(cm, aes(x = GFC_forest, y = ValidC2_forest_class, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), size = 6) +
  scale_fill_gradient(low = "#DCEAF7", high = "#08519C") +
  labs(
    x = "GFC2020 V1",
    y = "ValidC2 reference",
    fill = "Samples"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = 14)
  )

p

sve_it <- "yes"
sve_it <- "no"
if(sve_it == "yes"){
  ggsave(paste0(dir_GFC_assessment_c2, "/Valid_c2_GFC2020_V1_Forest_NonForest_confusion_matrix.png"), 
         p, width = 7, height = 6, dpi = 300)
}





## Validation ####

# load the the file with the strata names and count
strata_area <- read.csv(paste0(dir_assessment2024, "strata_size.csv"))

head(strata_area)
nrow(strata_area)   # 149
names(strata_area)   # 'Strata',  'strata_ha'
sort(unique(strata_area$Strata))   # e.g. 1001, etc
length(unique(strata_area$Strata))  # 149 strata, from 1001 to 8226


# load the files with the sample units interpretation and the map values

### GFC_vX_Valid_c2 ####
#scenario_kk <- read.csv(paste0(dir_assessment2024, "combined_scenario_all_GFCV2.csv"), sep = ",", na.strings = c#("NA", "N/A"), stringsAsFactors = FALSE)
#head(scenario_kk)
#
#table(scenario_kk$forest_class_num)
#table(scenario_kk$forest_class)

head(Valid_c2_GFC2020_full)


scenario <- Valid_c2_GFC2020_full %>% 
  rename(pixel_center_x = X2024_1st_pixel_center_x, 
         pixel_center_y = X2024_1st_pixel_center_y, 
         sample_id = X2024_1st_sample_id) %>% 
  mutate(
    forest_class_num = case_when(  # reference
      forest_class == "Forest" ~ 1,
      forest_class == "Non-forest" ~ 0,
      is.na(forest_class) ~ 100
    )
  ) %>% 
  select(pixel_center_x, 
         pixel_center_y, 
         sample_id, 
         forest_class_num, # ref
         type_class,       # ref
         strata, gaul, 
         GFC2020)  # Map

head(scenario)
table(scenario$forest_class)
table(scenario$forest_class_num)
#     0     1 
# 14954  6658 


scenario <- scenario %>% mutate(GFC2020 = coalesce(GFC2020, 0))
table(scenario$GFC2020)
#     0       1 
# 14334    7278 

sum(is.na(scenario$GFC2020)) # 0



names(scenario)   # "pixel_center_x"   "pixel_center_y"   "sample_id" "forest_class_num"  "type_class"  "strata"  "gaul"  "GFC2020" 
nrow(scenario)   # 21612

apply(scenario, 2, function(x) sum(is.na(x)))   # no NAs
sort(unique(scenario$forest_class_num))
sort(unique(scenario$GFC2020))
head(scenario)


## Strata info ####

# Nh_strata numeric vector. Number of pixels forming each stratum (it's actually the area in ha). It must be named (see details)
head(strata_area)

Nh_strata <- strata_area$strata_ha
names(Nh_strata) <- strata_area$Strata

Nh_strata

head(Nh_strata)
sum(Nh_strata)



## Sample units interpretation and map values ####
dim(scenario)  # 21612, 8
sum(is.na(scenario$strata))  # 0


# drop the sample units with no strata value associated (0 value in the strata column)- 62 sample units
df_dropped <- scenario %>% 
  filter(strata != "0")

dim(df_dropped)


# drop the samples with no assignment (value 100) in the forest_class_num column - 24 samples

df_dropped_2 <- df_dropped %>% 
  filter(forest_class_num != "100")

dim(df_dropped_2)


# drop the samples located outside the FAO GAUL boundaries - 54 additional samples
table(df_dropped_2$gaul)

df_dropped_3 <- df_dropped_2 %>% 
  filter(gaul != "0")

dim(df_dropped_3)  # 21612, 8
head(df_dropped_3)

unique(df_dropped_3$forest_class_num)
unique(df_dropped_3$GFC2020)  
table(df_dropped_3$forest_class_num)
table(df_dropped_3$GFC2020)
sum(is.na(df_dropped_3$GFC2020))


# clean dataset
map_ref_values <- df_dropped_3
head(map_ref_values)
nrow(map_ref_values)  # 21612

apply(map_ref_values, 2, function(x) sum(is.na(x)))
table(map_ref_values$GFC2020)
#     0     1 
# 14334  7278 




## Analysis ####

#??stehman2014

# s character vector. Strata class labels. The object will be coerced to factor.
s <- map_ref_values$strata
sum(is.na(s)) # 0


# r character vector. Reference class labels (validation dataset). The object will be coerced to factor.
r <- map_ref_values$forest_class_num


# m character vector. Map class labels (results of the model: GFC2020_vX -- v4 in this case). The object will be coerced to factor.
names(map_ref_values)
m <- map_ref_values$GFC2020
m

# need to indicate the order for the labels of the matrix (0 non forest, 1 forest)
order <- c(0,1)
scenario_AA <- stehman2014(s,           # Strata class labels
                           r,           # Reference class labels (validation dataset)
                           m,           # Map class labels (results of the model: GFC2020_vX)
                           Nh_strata,   # Number of pixels forming each stratum (it's actually the area in ha) 
                           margins = TRUE,
                           order)

scenario_AA



### Results ####

## Overall accuracy
scenario_AA$OA   # v2_Valid_c1: 0.9150627  # v2_Valid_c2: 0.9159055   # v4_Valid_c2: 0.9312725   # (v1_Valid_c2) 0.9176407
round(scenario_AA$OA, 3)   # 0.915         # v2_Valid_c2: 0.916       # v4_Valid_c2: 0.931       # (v1_Valid_c2) 0.918


## User's accuracy of the two classes. Related to the Commission Error (False positives)
scenario_AA$UA

#    0 (Non-For)         1  (For)
# 0.9630748            0.8200459 
# 0.9606977            0.8300328      (v1_Valid_c2)
# 0.9636623            0.8213937      (v2_Valid_c2)
# 0.9565429            0.8746267      (v4_Valid_c2)




#           0            1 
#  0.03692525   0.17995405     # Coommission Error = 0.17995405
# The estimated commission error for the Forest class was 18.0%, indicating that approximately 18%
# of the area mapped as Forest is estimated to correspond to Non-forest in the reference data.
#  0.9606977    0.8300328     (v1_Valid_c2)
#  0.03633772   0.17860631     # Coommission Error = 0.17860631    (Valid_c2)
#  0.04345707   0.12537333     (v4_Valid_c2)



print(paste0("Commission Error (Forest class) = ",
             round((1 - scenario_AA$UA[2]), 3)))  

print(paste0("Commission Error (Non-Forest class) = ",
             round((1 - scenario_AA$UA[1]), 3)))        # 0.037 (about 3.7% of the area (or pixels) mapped as Non-forest is 
# estimated to actually be Forest.)
# 0.039    (v1_Valid_c2)
# 0.036   (v2_Valid_c2)
# 0.043   (v4_Valid_c2)



# | Class      | User's Accuracy | Commission Error | Meaning                                              |
# | ---------- | --------------: | ---------------: | ---------------------------------------------------- |
# | Forest     |           82.0% |            18.0% | 18% of mapped Forest pixels are false positives      |
# | Non-forest |           96.3% |             3.7% | 3.7% of mapped Non-forest pixels are false positives |



## Producer's accuracy. Related to Omission Error (False Negatives)
scenario_AA$PA

#         0         1 
# 0.9137283   0.9181793 
#  0.9200039 0.9121230     v1_Valid_c2
# 0.9143668   0.9194979     (v2_Valid_c2)
# 0.9447583   0.8997854     (v4_Valid_c2)


print(paste0("Ommission Error (Forest) = ",
             round((1 - scenario_AA$PA[2]), 3)))  # An estimated 8.2% of the pixels that are truly Forest were mapped as Non-forest.
#              8.8     v1_Valid_c2
#              8.1%   (v2_Valid_c2)
#              10%    (v4_Valid_c2)

print(paste0("Ommission Error (Non-Forest) = ",
             round((1 - scenario_AA$PA[1]), 3)))  # An estimated 8.6% of the pixels that are truly Non-forest were mapped as Forest.
#                                                                8.0%   v1_Valid_c2
#                                                                8.6%   (v2_Valid_c2)
#                                                                5.5%   (v4_Valid_c2)


## Conclusion:
## The estimated omission error for the Forest class was 8.2% (v2_Valid_c2: 8.1%), indicating that approximately 8.2% of the pixels that are truly 
## Forest were mapped as Non-forest. Conversely, the estimated commission error was 18.0% (v2_Valid_c2: 17.9%), indicating that approximately 18.0% 
## of the pixels mapped as Forest are estimated to actually be Non-forest.



## Standard error of OA
scenario_AA$SEoa    # 0.002220113    #v1_Valid_c2: 0.002207737     # (v2_Valid_c2: 0.002267183)       # (v4_Valid_c2: 0.002044676
round(((1.96*scenario_AA$SEoa)*100), 1)   # 0.4  (v2_Valid_c2: 0.4)  # (v4_Valid_c2: 0.4    

## Standard error of UA
scenario_AA$SEua
#           0             1 
# 0.001892472   0.005269710 
# 0.001942165 0.005253441 
# 0.001890410   0.005387148    (v2_Valid_c2)
# 0.001995071   0.004829038    (v4_Valid_c2)

## standard error of PA
scenario_AA$SEpa
#           0           1 
# 0.002605602   0.004073868 
# 0.002548335 0.004199084 
# 0.002690058   0.004066892    (v2_Valid_c2)
# 0.002176235   0.004429128    (v4_Valid_c2)

## Proportion of area 
scenario_AA$area
#         0         1 
# 0.7001962 0.2998038 
# 0.7001341 0.2998659 
# 0.7001341 0.2998659   (v2_Valid_c2)
# 0.7001341 0.2998659   (v4_Valid_c2)

## standard error of area proportion
scenario_AA$SEa
#           0           1 
# 0.003158123   0.003158123 
# 0.003123383   0.003123383    (v1_Valid_c2)
# 0.003123383   0.003123383    (v2_Valid_c2)
# 0.003123383   0.003123383    (v4_Valid_c2)


## Confusion error (area proportion). Rows and columns represent map and reference class labels, respectively
scenario_AA$matrix

#               0           1        sum
# 0    0.63978911  0.02453015  0.6643193
# 1    0.06040711  0.27527363  0.3356807
# sum  0.70019622  0.29980378  1.0000000


# 0   0.64412608 0.02635131 0.6704774     (v1_Valid_c2)
# 1   0.05600803 0.27351458 0.3295226
# sum 0.70013412 0.29986588 1.0000000


#
# 0    0.6401794   0.02413985  0.6643193      (v2_Valid_c2)
# 1    0.0599547   0.27572604  0.3356807
# sum  0.7001341   0.29986588  1.0000000

#              0          1       sum
# 0   0.66145750 0.03005093 0.6915084      (v4_Valid_c2)
# 1   0.03867662 0.26981495 0.3084916
# sum 0.70013412 0.29986588 1.0000000



## Confidence intervals:   CI95 = 1.96 × SE
z <- 1.96

CI95_UA <- z * scenario_AA$SEua * 100; CI95_UA
#          0           1 
#  0.3709245   1.0328633 
#  0.3806643 1.0296745 
#  0.3705203   1.0558810      (v2_Valid_c2)
#  0.3910339   0.9464915      (v4_Valid_c2)

CI95_PA <- z * scenario_AA$SEpa * 100; CI95_PA
#          0           1 
#  0.5106979   0.7984782 
#  0.4994736 0.8230205 
#  0.5272513   0.7971108      (v2_Valid_c2)
#  0.4265420   0.8681091      (v4_Valid_c2)

## Commission error (%)
commission <- (1 - scenario_AA$UA) * 100

## Omission error (%)
omission <- (1 - scenario_AA$PA) * 100



### Reporting table ####
v <- "GFC_v2_Valid_v3"
v <- "GFC_v4_Valid_c2"
v <- "GFC_v1_Valid_c2"
xlsx_fileName <- paste0("Table_ErrorMatrix_", v, ".xlsx")

report_error_matrix <- function(data,
                                map_col,
                                ref_col,
                                assessment,
                                output_dir,
                                sheet_name = NULL,
                                excel_file = xlsx_fileName) {
  
  ## Names
  
  dataset_name <- as.character(substitute(data))
  
  if (is.null(sheet_name)) {
    sheet_name <- paste0("Err_matr_", map_col, "_", dataset_name)
  }
  
  ## Raw confusion matrix
  
  raw <- addmargins(table(
    Map = factor(data[[map_col]],
                 levels = c(0, 1),
                 labels = c("Non-forest", "Forest")),
    Reference = factor(data[[ref_col]],
                       levels = c(0, 1),
                       labels = c("Non-forest", "Forest"))
  ))
  
  
  ## Cell proportions (%)
  
  #prop <- round(100 * raw / sum(raw[1:2, 1:2]), 1)
  prop <- raw
  prop_1 <- round(assessment$matrix, 3) * 100
  
  prop['Non-forest', 'Non-forest'] <- prop_1["0", "0"]
  prop['Non-forest', 'Forest'] <- prop_1["0", "1"]
  prop['Non-forest', 'Sum'] <- prop_1["0", "sum"]
  prop['Forest', 'Non-forest'] <- prop_1["1", "0"]
  prop['Forest', 'Forest'] <- prop_1["1", "1"]
  prop['Forest', 'Sum'] <- prop_1["1", "sum"]
  prop['Sum', 'Non-forest'] <- prop_1["sum", "0"]
  prop['Sum', 'Forest'] <- prop_1["sum", "1"]
  prop['Sum', 'Sum'] <- prop_1["sum", "sum"]
  
  
  
  ## Commission errors
  
  commission <- c(
    
    sprintf("%.1f (%.1f)",
            (1 - assessment$UA["0"]) * 100,
            1.96 * assessment$SEua["0"] * 100),
    
    sprintf("%.1f (%.1f)",
            (1 - assessment$UA["1"]) * 100,
            1.96 * assessment$SEua["1"] * 100),
    
    "",
    ""
    
  )
  
  ## Omission errors
  
  
  omission <- c(
    
    sprintf("%.1f (%.1f)",
            (1 - assessment$PA["0"]) * 100,
            1.96 * assessment$SEpa["0"] * 100),
    
    sprintf("%.1f (%.1f)",
            (1 - assessment$PA["1"]) * 100,
            1.96 * assessment$SEpa["1"] * 100),
    
    "",
    paste0("OA Acc. (CI95) [%] = ",
           sprintf("%.1f (%.1f)",
                   assessment$OA * 100,
                   1.96 * assessment$SEoa * 100))
    
  )
  
  ## Reporting table
  
  report_table <- data.frame(
    
    Class = c(
      "Non-forest",
      "Forest",
      "Total",
      "Omission (CI95) [%]"
    ),
    
    Raw_NF    = c(raw[1,1], raw[2,1], raw[3,1], ""),
    Raw_F     = c(raw[1,2], raw[2,2], raw[3,2], ""),
    Raw_Total = c(raw[1,3], raw[2,3], raw[3,3], ""),
    
    Prop_NF    = c(prop[1,1], prop[2,1], prop[3,1], omission[1]),
    Prop_F     = c(prop[1,2], prop[2,2], prop[3,2], omission[2]),
    Prop_Total = c(prop[1,3], prop[2,3], prop[3,3], ""),
    
    commission_ci95 = commission,
    check.names = FALSE
    
  )
  
  report_table[4, "commission_ci95"] <- omission[4]
  
  names(report_table)[8] <- "Commission (CI95) [%]"
  
  
  ## Excel
  
  excel_path <- file.path(output_dir, excel_file)
  
  if (file.exists(excel_path)) {
    
    wb <- openxlsx::loadWorkbook(excel_path)
    
    if (sheet_name %in% names(wb)) {
      removeWorksheet(wb, sheet_name)
    }
    
  } else {
    
    wb <- createWorkbook()
    
  }
  
  addWorksheet(wb, sheet_name)
  
  writeData(
    wb,
    sheet = sheet_name,
    x = report_table
  )
  
  saveWorkbook(
    wb,
    excel_path,
    overwrite = TRUE
  )
  
  ## Flextable
  
  ft <- flextable(report_table)
  
  ft <- bg(ft, bg = "white", part = "all")
  
  grey <- "#808080"
  
  border <- fp_border(color = grey, width = 1)
  
  ft <- border_remove(ft)
  ft <- border_outer(ft, border)
  ft <- border_inner_h(ft, border)
  ft <- border_inner_v(ft, border)
  
  ft <- add_header_row(
    ft,
    values = c(
      "",
      "Raw counts (Reference)",
      "Proportions [%] (Reference)",
      ""
    ),
    colwidths = c(1, 3, 3, 1)
  )
  
  ft <- bold(ft, part = "header")
  ft <- align(ft, align = "center", part = "header")
  ft <- bold(ft, j = 1, bold = TRUE, part = "body")
  ft <- align(ft, j = 2:8, align = "center", part = "body")
  ft <- valign(ft, valign = "center", part = "all")
  ft <- autofit(ft)
  ft <- bold(ft, i = 4, j = 8, bold = TRUE, part = "body")
  
  
  ## PNG
  
  save_as_image(
    ft,
    path = file.path(output_dir,
                     paste0(sheet_name, ".png"))
  )
  
  
  ## Return objects
  
  invisible(
    list(
      raw_table = raw,
      table = report_table,
      flextable = ft
    )
  )
  
}



## Run the function
#Valid_c2 <- map_ref_values   # change the name to write correct file/tab names
v1_Valid_c2 <- map_ref_values

res <- report_error_matrix(
  data       = v1_Valid_c2,             # change the name
  map_col    = "GFC2020",             
  ref_col    = "forest_class_num",
  assessment = scenario_AA,
  output_dir = dir_GFC_assessment_c2    
)

res$raw_table
res$table
res$flextable  ## Table 4 of the 2025 repoort



## MAPS ####

### Correctly and missclassified sample units (Figure 13) ####

head(scenario) ; nrow(scenario)
head(map_ref_values) ; nrow(map_ref_values)


map_ref_values <- map_ref_values %>%
  mutate(
    class = case_when(
      GFC2020 == 0 & forest_class_num == 0 ~ "No forest",
      GFC2020 == 1 & forest_class_num == 1 ~ "Forest",
      GFC2020 == 1 & forest_class_num == 0 ~ "Commission error",
      GFC2020 == 0 & forest_class_num == 1 ~ "Omission error"
    )
  )

head(map_ref_values)
unique(map_ref_values$class)

sum(is.na(map_ref_values$pixel_center_x))  # 0
sum(is.na(map_ref_values$pixel_center_y))  # 0


pts <- st_as_sf(map_ref_values, coords = c("pixel_center_x","pixel_center_y"), crs = 4326)
pts

world <- ne_countries(scale = "medium", returnclass = "sf")


# map
figure_title <- "GFC2020_v2 / Valid_v2"
figure_title <- "GFC2020_v4 / Valid_c2"
figure_title <- "GFC2020_v1 / Valid_c2"

p <- ggplot() +
  geom_sf(data = world, fill = "white", colour = "black", linewidth = 0.2) +
  geom_sf(data = filter(pts, class == "No forest"), aes(colour = "No forest"),
          size = 0.10,
          alpha = 0.25,
          show.legend = TRUE) +
  geom_sf(data = filter(pts, class == "Forest"), aes(colour = "Forest"),
          size = 0.10,
          alpha = 0.25,
          show.legend = TRUE) +
  geom_sf(data = filter(pts, class == "Commission error"), aes(colour = "Commission error"),
          size = 1.20,
          alpha = 0.95,
          show.legend = TRUE) +
  geom_sf(data = filter(pts, class == "Omission error"), aes(colour = "Omission error"),
          size = 1.20,
          alpha = 0.95,
          show.legend = TRUE) +
  scale_colour_manual(values = c("No forest" = "grey70", "Forest" = "#33A02C", "Commission error" = "#FDBF00", "Omission error" = "#1F78B4"),
                      breaks = c("No forest", "Forest", "Commission error", "Omission error")) +
  guides(colour = guide_legend(override.aes = list(size = c(2, 2, 4, 4), alpha = 1))) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  ggtitle(figure_title) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.90, face = "bold", size = 12, margin = margin(b = 10)),
        plot.title.position = "plot", legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 11))

p


# save figure
Fig13_Filename <- "Fig_ErrorDistribution_GFC2020_v2_Valid_c2.png"
Fig13_Filename <- "Fig_ErrorDistribution_GFC2020_v4_Valid_c2.png"
Fig13_Filename <- "Fig_ErrorDistribution_GFC2020_v1_Valid_c2.png"

#Fig13_dir <- dir_2ndAssessment
Fig13_dir <- dir_GFC_assessment_c2

save_it <- "yes"
save_it <- "no"
if(save_it == "yes"){
  ggsave(filename = file.path(Fig13_dir, Fig13_Filename), 
         plot = p, 
         width = 20, height = 12, units = "cm", dpi = 600, bg = "white")
}






## Sample units classified differently in GFC2020 V1 versus V2  --> a map (to be done!!)



## Forest area in GFC2020 (Table 6) ####

names(scenario_AA)
head(strata_area)


scenario_AA$area
# 0           1 
# 0.7001962   0.2998038 
# 0.7001341 0.2998659 
# 0.7001341   0.2998659     (v2_Valid_c2)
# 0.7001341   0.2998659     (v4_Valid_c2)

sum(scenario_AA$area)  # 1


total_area <- sum(strata_area$strata_ha)

forest_area <- scenario_AA$area["1"] * total_area
forest_area <- forest_area / 10^6    # Mha
forest_area   # 4021.075 Mha
# 4021.908 Mha   (v1_Valid_c2)
# 4021.908 Mha   (v2_Valid_c2)
# 4021.908 Mha   (v4_Valid_c2)    # 4021908385

scenario_AA$SEa
forest_se   <- scenario_AA$SEa["1"] * total_area
forest_ci95 <- 1.96 * forest_se
forest_ci95 <- round((forest_ci95 / 10^6), 1)
forest_ci95  # 83
# 82.1  (v1_Valid_c2)
# 82.1  (v2_Valid_c2)
# 82.1  (v4_Valid_c2)


## In the repor


#_________________ Below not done yet fo V1_C2 ________________________________________#

### Calculate total forest area (not adjusted) ####
#library(rgee)
#ee_install_upgrade()
#ee_Initialize()

## rgee doesn't work. We'll use 'reticulate'
library(reticulate)

# Use the dedicated Python environment
use_condaenv("rgee311", required = TRUE)

# Import the Earth Engine Python package
ee <- import("ee")

# Authenticate
ee$Authenticate()

# Initialize Earth Engine
ee$Initialize(project = "gfc2020-503311")


## GEE is initialised, now we can run the calculations


## Forest area
## Clement's GEE script: https://code.earthengine.google.com/7cfb76f43e06c22856311f81662552da

run_this <- "yes"
run_this <- "no"

if(run_this == "yes"){
  
  py_run_string("
import ee

# ---------------------------------------------------------------------
# GFC v1
# ---------------------------------------------------------------------

# Mosaic the collection into a single image (remove overlapping)
#GFCv3 = ee.ImageCollection('JRC/GFC2020/V3').mosaic()
#GFCv3 = ee.Image('JRC/GFC2020/V3')
GFCv1 = ee.ImageCollection('JRC/GFC2020/V1').mosaic()

# Spatial zones to iterate over
Zones = ee.FeatureCollection(
    'projects/ee-astridverhegghen/assets/EUFO/continents/gaul2015_GFC_1deg-final'
)

# ---------------------------------------------------------------------
# Forest layer
# ---------------------------------------------------------------------

# Keep only value 1 (forest), rename the band.
# ee.Image.cat() is kept for fidelity with the original script.
AllClasses = ee.Image.cat(
    GFCv1.eq(1).rename('Forest_v1')
)

# ---------------------------------------------------------------------
# Function applied to every feature
# ---------------------------------------------------------------------

def LOOPsamples(feature):

    vals = (
        AllClasses
        .multiply(ee.Image.pixelArea())
        .reduceRegion(
            reducer = ee.Reducer.sum(),
            geometry = feature.geometry(),
            scale = 10,
            maxPixels = 1e13
        )
    )

    return (
        ee.Feature(None, vals)
        .copyProperties(feature, feature.propertyNames())
    )

# Apply the function to every feature
LOOPresult2 = Zones.map(LOOPsamples)

# ---------------------------------------------------------------------
# Export
# ---------------------------------------------------------------------

task = ee.batch.Export.table.toDrive(
    collection = LOOPresult2,
    description = 'JRC_GFC_v1_gaul2015_GFC_1deg-final',
    folder = 'EarthEngine',
    fileNamePrefix = 'JRC_GFC_v1_gaul2015_GFC_1deg-final',
    fileFormat = 'CSV'
)

task.start()

print('Task submitted. Check your submitted tasks in https://code.earthengine.google.com/')
")


}


## Results after running GEE script (from this R session)
library(googledrive)

drive_ls("EarthEngine")
2
file <- drive_find("JRC_GFC_v1_gaul2015_GFC_1deg-final.csv")
drive_download(
  file,
  path = paste0(dir_GFC_assessment_c2, "/JRC_GFC_v1_gaul2015_GFC_1deg-final.csv"),
  overwrite = TRUE
)

areas <- read.csv(paste0(dir_GFC_assessment_c2, "/JRC_GFC_v1_gaul2015_GFC_1deg-final.csv"))
head(areas)
names(areas)
nrow(areas)  # 28452
unique(areas$FID_gaul20) %>% length  # 25668


unique(areas$Forest_v1)
sum(is.na(areas$Forest_v1)) # 0 NA's




mapped_area <- round(sum(areas$Forest_v4)/10^10, 0)    # m2 to Mha    # v4_c2: 4161 Mha
sum(areas$Forest_v4)/10^10    # m2 to Mha    # 4561.726       ; old: 4561.726      # v4_c2: 4161.457 Mha
sum(areas$Forest_v4)          # m2           # 4.561726e+13   ; old: 4.561726e+13   # v4_c2:
format(sum(areas$Forest_v4), scientific = FALSE)  # 45617263160131  m2   ;  old: 45617263160131   # v4_c2: 41614568537094 ha

mapped_area  # this is exactly the number reported in the first assessment. (4562 Mha)   # v4_c2: 4161 Mha



## Check Vietnam
head(areas)
sort(unique(areas$ADM0_NAME))

length(unique(areas$ADM0_NAME))
length(unique(areas$FAO_CODE))



 areas %>% 
  filter(ADM0_NAME == "Viet Nam") %>% #head
  #nrow()  # 64
  #pull(FID_gaul20) %>% unique %>% length  # 64
  pull(FAO_CODE) %>% unique  # 237

 areas %>% 
   filter(FAO_CODE == 237) %>% 
   nrow  # 64


forest_area_VietNam <- areas %>% 
  filter(ADM0_NAME == "Viet Nam") %>%
  pull(Forest_v1) %>% sum/10^7    # m2 to 1000ha  

forest_area_VietNam   # (v4_c2): 15280.58

# should do the same for GFT to get Primary / Planted areas

































