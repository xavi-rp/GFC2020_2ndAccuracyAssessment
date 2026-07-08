
#__________________________________________________________________________________________#
#__________________________________________________________________________________________#
#___                                                                                   ____#
#___  Script based on the script used for the 1st assessment, and shared in:           ____#
#___  https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/FOREST/EUFO/GFC2020_validation/ ____#
#___                                                                                   ____#
#__________________________________________________________________________________________#
#__________________________________________________________________________________________#

## The main objective of this script is to reproduce the 1st assessment


## Setup ####

## libraries
library(mapaccuracy)
library(tidyverse)
library(openxlsx)
library(flextable)
library(officer)


# set a working directory
#setwd("/validation")

dir_assessment2024 <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_first/GFC2020_validation/"

dir_2ndAssessment <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_Second/2nd_Assessment/"


## Loading datasets ####

# load the the file with the strata names and count
strata_area <- read.csv(paste0(dir_assessment2024, "strata_size.csv"))

head(strata_area)
nrow(strata_area)   # 149
names(strata_area)   # 'Strata',  'strata_ha'
sort(unique(strata_area$Strata))   # e.g. 1001, etc
length(unique(strata_area$Strata))  # 149 strata, from 1001 to 8226


# load the files with the sample units interpretation and the map values
scenario <- read.csv(paste0(dir_assessment2024, "combined_scenario_all_GFCV2.csv"), sep = ",", na.strings = c("NA", "N/A"), stringsAsFactors = FALSE)

head(scenario)
names(scenario)   # "pixel_center_x"   "pixel_center_y"   "sample_id"  "forest_class_num"   "strata"   "GFC_v2"    "gaul"
nrow(scenario)   # 21752

apply(scenario, 2, function(x) sum(is.na(x)))   # no NAs

sort(unique(scenario$forest_class_num))
sort(table(scenario$forest_class_num))
#   100     1       0 
#    24  6598   15130       # 24 samples with forest_class_num = 100 are those not assigned in 2024; not included in the assessment

table(scenario$strata)      # 64 samples with strata = 0 (no strata); not included in the assessment
table(scenario$gaul)        # 59 smaples with gaul = 0 (no gaul); not included in the assessment. However, keep in mind that 5 of these are 
#    0       1              # already not included because they do not have strata; therefore 54 additional samples not included
#   59   21693 


table(scenario$GFC_v2)      
#     0       1 
# 14386    7366  




## Strata info ####

# Nh_strata numeric vector. Number of pixels forming each stratum. It must be named (see details)
Nh_strata <- strata_area$strata_ha
names(Nh_strata) <- strata_area$Strata

Nh_strata




## Sample units interpretation and map values ####
dim(scenario)  # 21752, 7
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

df_dropped_3 <- df_dropped_2 %>% 
  filter(gaul != "0")

dim(df_dropped_3)  # 21612, 7

# clean dataset
map_ref_values <- df_dropped_3
head(map_ref_values)
nrow(map_ref_values)  # 21612




## Analysis ####

#??stehman2014


# s character vector. Strata class labels. The object will be coerced to factor.
s <- map_ref_values$strata
sum(is.na(s)) # 0


# r character vector. Reference class labels. The object will be coerced to factor.
r <- map_ref_values$forest_class_num


# m character vector. Map class labels. The object will be coerced to factor.
m <- map_ref_values$GFC_v2


# need to indicate the order for the labels of the matrix (0 non forest, 1 forest)
order <- c(0,1)
scenario_AA <- stehman2014(s, r, m, Nh_strata, margins = TRUE, order)

scenario_AA


### Results ####

## Overall accuracy
scenario_AA$OA   # 0.9150627


## User's accuracy of the two classes. Related to the Commission Error (False positives)
scenario_AA$UA

#    0 (Non-For)         1  (For)
# 0.9630748            0.8200459 

1 - scenario_AA$UA#[2]
#           0            1 
#  0.03692525   0.17995405     # Coommission Error = 0.17995405
                               # The estimated commission error for the Forest class was 18.0%, indicating that approximately 18%
                               # of the area mapped as Forest is estimated to correspond to Non-forest in the reference data.

print(paste0("Commission Error (Forest class) = ",
             round((1 - scenario_AA$UA[2]), 3)))  

print(paste0("Commission Error (Non-Forest class) = ",
             round((1 - scenario_AA$UA[1]), 3)))        # 0.037 (about 3.7% of the area (or pixels) mapped as Non-forest is 
                                                        # estimated to actually be Forest.)


# | Class      | User's Accuracy | Commission Error | Meaning                                              |
# | ---------- | --------------: | ---------------: | ---------------------------------------------------- |
# | Forest     |           82.0% |            18.0% | 18% of mapped Forest pixels are false positives      |
# | Non-forest |           96.3% |             3.7% | 3.7% of mapped Non-forest pixels are false positives |
  




## Producer's accuracy. Related to Omission Error (False Negatives)
scenario_AA$PA

#         0         1 
# 0.9137283   0.9181793 


print(paste0("Ommission Error (Forest) = ",
             round((1 - scenario_AA$PA[2]), 3)))  # An estimated 8.2% of the pixels that are truly Forest were mapped as Non-forest.

print(paste0("Ommission Error (Non-Forest) = ",
             round((1 - scenario_AA$PA[1]), 3)))  # An estimated 8.6% of the pixels that are truly Non-forest were mapped as Forest.


## Conclusion:
## The estimated omission error for the Forest class was 8.2%, indicating that approximately 8.2% of the pixels that are truly 
## Forest were mapped as Non-forest. Conversely, the estimated commission error was 18.0%, indicating that approximately 18.0% 
## of the pixels mapped as Forest are estimated to actually be Non-forest.

## Standard error of OA
scenario_AA$SEoa    # 0.002220113

## Standard error of UA
scenario_AA$SEua
#           0             1 
# 0.001892472   0.005269710 

## standard error of PA
scenario_AA$SEpa
#           0           1 
# 0.002605602   0.004073868 

## Proportion of area 
scenario_AA$area
#         0         1 
# 0.7001962 0.2998038 

## standard error of area proportion
scenario_AA$SEa
#           0           1 
# 0.003158123   0.003158123 

## Confusion error (area proportion). Rows and columns represent map and reference class labels, respectively
scenario_AA$matrix

#               0           1        sum
# 0    0.63978911  0.02453015  0.6643193
# 1    0.06040711  0.27527363  0.3356807
# sum  0.70019622  0.29980378  1.0000000


## Confidence intervals:   CI95 = 1.96 × SE
z <- 1.96

CI95_UA <- z * scenario_AA$SEua * 100; CI95_UA
#          0           1 
#  0.3709245   1.0328633 

CI95_PA <- z * scenario_AA$SEpa * 100; CI95_PA
#          0           1 
#  0.5106979   0.7984782 


## Commission error (%)
commission <- (1 - scenario_AA$UA) * 100

## Omission error (%)
omission <- (1 - scenario_AA$PA) * 100



### Reporting table ####

report_error_matrix <- function(data,
                                map_col,
                                ref_col,
                                assessment,
                                output_dir,
                                sheet_name = NULL,
                                excel_file = "Table_ErrorMatrix.xlsx") {
  
  ##------------------------------------------------------------
  ## Names
  ##------------------------------------------------------------
  
  dataset_name <- as.character(substitute(data))
  
  if (is.null(sheet_name)) {
    sheet_name <- paste0("Err_matr_", map_col, "_", dataset_name)
  }
  
  ##------------------------------------------------------------
  ## Raw confusion matrix
  ##------------------------------------------------------------
  
  raw <- addmargins(table(
    Map = factor(data[[map_col]],
                 levels = c(0, 1),
                 labels = c("Non-forest", "Forest")),
    Reference = factor(data[[ref_col]],
                       levels = c(0, 1),
                       labels = c("Non-forest", "Forest"))
  ))
  
  ##------------------------------------------------------------
  ## Cell proportions (%)
  ##------------------------------------------------------------
  
  prop <- round(100 * raw / sum(raw[1:2, 1:2]), 1)
  
  ##------------------------------------------------------------
  ## Commission errors
  ##------------------------------------------------------------
  
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
  
  ##------------------------------------------------------------
  ## Omission errors
  ##------------------------------------------------------------
  
  omission <- c(
    
    sprintf("%.1f (%.1f)",
            (1 - assessment$PA["0"]) * 100,
            1.96 * assessment$SEpa["0"] * 100),
    
    sprintf("%.1f (%.1f)",
            (1 - assessment$PA["1"]) * 100,
            1.96 * assessment$SEpa["1"] * 100),
    
    "",
    
    sprintf("%.1f (%.1f)",
            assessment$OA * 100,
            1.96 * assessment$SEoa * 100)
    
  )
  
  ##------------------------------------------------------------
  ## Reporting table
  ##------------------------------------------------------------
  
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
  
  ##------------------------------------------------------------
  ## Excel
  ##------------------------------------------------------------
  
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
  
  ##------------------------------------------------------------
  ## Flextable
  ##------------------------------------------------------------
  
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
  ft <- align(ft, j = 2:8, align = "center", part = "body")
  ft <- valign(ft, valign = "center", part = "all")
  ft <- autofit(ft)
  
  ##------------------------------------------------------------
  ## PNG
  ##------------------------------------------------------------
  
  save_as_image(
    ft,
    path = file.path(output_dir,
                     paste0(sheet_name, ".png"))
  )
  
  ##------------------------------------------------------------
  ## Return objects
  ##------------------------------------------------------------
  
  invisible(
    list(
      raw_table = raw,
      table = report_table,
      flextable = ft
    )
  )
  
}



## Run the function
Valid_v2 <- map_ref_values   # change the name to write correct file/tab names

res <- report_error_matrix(
  data       = Valid_v2,
  map_col    = "GFC_v2",
  ref_col    = "forest_class_num",
  assessment = scenario_AA,
  output_dir = dir_2ndAssessment
)

res$raw_table
res$table
res$flextable

