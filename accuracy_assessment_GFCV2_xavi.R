
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
library(sf)
library(rnaturalearth)



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

# Nh_strata numeric vector. Number of pixels forming each stratum (it's actually the area in ha). It must be named (see details)
head(strata_area)

Nh_strata <- strata_area$strata_ha
names(Nh_strata) <- strata_area$Strata

Nh_strata

head(Nh_strata)
sum(Nh_strata)



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



# r character vector. Reference class labels (validation dataset). The object will be coerced to factor.
r <- map_ref_values$forest_class_num


# m character vector. Map class labels (results of the model: GFC2020_vX -- v2 in this case). The object will be coerced to factor.
m <- map_ref_values$GFC_v2


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
scenario_AA$OA   # 0.9150627
round(scenario_AA$OA, 3)   # 0.915


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
round(((1.96*scenario_AA$SEoa)*100), 1)   # 0.4

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

  prop <- round(100 * raw / sum(raw[1:2, 1:2]), 1)
  

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
res$flextable  ## Table 4 of the 2025 repoort





## MAPS ####

### Correctly and missclassified sample units (Figure 13) ####

head(scenario) ; nrow(scenario)
head(map_ref_values) ; nrow(map_ref_values)


map_ref_values <- map_ref_values %>%
  mutate(
    class = case_when(
      GFC_v2 == 0 & forest_class_num == 0 ~ "No forest",
      GFC_v2 == 1 & forest_class_num == 1 ~ "Forest",
      GFC_v2 == 1 & forest_class_num == 0 ~ "Commission error",
      GFC_v2 == 0 & forest_class_num == 1 ~ "Omission error"
    )
  )

head(map_ref_values)
unique(map_ref_values$class)

sum(is.na(map_ref_values$pixel_center_x))  # 0
sum(is.na(map_ref_values$pixel_center_y))  # 0


pts <- st_as_sf(
  map_ref_values,
  coords = c("pixel_center_x","pixel_center_y"),
  crs = 4326)

pts


world <- ne_countries(scale = "medium", returnclass = "sf")


# map
figure_title <- "GFC2020_v2 / Valid_v2"

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

ggsave(filename = file.path(dir_2ndAssessment, "Fig_ErrorDistribution_GFC2020_v2_Valid_v2.png"), 
       plot = p, 
       width = 20, height = 12, units = "cm", dpi = 600, bg = "white")





## Sample units classified differently in GFC2020 V1 versus V2  --> a map (to be done!!)




## Forest area in GFC2020 (Table 6) ####

names(scenario_AA)
head(strata_area)


scenario_AA$area
# 0           1 
# 0.7001962   0.2998038 

sum(scenario_AA$area)  # 1


total_area <- sum(strata_area$strata_ha)

forest_area <- scenario_AA$area["1"] * total_area
forest_area <- forest_area / 10^6    # Mha
forest_area   # 4021.075 Mha

scenario_AA$SEa
forest_se   <- scenario_AA$SEa["1"] * total_area
forest_ci95 <- 1.96 * forest_se
forest_ci95 <- round((forest_ci95 / 10^6), 1)
forest_ci95  # 83

## In the repor



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

#gaul <- ee$FeatureCollection("FAO/GAUL/2015/level0")
#ee$data$getAsset("JRC/GFC2020/V2")   # image collection
#gfc <- ee$ImageCollection("JRC/GFC2020/V2")
#gfc$size()$getInfo()  # 1420 images (very likely one image per tile)
#gfc_img$bandNames()$getInfo()  # Each image has a single band called "Map". THerefore, first() would give us only the first tile

# The collection is organized as:
# Geographic tiles (N0_E0, N0_E10, N0_E100, ...)
# Each geographic tile is further split into four internal chunks (0000000000-0000000000, etc.)
# The tiles overlap, so mosaic() removes overlapping


## Forest area
## Clement's GEE script: https://code.earthengine.google.com/7cfb76f43e06c22856311f81662552da

run_this <- "no"

if(run_this == "yes"){
  
  py_run_string("
import ee

# ---------------------------------------------------------------------
# GFC v2
# ---------------------------------------------------------------------

# Mosaic the collection into a single image (remove overlapping)
GFCv2 = ee.ImageCollection('JRC/GFC2020/V2').mosaic()

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
    GFCv2.eq(1).rename('Forest_v2')
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
    description = 'JRC_GFC_v2_gaul2015_GFC_1deg-final',
    folder = 'EarthEngine',
    fileNamePrefix = 'JRC_GFC_v2_gaul2015_GFC_1deg-final',
    fileFormat = 'CSV'
)

task.start()

print('Task submitted.')
")

  
}





## Results after running Clement's GEE script (from this R session)
library(googledrive)

drive_ls("EarthEngine")
file <- drive_find("JRC_GFC_v2_gaul2015_GFC_1deg-final.csv")
drive_download(
  file,
  path = paste0(dir_2ndAssessment, "JRC_GFC_v2_gaul2015_GFC_1deg-final.csv"),
  overwrite = TRUE
)

areas <- read.csv(paste0(dir_2ndAssessment, "JRC_GFC_v2_gaul2015_GFC_1deg-final.csv"))
#areas <- read.csv(paste0(dir_2ndAssessment, "JRC_GFC_v2_gaul2015_GFC_1deg-final_old.csv"))
head(areas)
names(areas)
unique(areas$Forest_v2)

sum(is.na(areas$Forest_v2)) # 0 NA's


mapped_area <- round(sum(areas$Forest_v2)/10^10, 0)    # m2 to Mha
sum(areas$Forest_v2)/10^10    # m2 to Mha    # 4561.726       ; old: 4561.726
sum(areas$Forest_v2)          # m2           # 4.561726e+13   ; old: 4.561726e+13
format(sum(areas$Forest_v2), scientific = FALSE)  # 45617263160131  m2   ;  old: 45617263160131

mapped_area  # this is exactly the number reported in the first assessment. (4562 Mha)






### Table 6 reproduction ####

fra_area <- 4058

report_table6 <- data.frame(
  Metric = "Forest area (Mha)",
  `GFC2020 V2` = sprintf("%.1f", mapped_area),
  `Reference set (95% CI)` = sprintf("%.1f (±%.1f)",
                                      forest_area,
                                      forest_ci95),
  `FAO-FRA 2020` = sprintf("%.0f", fra_area),
  check.names = FALSE
)

report_table6


ft <- flextable(report_table6)

ft <- theme_booktabs(ft)

ft <- bold(ft, part = "header")
ft <- bold(ft, j = 1)

ft <- align(ft, align = "center", part = "header")
ft <- align(ft, j = 1, align = "left", part = "body")
ft <- align(ft, j = 2:4, align = "right", part = "body")

ft <- border_outer(
  ft,
  border = fp_border(color = "black", width = 1)
)

ft <- border_inner_h(
  ft,
  border = fp_border(color = "grey70", width = 0.5)
)

ft <- border_inner_v(
  ft,
  border = fp_border(color = "grey70", width = 0.5)
)

ft <- bg(ft, bg = "white", part = "all")

ft <- autofit(ft)

ft


# to excel

wb <- createWorkbook()

addWorksheet(wb, "Forest_area_GFC_v2_Valid_v2")

writeData(wb,
          sheet = "Forest_area_GFC_v2_Valid_v2",
          report_table6)

saveWorkbook(wb, file = file.path(dir_2ndAssessment, "Forest_area_report.xlsx"), overwrite = TRUE)


# to png

save_as_image(ft, path = file.path(dir_2ndAssessment, "Forest_area_GFC_v2_Valid_v2.png"))




## Figure 15  --> a map (to be done!!)  ########
## Forest area proportion in each 1x1 degree cell for GFC2020 V2. (Figure 15)  --> a map (to be done!!)
##__________

areas <- read.csv(paste0(dir_2ndAssessment, "JRC_GFC_v2_gaul2015_GFC_1deg-final.csv"))
head(areas, 1)
nrow(areas)  # 28452
names(areas)  
areas[2000, ]

areas %>%
  group_by(PageNumber) %>%
  summarise(
    forest = sum(Forest_v2),
    land   = sum(LandArea)
  ) %>% View()


areas %>%
  summarise(
    n_rows = n(),
    n_pages = n_distinct(PageNumber)
  )

areas %>%
  count(PageNumber) %>%
  arrange(desc(n))

head(areas$PageName, 10)

## Look at https://code.earthengine.google.com/6b90dc887f94adb09ac4d237a678319a
## The geometry follows the coastline, and it is not a full 1° square. It is the land portion of the 1° cell for that country.

## Now, let's compute the forest area and the land area for each feature, as for Table 6 above

run_this <- "yes"
run_this <- "no"

if(run_this == "yes"){
  
  py_run_string("

# ------------------------------------------------------------
# Load GFC2020 V2
# ------------------------------------------------------------

GFCv2 = ee.ImageCollection('JRC/GFC2020/V2').mosaic()

# ------------------------------------------------------------
# Load 1° GAUL grid
# ------------------------------------------------------------

Zones = ee.FeatureCollection(
    'projects/ee-astridverhegghen/assets/EUFO/continents/gaul2015_GFC_1deg-final'
)

# ------------------------------------------------------------
# Forest mask
# ------------------------------------------------------------

Forest = GFCv2.eq(1)

# ------------------------------------------------------------
# Compute forest and land area
# ------------------------------------------------------------

def computeAreas(feature):

    forestArea = (
        Forest
        .multiply(ee.Image.pixelArea())
        .reduceRegion(
            reducer = ee.Reducer.sum(),
            geometry = feature.geometry(),
            scale = 10,
            maxPixels = 1e13
        )
        .get('Map')
    )

    return ee.Feature(None, {
        'system:index': feature.get('system:index'),
        'PageNumber': feature.get('PageNumber'),
        'PageName': feature.get('PageName'),
        'ForestArea': forestArea,
        'LandArea': feature.geometry().area()
    })

result = Zones.map(computeAreas)

# ------------------------------------------------------------
# Export to Drive
# ------------------------------------------------------------

task = ee.batch.Export.table.toDrive(
    collection=result,
    description='Figure15_ForestArea_1deg',
    folder='EarthEngine',
    fileNamePrefix='Figure15_ForestArea_1deg',
    fileFormat='CSV'
)

task.start()

print('Export started.')
")

  
}  # end of run_this





drive_ls("EarthEngine")
file <- drive_find("Figure15_ForestArea_1deg.csv")
drive_download(
  file,
  path = paste0(dir_2ndAssessment, "Figure15_ForestArea_1deg.csv"),
  overwrite = TRUE
)

areas <- read.csv(paste0(dir_2ndAssessment, "Figure15_ForestArea_1deg.csv"))

figure15 <- areas %>%
  group_by(PageNumber, PageName) %>%
  summarise(
    ForestArea = sum(ForestArea, na.rm = TRUE),
    LandArea   = sum(LandArea, na.rm = TRUE),
    ForestProp = ForestArea / LandArea,
    .groups = "drop"
  )

head(figure15)
nrow(figure15)



## Spatial layer:

do_checks <- "no"

if(do_checks == "yes"){
  py_run_string("

Zones = ee.FeatureCollection(
    'projects/ee-astridverhegghen/assets/EUFO/continents/gaul2015_GFC_1deg-final'
)

def addType(f):
    return f.set('geomType', f.geometry().type())

types = Zones.map(addType)

print(types.aggregate_histogram('geomType').getInfo())
")

# {'GeometryCollection': 248, 'LineString': 9, 'MultiPolygon': 5376, 'Polygon': 22819}


# First 'GeometryCollection'?

py_run_string("
import ee
import json

Zones = ee.FeatureCollection(
    'projects/ee-astridverhegghen/assets/EUFO/continents/gaul2015_GFC_1deg-final'
)

def addType(f):
    return f.set('geomType', f.geometry().type())

gcs = Zones.map(addType).filter(
    ee.Filter.eq('geomType', 'GeometryCollection')
)

geom = gcs.first().geometry().getInfo()

print(json.dumps(geom, indent=2))
")
# 1 polygon plus 2 lines. We extract the polygon and remove the lines, then export to SHP
  
}  # end of do_checks


## Export the layer and fix it in R

do_export <- "no"

if(do_export == "yes"){
  py_run_string("

Zones = ee.FeatureCollection(
  'projects/ee-astridverhegghen/assets/EUFO/continents/gaul2015_GFC_1deg-final'
)

task = ee.batch.Export.table.toDrive(
  collection=Zones,
  description='gaul2015_GFC_1deg_GeoJSON',
  folder='EarthEngine',
  fileNamePrefix='gaul2015_GFC_1deg',
  fileFormat='GeoJSON'
)

task.start()

print('Export started.')
")

} # end of do_export


file <- drive_find("gaul2015_GFC_1deg.geojson")
drive_download(
  file,
  path = paste0(dir_2ndAssessment, "gaul2015_GFC_1deg.geojson"),
  overwrite = TRUE
)


grid <- st_read(paste0(dir_2ndAssessment, "gaul2015_GFC_1deg.geojson"))
grid

head(grid)
grid %>% filter(PageName == "BU1")
grid %>% filter(PageName == "E1")
grid %>% filter(PageName == "E359")

table(st_geometry_type(grid))


## Cleaning 'geometrycollection' and produce the final grid

do_the_grid <- "yes"
do_the_grid <- "no"

if(do_the_grid == "yes"){
  
  # Keep normal polygons
  poly <- grid %>%
    filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
  
  # Extract polygon parts from GeometryCollections
  gc_poly <- grid %>%
    filter(st_geometry_type(.) == "GEOMETRYCOLLECTION") %>%
    st_collection_extract("POLYGON") %>% 
    st_cast("MULTIPOLYGON")
  
  # Merge them
  grid_clean <- bind_rows(poly, gc_poly)
  
  table(st_geometry_type(grid_clean))
  nrow(grid_clean)
  
  
  ## Disolving features
  grid_1deg <- grid_clean %>%
    group_by(PageNumber, PageName) %>%
    summarise(do_union = TRUE)
  
  nrow(grid_1deg)  # 25668, correct!
  head(grid_1deg)  # 2 fiels (PageNumber and PageName) + geometry
  
  ## saving it
  st_write(
    grid_1deg,
    paste0(dir_2ndAssessment, "grid_1deg.gpkg"),
    delete_dsn = TRUE
  )
  
} # end of do_the_grid



grid_1deg <- st_read(paste0(dir_2ndAssessment, "grid_1deg.gpkg"))
grid_1deg
nrow(grid_1deg) # 25668

names(figure15)
nrow(figure15) # 25668

kk <- grid_1deg %>% filter(PageName == "BU1")   # stripe
kk <- grid_1deg %>% filter(PageName == "BU360") 
kk
plot(st_geometry(kk), col = "blue", border = NA)


figure15_sf <- grid_1deg %>%
  left_join(figure15,
            by = c("PageNumber", "PageName"))



head(figure15_sf)
nrow(figure15_sf)
colnames(figure15_sf)

sum(is.na(figure15_sf$ForestProp)) # No NAs

summary(figure15_sf$ForestProp)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000000 0.000000 0.002898 0.236439 0.481433 1.001876 


#figure15_sf <- figure15_sf %>%
#  mutate(
#    ForestClass = cut(
#      ForestProp,
#      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
#      include.lowest = TRUE
#    )
#  )

head(figure15_sf)
unique(figure15_sf$ForestClass)
sum(is.na(figure15_sf$ForestClass))


#plotting Fig 15
library(viridis)

p15 <- ggplot(figure15_sf) +
  #geom_sf(aes(fill = ForestProp), colour = NA) +  # colour = NA avoids drawing borders around the 1° cells
  geom_sf(aes(fill = ForestProp), colour = "transparent", linewidth = 0.01) +
    scale_fill_viridis_c(
    option = "D",
    direction = -1,
    limits = c(0, 1),  # fixes the scale to the theoretical range.
    oob = scales::squish,   # maps the 2 values slightly above 1 to exactly 1, instead of dropping them
    name = "Forest\nproportion"
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "right"
  )


ggsave(
  filename = file.path(dir_2ndAssessment,
                       "Fig15_ForestAreaProportion_GFC2020_v2_Valid_v2.png"),
  plot = p15,
  device = ragg::agg_png,
  width = 20,
  height = 12,
  units = "cm",
  dpi = 600,
  bg = "white"
)



figure_title <- "Forest area proportion in each 1° × 1° cell"

#






##  Figure 16  --> a map (to be done!!) ########
## Net relative difference in forest area estimates between GFC2020 V1 and V2 at 1 degree grid cells  (Figure 16)   --> a map (to be done!!)
##__________





##  Assessment of forest and land use types --> bar charts and maps (to be done!!) ########
## 
##__________







## Regional Assessment ####
dir_assessment2024
scenario


dir_2ndAssessment

list.files(dir_assessment2024)
list.files("/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_first")

GFC_v2_gaul <- readxl::read_excel("/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_first/GFC_v2_accuracy-assessment_gaul.xlsx", sheet = "5_combined")

names(GFC_v2_gaul)
nrow(GFC_v2_gaul)  # 21612, only those for the assessment

sort(unique(GFC_v2_gaul$continent))
sort(unique(GFC_v2_gaul$continent_gaul))

#






