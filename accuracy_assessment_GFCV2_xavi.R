
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

# Initialize Earth Engine
ee$Initialize()

## GEE is initialised, now we can run the calculations

gaul <- ee$FeatureCollection("FAO/GAUL/2015/level0")



ee$data$getAsset("JRC/GFC2020/V2")   # image collection

gfc <- ee$ImageCollection("JRC/GFC2020/V2")
gfc_img <- gfc$first()

gfc$size()$getInfo()  # 1420 images (very likely one image per tile)
gfc_img$bandNames()$getInfo()  # Each image has a single band called "Map". THerefore, first() would give us only the first tile

gfc <- ee$ImageCollection("JRC/GFC2020/V2")$
  select("Map")$
  mosaic()

gfc$bandNames()$getInfo()   # "Map"


# Native pixel size
scale <- gfc$
  projection()$
  nominalScale()$
  getInfo()

scale  # 111319.5  111319.5 m is not the native resolution of the data. It's approximately 1 degree at the equator (111,319.5 m).

gfc_ic <- ee$ImageCollection("JRC/GFC2020/V2")

tile <- ee$Image(gfc_ic$first())

tile$projection()$getInfo()

scale <- tile$projection()$
  nominalScale()$
  getInfo()

scale  # now, scale is 10 m

gfc_ic$first()$getInfo()

gfc_ic$
  aggregate_array("system:index")$
  getInfo()[1:20]

# The collection is organized as:
# Geographic tiles (N0_E0, N0_E10, N0_E100, ...)
# Each geographic tile is further split into four internal chunks (0000000000-0000000000, etc.)
# The tiles are non-overlapping, so mosaic() is exactly the right operation.


# create again the mosaic, in a clean way:
gfc_ic <- ee$ImageCollection("JRC/GFC2020/V2")$
  select("Map")

# first tile to retrieve the native projection
tile <- ee$Image(gfc_ic$first())

proj <- tile$projection()

scale <- proj$nominalScale()$getInfo()
scale # 10m

# Building the global image and forcing the native projection
gfc <- gfc_ic$
  mosaic()$
  setDefaultProjection(proj)

gfc$
  projection()$
  nominalScale()$
  getInfo()  # 10


## Forest area
# avoid using the complex GAUL geometry. Building a global rectangle:
  
world <- ee$Geometry$Rectangle(
  coords = c(-180, -90, 180, 90),
  geodesic = FALSE)


forest_area <- ee$Image$pixelArea()$
  divide(10000)$
  updateMask(gfc$eq(1))$
  reduceRegion(
    reducer = ee$Reducer$sum(),
    geometry = world,
    scale = scale,
    maxPixels = 1e13
  )$
  getInfo()  # exceeds the interactive timeout (typically a few minutes) --> error



## Another approach: compute the area tile by tile and then sum the results.

# img is an ee.Image corresponding to one tile

#tile_forest_area <- function(img, scale = 10) {
#  
#  area <- ee$Image$pixelArea()$
#    divide(10000)$
#    updateMask(img$select("Map")$eq(1))$
#    reduceRegion(
#      reducer = ee$Reducer$sum(),
#      geometry = img$geometry(),
#      scale = scale,
#      maxPixels = 1e13
#    )$
#    getInfo()
#  
#  return(area$area)
#}
#
#
#img1 <- ee$Image(gfc_ic$first())
#area1 <- tile_forest_area(img1) ; print(area1) # 0
#img1$get("system:index")$getInfo() # tiny island on the Atlantic
#
#ids <- gfc_ic$
#  limit(20)$
#  aggregate_array("system:index")$
#  getInfo()
#print(ids)
#
#
#img <- ee$Image(
#  gfc_ic$
#    filter(ee$Filter$stringContains("system:index", "S10_W70"))$
#    first()
#)
#
#img$
#  reduceRegion(
#    reducer = ee$Reducer$frequencyHistogram(),
#    geometry = img$geometry(),
#    scale = 10,
#    maxPixels = 1e13
#  )$
#  getInfo()
#
#
#res <- ee$Image$pixelArea()$
#  divide(10000)$
#  updateMask(img$select("Map")$eq(1))$
#  reduceRegion(
#    reducer = ee$Reducer$sum(),
#    geometry = img$geometry(),
#    scale = 10,
#    maxPixels = 1e13
#  )$
#  getInfo()
#
#str(res)
#print(res)
#
#res <- img$
#  select("Map")$
#  eq(1)$
#  reduceRegion(
#    reducer = ee$Reducer$sum(),
#    geometry = img$geometry(),
#    scale = 10,
#    maxPixels = 1e13
#  )$
#  getInfo()
#
#print(res)
#
## Reducer.sum() on a binary integer image is overflowing (sum is negative), while summing pixelArea() works #because it's a floating-point image.
#
#img$bandNames()$getInfo()
#img$projection()$nominalScale()$getInfo()


### This works but it's far too slow
run_this <- "no"

if(run_this != "no"){
  tile_forest_area <- function(img) {
    
    res <- ee$Image$pixelArea()$
      divide(10000)$
      updateMask(img$select("Map")$eq(1))$
      reduceRegion(
        reducer = ee$Reducer$sum(),
        geometry = img$geometry(),
        scale = 10,
        maxPixels = 1e13
      )$
      getInfo()
    
    as.numeric(res$area)
  }
  
  
  # test it on a few tiles:
  
  imgs <- gfc_ic$
    limit(5)$
    toList(5)
  
  areas <- numeric(5)
  
  for(i in 0:4){
    
    img <- ee$Image(imgs$get(i))
    
    idx <- img$get("system:index")$getInfo()
    
    cat(i + 1, idx, "\n")
    
    areas[i + 1] <- tile_forest_area(img)
    
    cat("Area:", round(areas[i + 1]), "ha\n\n")
  }
  
  sum(areas)
  
  
  # scaling up to all (1,420) tiles
  
  nTiles <- gfc_ic$size()$getInfo()  
  
  imgs <- gfc_ic$toList(nTiles)
  
  areas <- numeric(nTiles)
  
  for(i in 0:(nTiles - 1)) {
    
    img <- ee$Image(imgs$get(i))
    
    idx <- img$get("system:index")$getInfo()
    
    cat(i + 1, idx, "\n")
    
    areas[i + 1] <- tile_forest_area(img)
    
    cat("Area:", round(areas[i + 1]), "ha\n\n")
  }
  
  
  forest_area <- sum(areas)
  forest_area
  
}

##

gfc_ic$first()$getInfo()
gfc_ic$first()$getInfo()
gfc_ic$first()$propertyNames()$getInfo()


## Next approach: using 'reticulate' to send the work to GEE  --> It times out!!

#py_run_string("
#import ee
#
## Load the image collection
#gfc = ee.ImageCollection('JRC/GFC2020/V2').select('Map')
#
## Function to compute forest area (ha) for each tile
#def add_area(img):
#
#    forest_area = (
#        ee.Image.pixelArea()
#          .toDouble()
#          .divide(10000)                 # m² -> hectares
#          .updateMask(img.eq(1))
#          .reduceRegion(
#              reducer = ee.Reducer.sum(),
#              geometry = img.geometry(),
#              scale = 10,
#              maxPixels = 1e13
#          )
#          .get('area')
#    )
#
#    return img.set('forest_area_ha', forest_area)
#
## Compute the area for every tile
#areas = gfc.map(add_area)
#
## Sum all tile areas (still server-side)
#total_forest_area = areas.aggregate_sum('forest_area_ha')
#")
#
## Retrieve the final result (hectares)
#total_ha <- py$total_forest_area$getInfo()
#
#cat(sprintf(
#  "Global mapped forest area: %.0f ha (%.2f million ha)\n",
#  total_ha,
#  total_ha / 1e6
#))


# Next approach: batch processing, not interactive requests
#The idea is:
#1)Compute the forest area for each tile.
#2)Store it as a property of a Feature.
#3)Export the resulting FeatureCollection as a CSV to Google Drive.
#4)Sum the forest_area_ha column in R.

py_run_string("
import ee

# ---------------------------------------------------------------------
# Load datasets
# ---------------------------------------------------------------------

gfc = ee.ImageCollection('JRC/GFC2020/V2').select('Map')

gaul = ee.FeatureCollection('FAO/GAUL/2015/level0')

# Dissolve all countries into a single geometry
gaul_geometry = gaul.geometry()

# ---------------------------------------------------------------------
# Compute forest area for each tile
# ---------------------------------------------------------------------

def image_to_feature(img):

    # Restrict the image to the GAUL land area
    img = img.clip(gaul_geometry)

    forest_area = (
        ee.Image.pixelArea()
          .toDouble()
          .divide(10000)
          .updateMask(img.eq(1))
          .reduceRegion(
              reducer = ee.Reducer.sum(),
              geometry = img.geometry(),
              scale = 10,
              maxPixels = 1e13
          )
          .get('area')
    )

    return ee.Feature(
        None,
        {
            'tile': img.get('system:index'),
            'forest_area_ha': forest_area
        }
    )

fc = ee.FeatureCollection(gfc.map(image_to_feature))

# ---------------------------------------------------------------------
# Export
# ---------------------------------------------------------------------

task = ee.batch.Export.table.toDrive(
    collection = fc,
    description = 'GFC2020_V2_forest_area_GAUL',
    folder = 'EarthEngine',
    fileNamePrefix = 'GFC2020_V2_forest_area_GAUL',
    fileFormat = 'CSV'
)

task.start()

print('Task submitted.')
")

# After submitting the python process to GEE:
# 1) Open the Earth Engine Code Editor.
# 2) Go to the Tasks tab (or check the task in the Cloud project if you're using the Python API).
# 3) You should see GFC2020_forest_area_by_tile.
# 4) Click Run if necessary (depending on how your project is configured) and wait for it to finish. 
# 5) Check export in the GDrive.


install.packages("googledrive")
library(googledrive)
#drive_auth()    # only for the first access, then credentials should be stored

drive_ls()
drive_ls("EarthEngine")
file <- drive_find("GFC2020_forest_area_by_tile.csv")
drive_download(
  file,
  path = paste0(dir_2ndAssessment, "GFC2020_forest_area_by_tile.csv"),
  overwrite = TRUE
)

areas <- read.csv(paste0(dir_2ndAssessment, "GFC2020_forest_area_by_tile.csv"))
head(areas)
sum(is.na(areas$forest_area_ha))


mapped_area <- round(sum(areas$forest_area_ha)/10^6, 0)
mapped_area


# Table 6 reproduction

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

addWorksheet(wb, "Forest_area")

writeData(wb,
          sheet = "Forest_area",
          report_table)

saveWorkbook(wb, file = file.path(dir_2ndAssessment, "Forest_area_report.xlsx"), overwrite = TRUE)


# to png

save_as_image(ft, path = file.path(dir_2ndAssessment, "Forest_area_GFC_v2.png"))







#






