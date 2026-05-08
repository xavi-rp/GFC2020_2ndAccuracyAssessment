#-------------------------------------------------------------#
#-------------------------------------------------------------#
#------- Script to process the GeoWiki extracted data --------#
#-------------------------------------------------------------#
#-------------------------------------------------------------#



library(tidyverse)





# ---- 0) Setup ----

dir <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_Second/geowiki_2026/"

input_file <- "2026_05_07_valgroup85_exports_GFC_2020.csv"

input_file <- paste0(dir, input_file)


# Extract date (YYYYMMDD)
date_part <- "20260507"




# ---- 1) Read CSV (semicolon separated) ----

input_file_df <- read.csv(input_file, sep = ";") # col 'name' is repeated. Transformed them into "name" and "name.1" 


head(input_file_df)
nrow(input_file_df)  # 56007
ncol(input_file_df)  # 18
names(input_file_df)


# ---- 2) Save initial copy ----

write.csv(input_file_df, 
          paste0(dir, date_part, "_GFC_2020.csv"), 
          row.names = FALSE)



# ---- 3) Remove last line ----     
# Make sure if needed!!

#View(tail(input_file_df))

#input_file_df <- input_file_df %>%
#  slice_head(n = n() - 1)




# ---- 4 & 5) Remove commas and quotation marks ----
View(input_file_df)

# just to be sure if there are
checks <- input_file_df %>%
  summarise(across(
    everything(),
    list(
      commas = ~ sum(str_count(.x, ","), na.rm = TRUE),
      quotes = ~ sum(str_count(.x, '"'), na.rm = TRUE)
    )
  )) %>% as.data.frame()  %>% sum()


if (checks != 0){
  input_file_df <- input_file_df %>%
    mutate(across(everything(), ~ str_replace_all(.x, ",", ""))) %>%
    mutate(across(everything(), ~ str_replace_all(.x, '"', "")))
  
  print("commas and quotation marks removed")
}





# ---- 6) Save modified version ----

write.csv(input_file_df, 
          paste0(dir, date_part, "_GFC_2020_modified.csv"), 
          row.names = FALSE)





# ---- 7 & 8) Replace emails ----
# -Search for "rene.colditz@ec.europa.eu" in column "email" and replace with credentials from "danilo.mollicone@gmail.com". 
#  This is to correct for a missing confidence assignment in this strata.
# -Search for "astrid.verhegghen@ec.europa.eu" in column "email" and replace with credentials from "valery.gond@cirad.fr". 
#  This is to correct for missing forest/non-forest and confidence assignments in this strata.


    ### to be clarified with Rene ###


# just to be sure if there are
checks <- input_file_df %>%
  summarise(
    rene = sum(str_detect(email, "rene.colditz@ec.europa.eu"), na.rm = TRUE),
    astrid = sum(str_detect(email, "astrid.verhegghen@ec.europa.eu"), na.rm = TRUE),
    danilo = sum(str_detect(email, "danilo.mollicone@gmail.com"), na.rm = TRUE),
    valery = sum(str_detect(email, "valery.gond@cirad.fr"), na.rm = TRUE)
  )

checks


#input_file_df <- input_file_df %>%
#  mutate(
#    email = case_when(
#      email == "rene.colditz@ec.europa.eu" ~ "danilo.mollicone@gmail.com",
#      email == "astrid.verhegghen@ec.europa.eu" ~ "valery.gond@cirad.fr",
#      TRUE ~ email
#    )
#  )


write.csv(input_file_df, 
          paste0(dir, date_part, "_GFC_2020_modified.csv"), 
          row.names = FALSE)





# ---- 9) IDL code per_line_secondary.pro ----
# IDL program translated into R

paste0(date_part, "_GFC_2020_modified.csv")

#secondary_modified <- read.csv(paste0(dir, date_part, "_GFC_2020_modified.csv"))
dataset_modified <- read.csv(paste0(dir, date_part, "_GFC_2020_modified.csv"))

View(dataset_modified)

unique(dataset_modified$name)     
unique(dataset_modified$name.1)

length(unique(dataset_modified$validation_id))  # 14019 validations
nrow(dataset_modified)                          # total file rows (still several row per validation)

table(dataset_modified$name)

# check if there are duplicates per (validation_id, question). If 0 rows: each question appears exactly once per validation
dataset_modified %>%
  count(validation_id, name) %>%
  filter(n > 1)



dataset_modified_wide <- dataset_modified %>%
  # make column names easier to work with
  rename(
    question = name,
    answer = name.1
  ) %>%
  # Keep all other columns as identifiers
  pivot_wider(
    names_from = question,
    values_from = answer
  )  %>%
  relocate(
    `GFC validation - forest`,
    `GFC validation - confidence`,
    `GFC validation - forest type`,
    `GFC validation - other land use type`,
    `GFC validation - Issues with class assignment`,
    .after = last_col()
  )

View(dataset_modified_wide)
nrow(dataset_modified_wide)

head(sort(table(dataset_modified_wide$validation_id)), 10)
tail(sort(table(dataset_modified_wide$validation_id)), 10)
sum(table(dataset_modified_wide$validation_id) == 4)  # 13950
sum(table(dataset_modified_wide$validation_id) == 3)  #    69

dataset_modified_wide[dataset_modified_wide$validation_id == 2936848, ] %>% View() # if 'GFC validation - forest' = 'no assignment', it has only 3 rows
dataset_modified_wide[dataset_modified_wide$validation_id == 2959057, ] %>% View() 



### Collapsing the rows into a single row per validation_id, merging the non-NA values ####
dataset_modified_wide_1row <- dataset_modified_wide %>%
  group_by(validation_id) %>%
  summarise(
    across(starts_with("GFC"),
           ~ if (all(is.na(.x))) NA else na.omit(.x)[1]),  # if all values are NA, keep NA
    across(-c(starts_with("GFC")), first),
    .groups = "drop") %>%
  relocate(starts_with("GFC"), .after = last_col()) %>%
  relocate(comment, .after = last_col())

View(dataset_modified_wide_1row)


dataset_modified_wide_1row[dataset_modified_wide_1row$validation_id == 2936848, ] %>% View() 
dataset_modified_wide_1row[dataset_modified_wide_1row$validation_id == 2959057, ] %>% View() 



write.csv(dataset_modified_wide_1row, 
          paste0(dir, date_part, "_data.csv"), 
          row.names = FALSE)





### Keeping the latest validation ####

## are there samples which have several validations?

length(unique(dataset_modified_wide$validation_id)) # 14019, because some sample units were validated more than once
length(unique(dataset_modified_wide$sample_id))     # 13764, which is the number of sample units to be validated in this round
                                                    # correct



dataset_modified_latest <- dataset_modified_wide_1row %>%
  group_by(sample_id) %>%
  slice_max(validation_id, n = 1, with_ties = FALSE) %>%
  ungroup()

View(dataset_modified_latest)
nrow(dataset_modified_latest)   # 13764



write.csv(dataset_modified_latest, 
          paste0(dir, date_part, "_data_latest.csv"), 
          row.names = FALSE)



### Some checks ####

View(dataset_modified[dataset_modified$sample_id == 2229436, ])
View(dataset_modified_wide[dataset_modified_wide$sample_id == 2229436, ])
View(dataset_modified_wide_1row[dataset_modified_wide_1row$sample_id == 2229436, ])  # info from "submission_item_id" and "enhancement" are lost (only one remain), but they seem not relevant (each question/answer has its own "submission_item_id" and "enhancement")
View(dataset_modified_latest[dataset_modified_latest$sample_id == 2229436, ])





### Some important remarks:  ###
#  - In the 2024 dataset, the column names for the answers to the validation questions (e.g. "GFC validation - forest") are not modified; therefore they are all "name". When these are ingested to an R session, they are sequentially renamed to 'name.1', 'name.2', etc.  
#  - In the 2024 dataset, `GFC validation - forest type` and `GFC validation - other land use type` are merged in one single column ('name.7'). This has to be kept in mind when reviewing the R scripts of the first assessment.
# 




# ---- 10) Issues by interpreter ----

# Check for issues by interpreter. There should be nin, except for "no assignment" under "issues" for Europe, N-Africa, and S-Asia
# This needs to be clarified with Rene
## 


View(dataset_modified_latest)



# ---- 11) Replace "no assignment" with "no issue" under "issues" ----

View(dataset_modified_latest)

table(sort(dataset_modified_latest$`GFC validation - Issues with class assignment`))
sum(dataset_modified_latest$`GFC validation - Issues with class assignment` == "no assignment", na.rm = TRUE)  # 97
sum(is.na(dataset_modified_latest$`GFC validation - Issues with class assignment`))   # 0 NAs


dataset_modified_latest_adjusted <- dataset_modified_latest %>%
  mutate(`GFC validation - Issues with class assignment` = 
           if_else(`GFC validation - Issues with class assignment` == "no assignment",
                   "no issues",
                   `GFC validation - Issues with class assignment`))

table(sort(dataset_modified_latest_adjusted$`GFC validation - Issues with class assignment`))
sum(dataset_modified_latest_adjusted$`GFC validation - Issues with class assignment` == "no assignment", na.rm = TRUE)  # 0



write.csv(dataset_modified_latest_adjusted, 
          paste0(dir, date_part, "_data_latest_adjusted.csv"), 
          row.names = FALSE)


View(dataset_modified_latest_adjusted)
nrow(dataset_modified_latest_adjusted) # 13764




## Check for completeness ####
Reference_data_2026_strata <- readxl::read_excel("/Users/xavi_rp/Documents/JRC_D1/copy_SharePoint_kk/validation/Reference_data_2026_strata.xlsx", n_max = 15)

nrow(Reference_data_2026_strata)
names(Reference_data_2026_strata)
sort(Reference_data_2026_strata$ID2)

Ref_strata <- Reference_data_2026_strata %>% 
  filter(ID2 %in% c(284, 285, 286, 287, 288, 291, 292, 293, 294)) %>%
  select(ID2, `Region / Countries`, `Sample units...10`) %>%
  rename("ID2_2024" = "ID2")  %>%
  rename("Sample_units_for_valid" = `Sample units...10`) 



names(dataset_modified_latest_adjusted)
unique(dataset_modified_latest_adjusted$groupid)


table(dataset_modified_latest_adjusted$groupid) 

SampleUnits_validated <- table(dataset_modified_latest_adjusted$groupid) %>% 
  as.data.frame() %>%
  rename("groupid_2026" = "Var1")  %>%
  rename("Sample_units_validated_2026" = "Freq") 

check_completeness <- cbind(Ref_strata, SampleUnits_validated)
check_completeness

check_completeness_ft <- flextable::flextable(check_completeness)
flextable::save_as_image(check_completeness_ft, 
                         path = paste0(dir, date_part, "_check_completeness.png"))
 

write.csv(check_completeness, 
          paste0(dir, date_part, "_check_completeness.csv"), 
          row.names = FALSE)





