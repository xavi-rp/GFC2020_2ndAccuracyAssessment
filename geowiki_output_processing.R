#-------------------------------------------------------------#
#-------------------------------------------------------------#
#------- Script to process the GeoWiki extracted data --------#
#-------------------------------------------------------------#
#-------------------------------------------------------------#



library(tidyverse)
library(writexl)
library(openxlsx)





# ---- 0) Setup ----

dir <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_Second/geowiki_2026/"

input_file <- "2026_05_07_valgroup85_exports_GFC_2020.csv"
input_file <- "20260610_valgroup85_exports_GFC_2020.csv"   ## This DB contains the two 2026 rounds

input_file <- paste0(dir, input_file)


# Extract date (YYYYMMDD)
date_part <- "20260507"
date_part <- "20260610"




# ---- 1) Read CSV (semicolon separated) ----

input_file_df <- read.csv(input_file, sep = ";") # col 'name' is repeated. Transformed them into "name" and "name.1" 


head(input_file_df)
nrow(input_file_df)  # 56007, 72612
ncol(input_file_df)  # 18
names(input_file_df)

range(input_file_df$timestamp)


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

nrow(dataset_modified)

unique(dataset_modified$name)     
unique(dataset_modified$name.1)

length(unique(dataset_modified$validation_id))  # 14019 validations, 18171 (second round)
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

#View(dataset_modified_wide)
nrow(dataset_modified_wide)

head(sort(table(dataset_modified_wide$validation_id)), 10)
tail(sort(table(dataset_modified_wide$validation_id)), 10)
sum(table(dataset_modified_wide$validation_id) == 4)  # 13950, 18099
sum(table(dataset_modified_wide$validation_id) == 3)  #    69, 72 



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

length(unique(dataset_modified_wide$validation_id)) # 14019, because some sample units were validated more than once. 18171
length(unique(dataset_modified_wide$sample_id))     # 13764, which is the number of sample units to be validated in this round. 17825
                                                    # correct



dataset_modified_latest <- dataset_modified_wide_1row %>%
  group_by(sample_id) %>%
  slice_max(validation_id, n = 1, with_ties = FALSE) %>% 
  ungroup()

View(dataset_modified_latest)
nrow(dataset_modified_latest)   # 13764, 17825




write.csv(dataset_modified_latest, 
          paste0(dir, date_part, "_data_latest.csv"), 
          row.names = FALSE)



dataset_modified_latest_2ndRound <- dataset_modified_latest %>% 
  filter(timestamp > as.Date("2026/05/18")) #%>% nrow()  # 4061, correct (2nd round)

write.csv(dataset_modified_latest_2ndRound, 
          paste0(dir, date_part, "_data_latest_TieCall.csv"), 
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
sum(dataset_modified_latest$`GFC validation - Issues with class assignment` == "no assignment", na.rm = TRUE)  # 97, 119
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


dataset_modified_latest_adjusted %>% 
  filter(timestamp < as.Date("2026/05/18")) %>% nrow()  # 13764, correct (1st round)

dataset_modified_latest_adjusted_2ndRound <- dataset_modified_latest_adjusted %>% 
  filter(timestamp > as.Date("2026/05/18")) #%>% nrow()  # 4061, correct (2nd round)



write.csv(dataset_modified_latest_adjusted_2ndRound, 
          paste0(dir, date_part, "_data_latest_adjusted_TieCall.csv"), 
          row.names = FALSE)


# __________________________ #
#   From here, only checks   #
# __________________________ #



## Check for completeness (1st round) ####
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






## statistics of "no assignments", including in "issues" ####

## 2026 1st call:
date_part <- "20260507"


dataset_modified_latest <-read.csv(paste0(dir, date_part, "_data_latest.csv"))

nrow(dataset_modified_latest)   # 13764

#apply(dataset_modified_latest, 2, function(x) sum(x == "no assignment", na.rm = TRUE))


# count of "no assignment" in each column

dataset_modified_latest %>%
  summarise(
    across(
      c(
        GFC.validation...forest,
        GFC.validation...confidence,
        GFC.validation...Issues.with.class.assignment
      ),
      ~ sum(.x == "no assignment", na.rm = TRUE)
    )
  )


# rows that have NA both in forest.type and land.use.type
dataset_modified_latest %>%
  filter(
    is.na(`GFC.validation...forest.type`) &
      is.na(`GFC.validation...other.land.use.type`)
  ) %>%
  nrow()


# rows where at least one of those three columns contains "no assignment"
dataset_modified_latest %>%
  filter(
    if_any(
      c(
        GFC.validation...forest,
        GFC.validation...confidence,
        GFC.validation...Issues.with.class.assignment
      ),
      ~ .x == "no assignment"
    ) |
      (
        is.na(`GFC.validation...forest.type`) &
          is.na(`GFC.validation...other.land.use.type`)
      )
  ) %>% nrow()



# summarising all in a table
summary_tab <- tibble(
  NoAssignments = c(
    "No assignment in For/Non-For",
    "No assignment in Confidence",
    "No assignment in forest.type/land.use.type",
    "No assignment in Issues",
    "Rows with no assignment in any",
    "Total samples assessed"
  ),
  X2026_1stCall = c(
    sum(dataset_modified_latest$GFC.validation...forest == "no assignment", na.rm = TRUE),
    sum(dataset_modified_latest$GFC.validation...confidence == "no assignment", na.rm = TRUE),
    
    dataset_modified_latest %>%
      filter(
        is.na(`GFC.validation...forest.type`) &
          is.na(`GFC.validation...other.land.use.type`)
      ) %>%
      nrow(),
    
    sum(dataset_modified_latest$GFC.validation...Issues.with.class.assignment == "no assignment", na.rm = TRUE),
    
    dataset_modified_latest %>%
      filter(
        if_any(
          c(
            GFC.validation...forest,
            GFC.validation...confidence,
            GFC.validation...Issues.with.class.assignment
          ),
          ~ .x == "no assignment"
        ) |
          (
            is.na(`GFC.validation...forest.type`) &
              is.na(`GFC.validation...other.land.use.type`)
          )
      ) %>% nrow(),
    
    dataset_modified_latest %>% nrow()
  )
)

summary_tab

#





## 2026 Tie call:
date_part <- "20260610"

dataset_modified_latest_2ndRound <- read.csv(paste0(dir, date_part, "_data_latest_TieCall.csv"))

nrow(dataset_modified_latest_2ndRound)   # 4061


# summarising all in a table
summary_tab_tie <- tibble(
  NoAssignments = c(
    "No assignment in For/Non-For",
    "No assignment in Confidence",
    "No assignment in forest.type/land.use.type",
    "No assignment in Issues",
    "Rows with no assignment in any",
    "Total samples assessed"
  ),
  X2026_TieCall = c(
    sum(dataset_modified_latest_2ndRound$GFC.validation...forest == "no assignment", na.rm = TRUE),
    sum(dataset_modified_latest_2ndRound$GFC.validation...confidence == "no assignment", na.rm = TRUE),
    
    dataset_modified_latest_2ndRound %>%
      filter(
        is.na(`GFC.validation...forest.type`) &
          is.na(`GFC.validation...other.land.use.type`)
      ) %>%
      nrow(),
    
    sum(dataset_modified_latest_2ndRound$GFC.validation...Issues.with.class.assignment == "no assignment", na.rm = TRUE),
    
    dataset_modified_latest_2ndRound %>%
      filter(
        if_any(
          c(
            GFC.validation...forest,
            GFC.validation...confidence,
            GFC.validation...Issues.with.class.assignment
          ),
          ~ .x == "no assignment"
        ) |
          (
            is.na(`GFC.validation...forest.type`) &
              is.na(`GFC.validation...other.land.use.type`)
          )
      ) %>% nrow(),
    
    dataset_modified_latest_2ndRound %>% nrow()
  )
)

summary_tab_tie

summary_tab <- summary_tab %>% 
  left_join(summary_tab_tie)

summary_tab

#



## writing summary table
#write_xlsx(summary_tab,
#           paste0(dir, "2026_Summary_NoAssignment.xlsx"))




## statistics of "no assignments", including in "issues", by strata ####

summary_1st <- dataset_modified_latest %>%
  group_by(groupid) %>%
  summarise(
    NoAssignment_ForNonFor =
      sum(GFC.validation...forest == "no assignment", na.rm = TRUE),
    
    NoAssignment_Confidence =
      sum(GFC.validation...confidence == "no assignment", na.rm = TRUE),
    
    NoAssignment_ForestType_LandUseType =
      sum(
        is.na(`GFC.validation...forest.type`) &
          is.na(`GFC.validation...other.land.use.type`)
      ),
    
    NoAssignment_Issues =
      sum(
        GFC.validation...Issues.with.class.assignment == "no assignment",
        na.rm = TRUE
      ),
    
    NoAssignment_Any =
      sum(
        if_any(
          c(
            GFC.validation...forest,
            GFC.validation...confidence,
            GFC.validation...Issues.with.class.assignment
          ),
          ~ .x == "no assignment"
        ) |
          (
            is.na(`GFC.validation...forest.type`) &
              is.na(`GFC.validation...other.land.use.type`)
          )
      ),
    
    TotalSamples = n(),
    
    .groups = "drop"
  )

summary_1st 



summary_tie <- dataset_modified_latest_2ndRound %>%
  group_by(groupid) %>%
  summarise(
    NoAssignment_ForNonFor =
      sum(GFC.validation...forest == "no assignment", na.rm = TRUE),
    
    NoAssignment_Confidence =
      sum(GFC.validation...confidence == "no assignment", na.rm = TRUE),
    
    NoAssignment_ForestType_LandUseType =
      sum(
        is.na(`GFC.validation...forest.type`) &
          is.na(`GFC.validation...other.land.use.type`)
      ),
    
    NoAssignment_Issues =
      sum(
        GFC.validation...Issues.with.class.assignment == "no assignment",
        na.rm = TRUE
      ),
    
    NoAssignment_Any =
      sum(
        if_any(
          c(
            GFC.validation...forest,
            GFC.validation...confidence,
            GFC.validation...Issues.with.class.assignment
          ),
          ~ .x == "no assignment"
        ) |
          (
            is.na(`GFC.validation...forest.type`) &
              is.na(`GFC.validation...other.land.use.type`)
          )
      ),
    
    TotalSamples = n(),
    
    .groups = "drop"
  )

summary_tie




## Adding references
Reference_data_2026_strata <- readxl::read_excel("/Users/xavi_rp/Documents/JRC_D1/copy_SharePoint_kk/validation/Reference_data_2026_strata.xlsx", 
                                                 n_max = 15, sheet = "Main") 

View(Reference_data_2026_strata)

ref_2026 <- Reference_data_2026_strata %>% 
  filter(!is.na(ID2026_1)) %>% 
  select(ID...18, `Region / Countries...19`, 
         ID2026_1, `2026 Interpreters1`, 
         ID2026_Tie, `2026 Tie caller`) %>% 
  rename("ID" = "ID...18",
         "Region" = `Region / Countries...19`)



summary_1st <- ref_2026 %>% 
  select(ID, Region, ID2026_1, `2026 Interpreters1`) %>% 
  left_join(summary_1st, by = c("ID2026_1" = "groupid"))

summary_1st <-  bind_rows(
  summary_1st,
  summary_1st %>%
    summarise(
      across(
        -c(ID, Region, ID2026_1, `2026 Interpreters1`),
        ~ sum(.x, na.rm = TRUE)
      )
    )
)



summary_tie <- ref_2026 %>% 
  select(ID, Region, ID2026_Tie, `2026 Tie caller`) %>% 
  left_join(summary_tie, by = c("ID2026_Tie" = "groupid")) 

summary_tie <-  bind_rows(
    summary_tie,
    summary_tie %>%
      summarise(
        across(
          -c(ID, Region, ID2026_Tie, `2026 Tie caller`),
          ~ sum(.x, na.rm = TRUE)
        )
      )
  )


summary_tie






#summary_bygroup <- left_join(
#  summary_1st,
#  summary_tie,
#  by = "groupid",
#  suffix = c("_1stCall", "_TieCall")
#)
#
#summary_bygroup


## writing summary tables
#write_xlsx(summary_1st,
#           paste0(dir, "2026_1stCall_Summary_NoAssignment_bygroup.xlsx"))
#
#write_xlsx(summary_tie,
#           paste0(dir, "2026_TieCall_Summary_NoAssignment_bygroup.xlsx"))



write.xlsx(
  list(
    `2026_Summary_NoAssignment` = summary_tab,
    `2026_1stCall_Summary_bygroup` = summary_1st,
    `2026_TieCall_Summary_bygroup` = summary_tie
  ),
  file = paste0(dir, "2026_Summary_NoAssignment.xlsx"),
  overwrite = TRUE
)
