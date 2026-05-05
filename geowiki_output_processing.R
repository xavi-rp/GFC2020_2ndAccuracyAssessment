
library(tidyverse)





# ---- 0) Setup ----
dir <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_Second/kk/"

input_file <- "20260504.csv"
input_file <- "20240905.csv"

input_file <- paste0(dir, input_file)


# Extract date (YYYYMMDD)
date_part <- tools::file_path_sans_ext(basename(input_file))




# ---- 1) Read CSV (semicolon separated) ----

input_file_df <- read.csv(input_file) # col 'name' is repeated. Transformed them into "name" and "name.1" 


head(input_file_df)
nrow(input_file_df)
ncol(input_file_df)
names(input_file_df)


# ---- 2) Save initial copy ----
output_file_1 <- paste0(dir, date_part, "_GFC_2020_secondary.csv")

write.csv(input_file_df, output_file_1, row.names = FALSE)



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
output_file_2 <- paste0(dir, date_part, "_GFC_2020_secondary_modified.csv")

write.csv(input_file_df, output_file_2, row.names = FALSE)





# ---- 7 & 8) Replace emails ----
 # to be clarified with Rene
input_file_df <- input_file_df %>%
  mutate(
    email = case_when(
      email == "rene.colditz@ec.europa.eu" ~ "danilo.mollicone@gmail.com",
      email == "astrid.verhegghen@ec.europa.eu" ~ "valery.gond@cirad.fr",
      TRUE ~ email
    )
  )


output_file_2 <- paste0(dir, date_part, "_GFC_2020_secondary_modified.csv")
write.csv(input_file_df, output_file_2, row.names = FALSE)





# ---- 9) IDL code per_line_secondary.pro ----

secondary_modified <- read.csv(paste0(dir, date_part, "_GFC_2020_secondary_modified.csv"))

View(secondary_modified)

unique(secondary_modified$name)
unique(secondary_modified$name.1)

length(unique(secondary_modified$validation_id))
nrow(secondary_modified)
nrow(secondary_modified)/length(unique(secondary_modified$validation_id))

table(secondary_modified$name)

# check if there are duplicates per (validation_id, question). If 0 rows: each question appears exactly once per validation
secondary_modified %>%
  count(validation_id, name) %>%
  filter(n > 1)



secondary_modified_wide <- secondary_modified %>%
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

View(secondary_modified_wide)

nrow(secondary_modified_wide)
length(unique(secondary_modified_wide$validation_id)) * 4

nrow(secondary_modified_wide)/length(unique(secondary_modified_wide$validation_id))
length(unique(secondary_modified_wide$validation_id)) * 4

head(sort(table(secondary_modified_wide$validation_id)), 10)
sum(table(secondary_modified_wide$validation_id) == 4)  # 4233
sum(table(secondary_modified_wide$validation_id) == 3)  #   80

secondary_modified_wide[secondary_modified_wide$validation_id == 2745966, ] %>% View() # if 'GFC validation - forest' = 'no assignment', it has only 3 rows
secondary_modified_wide[secondary_modified_wide$validation_id == 2748197, ] %>% View() 



### Collapsing the rows into a single row per validation_id, merging the non-NA values ####
secondary_modified_wide_1row <- secondary_modified_wide %>%
  group_by(validation_id) %>%
  summarise(
    across(
      starts_with("GFC"),
      ~ if (all(is.na(.x))) NA else na.omit(.x)[1]  # if all values are NA, keep NA
    ),
    across(-c(starts_with("GFC")), first),
    .groups = "drop"
  ) %>%
  relocate(starts_with("GFC"), .after = last_col())

View(secondary_modified_wide_1row)


secondary_modified_wide_1row[secondary_modified_wide_1row$validation_id == 2745966, ] %>% View() 
secondary_modified_wide_1row[secondary_modified_wide_1row$validation_id == 2748197, ] %>% View() 



write.csv(secondary_modified_wide_1row, 
          paste0(dir, "secondary_data.csv"), 
          row.names = FALSE)





### Keeping the latest validation ####

## are there samples which have several validations?

length(unique(secondary_modified_wide$validation_id)) # 4313
length(unique(secondary_modified_wide$sample_id))     # 4000



secondary_modified_latest <- secondary_modified_wide_1row %>%
  group_by(sample_id) %>%
  slice_max(validation_id, n = 1, with_ties = FALSE) %>%
  ungroup()

View(secondary_modified_latest)



write.csv(secondary_modified_latest, 
          paste0(dir, "secondary_data_latest.csv"), 
          row.names = FALSE)



## some checks

View(secondary_modified[secondary_modified$sample_id == 2027527, ])
View(secondary_modified_wide[secondary_modified_wide$sample_id == 2027527, ])
View(secondary_modified_wide_1row[secondary_modified_wide_1row$sample_id == 2027527, ])  # info from "submission_item_id" and "enhancement" are lost, but they seem not relevant
View(secondary_modified_latest[secondary_modified_latest$sample_id == 2027527, ])










# Defining files names 
file_data <- paste0(dir, "secondary_data.csv")
file_data_latest <- paste0(dir, "secondary_data_latest.csv")
file_data_latest_adjusted <- paste0(dir, "secondary_data_latest_adjusted.csv")






