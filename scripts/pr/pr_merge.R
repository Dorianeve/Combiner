# PR Flow - 02 - Merge ----


## Prep env ----
source("config.yml")
source("requirements/libraries.R")

## Import ----
rt <- read.csv("data/arr/pr/pr_results.csv", encoding = "UTF-8") # results template
pr <- read.csv("data/arr/pr/pr_programme.csv", encoding = "UTF-8") # programme results
si <- read.csv("data/arr/pr/pr_standard_indicators.csv", encoding = "UTF-8") # standard indicators

## Headers harmonization ----
### Renaming ----
# Import columns for cleaning (file pr_columns in utilities folder)
cols <- read.csv("data/arr/pr/utilities/pr_columns.csv", encoding = "UTF-8")

# NA handling
cols %<>%
  mutate(RevisedResults = ifelse(RevisedResults == "", NA, RevisedResults),
         RevisedIndicators = ifelse(RevisedIndicators == "", NA, RevisedIndicators),
         RevisedProgramme = ifelse(RevisedProgramme == "", NA, RevisedProgramme))

# Trimws charcter columns
cols[] <- lapply(cols, function(x) if (is.character(x)) trimws(x) else x)

# Map columsn for each template and filter out NA
programme_mapping <- cols %>%
  filter(!is.na(RevisedProgramme)) %>%
  select(Programme, RevisedProgramme) %>%
  deframe() %>%
  { setNames(names(.), .) }  # flip names and values

results_mapping <- cols %>%
  filter(!is.na(RevisedResults)) %>%
  select(Results, RevisedResults) %>%
  deframe() %>%
  { setNames(names(.), .) }  # flip names and values

indicators_mapping <- cols %>%
  filter(!is.na(RevisedIndicators)) %>%
  select(Indicators, RevisedIndicators) %>%
  deframe() %>%
  { setNames(names(.), .) }  # flip names and values

# Function to rename the columns to the corresponding name
safe_rename <- function(df, mapping) {
  if (length(mapping) > 0) {
    df %>% rename(any_of(mapping))
  } else {
    df
  }
}

# Launch function on each template
pr <- safe_rename(pr, programme_mapping)
rt <- safe_rename(rt, results_mapping)
si <- safe_rename(si, indicators_mapping)

### Add missing columns ----- 
# Step 1: Put dataframes in a list
df_list <- list(pr, rt, si)

# Step 2: Get all unique columns across all dataframes
all_columns <- Reduce(union, lapply(df_list, names))

# Step 3: Function to add missing columns and coerce to character
add_missing_columns <- function(df, all_columns) {
  missing_cols <- setdiff(all_columns, names(df))
  if (length(missing_cols) > 0) {
    df[missing_cols] <- NA
  }
  df <- df %>% select(all_of(all_columns))
  
  # Coerce all columns to character
  df[] <- lapply(df, as.character)
  
  return(df)
}

# Step 4: Apply function to all dataframes
df_list_aligned <- lapply(df_list, add_missing_columns, all_columns = all_columns)

## Source column ----
# (to get from which template the data is from)
names(df_list_aligned) <- c("Programme", "Results", "SI")
df_list_aligned <- Map(function(df, source) {
  df$source <- source
  df
}, df_list_aligned, names(df_list_aligned))


## Bind rows ----
df <- bind_rows(df_list_aligned)

## NA handling ----
# It scans the whole data frame and replaces every string "NA" with an actual missing value NA
df[df == "NA"] <- NA


## Column order ----
# Load
order <- read.csv("data/arr/pr/utilities/pr_columns_order.csv", encoding = "UTF-8")

order <- order$Results

# Ensure all columns from order are in combined_df
existing_order <- order[order %in% names(df)]
# Add any extra columns not in the order
extra_columns <- setdiff(names(df), existing_order)
# Final column order
final_order <- c(existing_order, extra_columns)
# Reorder dataframe
df %<>% select(all_of(final_order))

## Load GMS ----
grants_db <- read.csv("data/arr/grants_db.csv", encoding = "UTF-8")

# Select relevant GMS columns
grants_db %<>%
  select(GMGRN, Granteeorganization, Country, ProgrammeID, Typeofinvestment, Reportingrole, 
         Startdate, Currentenddate, all_of(old_active_year),
         all_of(active_year), ActiveStrategicPlan, Covid.19related)

## Process GMS and PR pre join ----
# Trimws PR
df %<>%
  mutate(LeadGRN = trimws(LeadGRN))

# Trimws GMS
grants_db %<>%
  mutate(GMGRN = trimws(GMGRN))

## Join PR to GMS (LeadGRN to GMGRN) ----
df %<>%
  left_join(grants_db, by = c("LeadGRN" = "GMGRN"))

# RW with Covid flag
df %<>%
  mutate(
    covid_19_related_report = if_else(
      str_detect(file_name, "Covid"),
      "Yes",
      "No"
    )
  )

## Load RW ----
# This is because the latest reporting windows is necessary for PR
rw <- read.csv("data/arr/rw/reporting_window.csv", encoding = "UTF-8")

# Covid flag for RW
rw %<>%
  mutate(covid_flag = ifelse(grepl("covid", FileName, ignore.case =  TRUE), "Yes", "No"))

# Select the necessary columns
rw %<>% select(ProgrammeID, reporting_period_up_to, report_year, m_e_review, covid_flag)

# Get the latest RW for the year
rw %<>%
  filter(reporting_period_up_to < reporting_window_cutoffdate)

# Get the unique list
rw_unique <- rw %>%
  arrange(desc(reporting_period_up_to)) %>%
  group_by(ProgrammeID, covid_flag) %>%
  slice(1) %>%
  ungroup()

# Covid flag for PR (this is needed as PID can have C19 yes or no)
df %<>%
  mutate(covid_flag = ifelse(grepl("covid", file_name, ignore.case =  TRUE), "Yes", "No"))

## Join PR and RW (PID and Covid concatenated ID) ----
df %<>%
  left_join(rw_unique, by = c("ProgrammeID", "covid_flag"))

# Names cleaning 
df %<>%
  rename(MnEReview = m_e_review)

## Filter dirt left ----
df %<>%
  filter(!(grepl("additional results", result_statement, ignore.case =  TRUE) |
             grepl("additional indicators", result_statement, ignore.case =  TRUE)))

# Save output ----
write.csv(df, "data/arr/pr/pr source data.csv", row.names = FALSE)

rm(list = ls())
