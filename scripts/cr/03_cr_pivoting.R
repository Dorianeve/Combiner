# CR Flow - 03 - Pivoting ----


## Prep env ----
source("config.yml")
source("requirements/libraries.R")

## Load ----
df <- read.csv("data/arr/cr/cr joined.csv", encoding = "UTF-8")

## Compute the totals for each category -----
df %<>%
  mutate(across(c(
    pre_primary_targeted_female, pre_primary_targeted_male,
    primary_targeted_female, primary_targeted_male,
    secondary_targeted_female, secondary_targeted_male,
    unknown_level_of_education_targeted_female, unknown_level_of_education_targeted_male,
    pre_primary_reached_female, pre_primary_reached_male,
    primary_reached_female, primary_reached_male,
    secondary_reached_female, secondary_reached_male,
    unknown_level_of_education_reached_female, unknown_level_of_education_reached_male
  ), as.numeric)) %>%
  mutate(pre_primary_targeted_total = pre_primary_targeted_female + pre_primary_targeted_male,
         primary_targeted_total = primary_targeted_female + primary_targeted_male,
         secondary_targeted_total = secondary_targeted_female + secondary_targeted_male,
         unknown_level_of_education_targeted_total = unknown_level_of_education_targeted_female + unknown_level_of_education_targeted_male,
         pre_primary_reached_total = pre_primary_reached_female + pre_primary_reached_male,
         primary_reached_total = primary_reached_female + primary_reached_male,
         secondary_reached_total = secondary_reached_female + secondary_reached_male,
         unknown_level_of_education_reached_total = unknown_level_of_education_reached_female + unknown_level_of_education_reached_male)
  
## Pivoting ----
##  Function for pivoting
process_reporting_file <- function(df, file_name = NA) {
  
  # Explicit column list based on your input
  target_cols <- c(
    "pre_primary_targeted_female", "pre_primary_targeted_male", "pre_primary_targeted_total",
    "primary_targeted_female", "primary_targeted_male", "primary_targeted_total",
    "secondary_targeted_female", "secondary_targeted_male", "secondary_targeted_total",
    "unknown_level_of_education_targeted_female", "unknown_level_of_education_targeted_male", "unknown_level_of_education_targeted_total",
    "total_female_targeted", "total_male_targeted", "total_beneficiaries_targeted",
    "pre_primary_reached_female", "pre_primary_reached_male", "pre_primary_reached_total",
    "primary_reached_female", "primary_reached_male", "primary_reached_total",
    "secondary_reached_female", "secondary_reached_male", "secondary_reached_total",
    "unknown_level_of_education_reached_female", "unknown_level_of_education_reached_male", "unknown_level_of_education_reached_total",
    "total_female_reached", "total_male_reached", "total_beneficiaries_reached"
  )
  
  # Step 1: Convert to numeric safely (replace errors with NA)
  df_clean <- df %>%
    mutate(across(all_of(target_cols), ~ suppressWarnings(as.numeric(.)))) %>%
    # Optional: add filename tracking
    mutate(file_name = file_name)
  
  # Step 2: Add calculated totals (optional — depends if you still need, because some totals are already present!)
  # Note: your list already includes total columns, skip unless needed.
  
  # Step 3: Unpivot (long format)
  df_long <- df_clean %>%
    pivot_longer(
      cols = all_of(target_cols),
      names_to = "Attribute",
      values_to = "Number"
    )
    # ) %>%
    # filter(!is.na(Number), Number != 0)
  
  # Step 4: Derive Gender, Status, Level of Education
  df_long <- df_long %>%
    mutate(
      Gender = case_when(
        str_detect(Attribute, "female") ~ "Female",
        str_detect(Attribute, "male") ~ "Male",
        str_detect(Attribute, "total_beneficiaries") ~ "Total",
        str_detect(Attribute, "total_female|total_male") ~ "Total",
        str_detect(Attribute, "total") ~ "Total",
        TRUE ~ "Error"
      ),
      Status = case_when(
        str_detect(Attribute, "targeted") ~ "Targeted",
        str_detect(Attribute, "reached") ~ "Reached",
        TRUE ~ "Error"
      ),
      level_of_education = case_when(
        str_detect(Attribute, "pre_primary") ~ "Pre-primary",
        str_detect(Attribute, "primary") ~ "Primary",
        str_detect(Attribute, "secondary") ~ "Secondary",
        str_detect(Attribute, "unknown_level_of_education") ~ "Unknown level of education",
        str_detect(Attribute, "total") ~ "Total",
        TRUE ~ "Error"
      )
    )
  
  # Step 5: Clean final output
  df_long %>%
    select(
      everything(),  # Keeps original metadata columns + file name
      Gender,
      Status,
      level_of_education,
      Number
    )
}

# Use pivoting function
clean <- process_reporting_file(df)


## Cleaning ----
clean %<>%
  rename(Covid19relateddata = covid_19_related_report,
         Typeofreporting = reporting_stage)

# Covid19relateddata variable
clean %<>%
  mutate(Covid19relateddata = ifelse(Covid19relateddata == "No", "Non-Covid-19 related data",
                                     "Covid-19 related data"))

# Source of data
clean %<>%
  mutate(Sourceofdata = paste0("ARR", substr(reporting_year, 3, 4), " CR"))

# Type of beneficiary
clean %<>%
  mutate(type_of_beneficiary = case_when(
    grepl("other", type_of_beneficiary, ignore.case = TRUE) ~ "Other affected populations",
    grepl("total", type_of_beneficiary, ignore.case = TRUE) ~ "Total",
    grepl("unknown", type_of_beneficiary, ignore.case = TRUE) ~ "Type of beneficiary unknown",
    TRUE ~ type_of_beneficiary
  ))

# Type of reporting
clean %<>%
  mutate(Typeofreporting = case_when(
    grepl("individual", Typeofreporting, ignore.case = TRUE) ~ "Individual",
    grepl("joint", Typeofreporting, ignore.case = TRUE) ~ "Joint",
    TRUE ~ NA
  ))


# Naming
clean %<>%
  rename(Typeofeducation = type_of_education,
         Indicator = indicator,
         Levelofeducation = level_of_education,
         Typeofbeneficiary = type_of_beneficiary,
         Commentsandcontext = comments_and_context,
         Dateofreporting = date_of_reporting,
         ReportingWindow = report_year,
         ReportingPeriodUpTo = reporting_period_up_to,
         MnEReview = m_e_review,
         Covid19relatedgrant = Covid.19related,
         Filename = file_name)

# Select for final output
clean %<>%
  select(ProgrammeID,
         LeadGRN,
         GMGRN,
         Reportingrole,
         Granteeorganization,
         Country,
         Typeofinvestment,
         Startdate,
         Currentenddate,
         Excomapprovaldate,
         Covid19relatedgrant,
         Covid19relateddata,
         Typeofreporting,
         Typeofeducation, 
         Indicator,
         Status,
         Typeofbeneficiary,
         Levelofeducation,
         Gender,
         Number,
         Commentsandcontext,
         Dateofreporting,
         Sourceofdata,
         GRID,
         Filename,
         MnEReview,
         ReportingWindow,
         ReportingPeriodUpTo,
         all_of(old_active_year),
         all_of(active_year),
         ActiveStrategicPlan,
         ExportDate)

## Filters ----
clean %<>% filter(.data[[active_year]] == "Yes")
clean %<>% filter(Typeofinvestment != "AF")
clean %<>% filter(Country != "Global" | Country != "Regional")

## Save current year combiner root ----
write.csv(clean, "data/arr/cr/cr cleaned root.csv", row.names = FALSE)


## MnE saving ----

### Filters MnE Approved ----
cleanMnE <- clean %>%
  filter(if (apply_mne_filter) MnEReview == "Approved" else TRUE)

### Annual Cumulative pivoting----
cleanMnE %<>%
  mutate(!!paste0("ARR", as.numeric(reporting_year) %% 100, "Annual") := "Yes",
         !!paste0("ARR", as.numeric(reporting_year) %% 100, "Cumulative") := "Yes")

# Define the dynamic column names
cols_to_pivot <- paste0("ARR", as.numeric(reporting_year) %% 100, c("Cumulative", "Annual"))

# Pivot
cleanMnE %<>%
  pivot_longer(
    cols = all_of(cols_to_pivot),  # columns to unpivot
    names_to = "Exercise",                  # new column for former column names
    values_to = "Value"                      # new column for their values
  )

# Clean variable Exercise
cleanMnE %<>%
  mutate(Exercise = case_when(
    grepl("cumulative", Exercise, ignore.case = TRUE) ~ exercise_cumulative,
    grepl("annual", Exercise, ignore.case = TRUE) ~ exercise_annual,
    TRUE ~ Exercise
  ))

cleanMnE %<>%
  select(-c(Value))

### Load old combiner for binding ----
old_combiner <- read.csv("data/arr/cr/cr historical cleaned.csv", encoding = "UTF-8")

cleanMnE <- bind_rows(cleanMnE, old_combiner)

### Save MnE version ----
write.csv(cleanMnE, paste0("data/arr/cr/cleaned/", today(),"_cr cleaned.csv"), row.names = FALSE)
# Save output (xlsx) ----
write.xlsx(cleanMnE, file = "data/arr/cr/cleaned/cr_clean.xlsx", overwrite = TRUE)


rm(list = ls())
