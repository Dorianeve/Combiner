# PR Flow - 03 - Pivoting ----


## Prep env ----
source("config.yml")
source("requirements/libraries.R")


## Import ----
df <- read.csv("data/arr/pr/pr source data.csv", encoding = "UTF-8")


## Pivot flow ----
### Prepping ----
# Ensure character columns to prevent pivot errors
df[] <- lapply(df, as.character)

# Identify all pivot columns ending in _baseline, _target, etc.
pivot_cols <- names(df)[str_detect(names(df), "_(baseline|target|revised_target|rw[1-9])$")]

# Clean unit of measurement before pivoting
df %<>%
  mutate(unit_of_measurement = trimws(unit_of_measurement)) %>%
  mutate(unit_of_measurement = str_to_title(unit_of_measurement)) %>%
  mutate(unit_of_measurement = case_when(
    grepl("Us\\$", unit_of_measurement, ignore.case = TRUE) ~ "US$",
    grepl("Number", unit_of_measurement, ignore.case = TRUE) ~ "Number #",
    grepl("Percentage", unit_of_measurement, ignore.case = TRUE) ~ "Percentage %",
    grepl("Ratio", unit_of_measurement, ignore.case = TRUE) ~ "Ratio",
    grepl("Scale", unit_of_measurement, ignore.case = TRUE) ~ "Scale",
    grepl("Yes", unit_of_measurement, ignore.case = TRUE) ~ "Yes/No",
    TRUE ~ unit_of_measurement
  ))

### Pivot ----
df_long <- df %>%
  pivot_longer(
    cols = all_of(pivot_cols),
    names_to = "Attribute",
    values_to = "Value"
  )

### Attribute split ----
# Split into core attribute and reporting window
# Define suffixes, longest first
suffixes <- c(
  "revised_target",
  "revised",
  "baseline",
  "target",
  paste0("rw", 1:9)
)

# Create regex pattern
suffix_pattern <- paste0("(", paste0(suffixes, collapse = "|"), ")$")

df_split <- df_long %>%
  mutate(
    Attribute_2 = str_extract(Attribute, suffix_pattern),
    Attribute_1 = ifelse(!is.na(Attribute_2),
                         str_remove(Attribute, paste0("_", Attribute_2, "$")),
                         Attribute)
  )

### Core attributes (Attribute_1) ----
# Normalize Attribute_1 (core)
df_split %<>%
  mutate(Attribute_1 = case_when(
    grepl("year_of", Attribute_1, ignore.case = TRUE) ~ "Year",
    grepl("total", Attribute_1, ignore.case = TRUE) ~ "Total",
    grepl("female", Attribute_1, ignore.case = TRUE) ~ "Female",
    grepl("male", Attribute_1, ignore.case = TRUE) ~ "Male",
    grepl("other_levels_of_disaggregation", Attribute_1, ignore.case = TRUE) ~ "OtherLevelsOfDisaggregation",
    grepl("children_with_disabilities", Attribute_1, ignore.case = TRUE) ~ "ChildrenWithDisabilities",
    grepl("adapted_accessible", Attribute_1, ignore.case = TRUE) ~ "AdaptedAccessible",
    grepl("comments", Attribute_1, ignore.case = TRUE) ~ "Comments",
    grepl("report_end_date", Attribute_1, ignore.case = TRUE) ~ "ReportEndDate",
    TRUE ~ NA_character_
  )) %>%
  mutate(Attribute_2 = str_to_sentence(Attribute_2))  # Normalize reporting window name

#### Gender ----
df_gender <- df_split %>%
  filter(Attribute_1 %in% c("Total", "Female", "Male")) %>%
  rename(Gender = Attribute_1, ReportingWindow = Attribute_2)   %>%
  distinct(LeadGRN, result_statement, outcome_output_numbering, ReportingWindow, Gender, analysis_code, indicator, grantees_own_program_specific_indicator, source_of_data, .keep_all = TRUE)

#### ReportEndDate ----
df_dates <- df_split %>%
  select(LeadGRN, Attribute_2, Attribute_1, analysis_code, result_statement, outcome_output_numbering,
         indicator, grantees_own_program_specific_indicator, Value, source_of_data) %>%
  filter(Attribute_1 %in% c("ReportEndDate")) %>%
  rename(ReportEndDate = Value, ReportingWindow = Attribute_2) %>%
  distinct(LeadGRN, result_statement, outcome_output_numbering, ReportingWindow, ReportEndDate, analysis_code, indicator, grantees_own_program_specific_indicator, source_of_data, .keep_all = TRUE)

#### Comments -----
df_comments <- df_split %>%
  select(LeadGRN, Attribute_2, Attribute_1,analysis_code, result_statement, outcome_output_numbering,
         indicator, grantees_own_program_specific_indicator, Value, source_of_data) %>%
  filter(Attribute_1 %in% c("Comments")) %>%
  rename(Comments = Value, ReportingWindow = Attribute_2) %>%
  select(-c(Attribute_1)) %>%
  distinct(LeadGRN, result_statement, outcome_output_numbering, ReportingWindow, Comments, analysis_code, indicator, grantees_own_program_specific_indicator, source_of_data, .keep_all = TRUE)

#### Year ----
df_year <- df_split %>%
  select(LeadGRN, Attribute_2, Attribute_1, analysis_code, result_statement, outcome_output_numbering,
         indicator, grantees_own_program_specific_indicator, Value, source_of_data) %>%
  filter(Attribute_1 %in% c("Year")) %>%
  rename(Year = Value, ReportingWindow = Attribute_2) %>%
  select(-c(Attribute_1)) %>%
  distinct(LeadGRN,result_statement, outcome_output_numbering, ReportingWindow, Year, analysis_code, indicator, grantees_own_program_specific_indicator, source_of_data, .keep_all = TRUE)

#### Disaggregation (CWD, Accessibility, Other) ----
# This necessitates pivoting as well
df_disagg <- df_split %>%
  select(LeadGRN, Attribute_2, Attribute_1, analysis_code, result_statement, outcome_output_numbering,
         indicator, grantees_own_program_specific_indicator, Value, source_of_data) %>%
  filter(Attribute_1 %in% c("ChildrenWithDisabilities", "AdaptedAccessible", "OtherLevelsOfDisaggregation")) %>%
  pivot_wider(
    names_from = Attribute_1,
    values_from = Value,
    values_fn = ~ first(na.omit(.))
  ) %>%
  rename(ReportingWindow = Attribute_2)

df_disagg %<>%
  mutate(
    # Ensure both are character and trimmed
    ChildrenWithDisabilities = as.character(ChildrenWithDisabilities),
    AdaptedAccessible = as.character(AdaptedAccessible),
    
    ChildrenWithDisabilities = str_trim(str_to_lower(ChildrenWithDisabilities)),
    AdaptedAccessible = str_trim(str_to_lower(AdaptedAccessible))
  ) %>%
  mutate(
    # Fix: Replace ChildrenWithDisabilities if it's missing or placeholder
    ChildrenWithDisabilities = if_else(
      is.na(ChildrenWithDisabilities) | ChildrenWithDisabilities %in% c("n/a", "no data", "", "na"),
      AdaptedAccessible,
      ChildrenWithDisabilities
    )
  )

# Retain the necessary columns
df_disagg <- df_disagg %>%
  select(LeadGRN, ReportingWindow, OtherLevelsOfDisaggregation, ChildrenWithDisabilities, result_statement, outcome_output_numbering,
         analysis_code, indicator, grantees_own_program_specific_indicator, source_of_data) %>%
  distinct(LeadGRN,  result_statement, outcome_output_numbering, ReportingWindow, analysis_code, indicator, grantees_own_program_specific_indicator, source_of_data, .keep_all = TRUE)

#### Merge Core on LeadGRN and ReportingWindow ----
df_final <- df_gender %>%
  left_join(df_disagg, by = c("LeadGRN", "result_statement", "outcome_output_numbering","ReportingWindow", "analysis_code", "source_of_data", "indicator", "grantees_own_program_specific_indicator")) %>%
  left_join(df_dates, by = c("LeadGRN", "result_statement", "outcome_output_numbering","ReportingWindow", "analysis_code", "source_of_data","indicator", "grantees_own_program_specific_indicator")) %>%
  left_join(df_comments, by = c("LeadGRN", "result_statement", "outcome_output_numbering","ReportingWindow", "analysis_code", "source_of_data","indicator", "grantees_own_program_specific_indicator")) %>%
  left_join(df_year, by = c("LeadGRN", "result_statement", "outcome_output_numbering","ReportingWindow", "analysis_code", "source_of_data","indicator", "grantees_own_program_specific_indicator"))


# Final clean-up
df_final %<>%
  mutate(Value = ifelse(Value == "NULL", NA, Value)) %>%
  filter(!is.na(analysis_code) | 
           !is.na(Value) | 
           !is.na(ChildrenWithDisabilities) |
           !is.na(OtherLevelsOfDisaggregation))

df_final %<>%
  mutate(level = ifelse(is.na(level), level_outcome_output, level)) %>% select(-c(level_outcome_output))

## Filters "dirt" ----
# Filter out non reported analysis codes
df_final %<>%
  filter(!grepl("Data in this row is not needed", indicator_data_status, ignore.case = TRUE))

### Standard Indicators filter ----
# Filter out if ALL values in SI tab are NA
df_final %<>%
  group_by(LeadGRN, analysis_code, source) %>%
  filter(!(all(source == "SI") & all(is.na(Value)))) %>%
  ungroup()


## Headers renaming -----
# Define renaming map (cleaned, no spaces or special characters)
rename_map_flipped <- c(
  OutcomeOutputNumbering = "outcome_output_numbering",
  ReportingWindow = "Attribute_2",
  Level = "level",
  ResultStatement = "result_statement",
  Indicator = "indicator",
  GeneralComments = "comments",
  CurrentEndDate = "Currentenddate",
  DetailedComments = "notes_on_result_indicator",
  AlignmentWithHRPIndicators = "alignment_with_hrp_rrp_tep_esp_indicators",
  ProgramSpecificIndicator = "grantees_own_program_specific_indicator",
  DateIndicatorProposed = "date_indicator_is_proposed",
  ECWComments = "ecw_m_e_comments",
  GranteeResponseToMEComment = "grantee_response_to_m_e_comment",
  ContributingAgencies = "contributing_agencies",
  UnitOfMeasurement = "unit_of_measurement",
  SourceOfVerification = "source_of_verification",
  DataAvailability = "data_availability",
  DuplicatedIndicator = "duplicated_indicator",
  IndicatorAdmin = "indicator_admin",
  IndicatorApplicability = "indicator_applicability",
  DisaggregatedBySex = "disaggregated_by_sex",
  IndicatorType = "indicator_type",
  AnalysisCode = "analysis_code",
  GRID = "GRID",
  FileName = "file_name",
  LevelOutcomeOutput = "level_outcome_output",
  SourceOfData = "source_of_data",
  ReportingRole = "ReportingRole",
  Covid19related = "Covid.19related"
)

# Safely apply only for existing columns in the data
valid_renames <- rename_map_flipped[rename_map_flipped %in% names(df_final)]

df_final %<>%
  rename(!!!valid_renames)
names(df_final)

# Your original column names
original_columns <- names(df_final)

## Columns order ----
# Define the ordered columns based on your scheme
ordered_columns <- c(
  "ProgrammeID",
  "LeadGRN", 
  "GMGRN",  # Placeholder, only if this column exists
  "ReportingRole",
  "Typeofinvestment", 
  "Country", 
  "Granteeorganization", 
  "StartDate",  # Placeholder, only if this column exists
  "CurrentEndDate", 
  "Covid.19Related",  # Placeholder, only if this column exists
  "GeneralComments",
  "OutcomeOutputNumbering", 
  "Level", 
  "ResultStatement", 
  "Indicator", 
  "ProgramSpecificIndicator", 
  "IndicatorAndPSIMerged",  # Placeholder, only if this column exists
  "ECWComments", 
  "GranteeResponseToMEComment", 
  "AlignmentWithHRPIndicators", 
  "ContributingAgencies", 
  "DateIndicatorProposed", 
  "UnitOfMeasurement", 
  "SourceOfVerification", 
  "ReportingWindow", 
  "Year", 
  "ReportEndDate", 
  "Gender", 
  "Value", 
  "ChildrenWithDisabilities", 
  "OtherLevelsOfDisaggregation", 
  "Comments", 
  "AnalysisCode", 
  "Attribute", 
  "ecw_standard_indicator_select_from_dropdown_or_select_other_program_specific_indicator_and_add_it", 
  "IndicatorApplicability", 
  "DisaggregatedBySex", 
  "IndicatorAdmin", 
  "DuplicatedIndicator", 
  "DataAvailability", 
  "LinkageToECWResult",  # Placeholder
  "Exercise",  # Placeholder
  "SourceOfData", 
  old_active_year,
  active_year,
  "OmitForBeneficiaryAnalysis",  # Placeholder
  "OmitForEducationLevelAnalysis",  # Placeholder
  "MnEReview", 
  "GRID", 
  "FileName",
  "ExportDate"
)

# Keep only existing columns from the ordered list
matched_columns <- ordered_columns[ordered_columns %in% original_columns]

# Add unmatched columns at the end
unmatched_columns <- setdiff(original_columns, matched_columns)
final_column_order <- c(matched_columns, unmatched_columns)

# Reorder the dataframe
df_final <- df_final[, final_column_order]

clean <- df_final

## Final filters ----
clean %<>% filter(.data[[active_year]] == "Yes")
clean %<>% filter(Typeofinvestment != "AF")
clean %<>% filter(Country != "Global" | Country != "Regional")

# filter clean based on the list
# filtered on Approved
clean %<>%
  filter(if (apply_mne_filter) MnEReview == "Approved" else TRUE)

## Annual / Cumulative ----
clean %<>%
  mutate(!!paste0("ARR", as.numeric(reporting_year) %% 100, "Annual") := "Yes",
         !!paste0("ARR", as.numeric(reporting_year) %% 100, "Cumulative") := "Yes")

# Define the dynamic column names
cols_to_pivot <- paste0("ARR", as.numeric(reporting_year) %% 100, c("Cumulative", "Annual"))

# Pivot
clean %<>%
  pivot_longer(
    cols = all_of(cols_to_pivot),  # columns to unpivot
    names_to = "Exercise",                  # new column for former column names
    values_to = "ValueExer"                      # new column for their values
  )

clean %<>%
  mutate(Exercise = case_when(
    grepl("cumulative", Exercise, ignore.case = TRUE) ~ exercise_cumulative,
    grepl("annual", Exercise, ignore.case = TRUE) ~ exercise_annual,
    TRUE ~ Exercise
  ))

clean %<>%
  select(-c(ValueExer, Attribute))

## Safe filtering: filter to avoid duplicates lines ----
clean %<>%
  unique()

## Old Combiner ----
old_combiner <- read.csv("data/arr/pr/pr historical cleaned.csv", encoding = "UTF-8")
names_pr <- read.csv("data/arr/pr/utilities/pr_old_new_names.csv", encoding = "UTF-8")

### Headers harmonization ----
# 1. Create a named vector: names to rename FROM and TO
rename_vector <- setNames(names_pr$clean, names_pr$old_combiner)

# 2. Rename columns in old_combiner (lookup-style)
names(old_combiner) <- ifelse(
  names(old_combiner) %in% names(rename_vector),
  rename_vector[names(old_combiner)],
  names(old_combiner)
)

# 3. Add missing columns to old_combiner (those in clean but not in old_combiner)
missing_in_old <- setdiff(names(clean), names(old_combiner))
old_combiner[missing_in_old] <- NA

# 4. Add missing columns to clean (those in old_combiner but not in clean)
missing_in_clean <- setdiff(names(old_combiner), names(clean))
clean[missing_in_clean] <- NA

# 5. Reorder columns to match
old_combiner <- old_combiner[, names(clean)]

## Bind old and new combiner ----
# Convert all columns in both data frames to character
clean <- clean %>% mutate(across(everything(), as.character))
old_combiner <- old_combiner %>% mutate(across(everything(), as.character))
clean <- bind_rows(clean, old_combiner)

# Save output ----
write.csv(clean, paste0("data/arr/pr/cleaned/", today(),"_pr cleaned.csv"), row.names = FALSE)

rm(list = ls())