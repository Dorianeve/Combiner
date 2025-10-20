# PR Flow - 01 - Processing previous combiner ----


## Prep env ----
source("config.yml")
source("requirements/libraries.R")

## Load ----
old <- read.csv(old_pr_combiner, encoding = "UTF-8")
db <- read.csv("data/arr/grants_db.csv", encoding = "UTF-8")

## GMS Processing ----
# Select the necessary columns
db %<>%
  select(GMGRN, ProgrammeID, all_of(active_year), ActiveStrategicPlan, Reportingrole)


# Fix the Multiple grantees
old %<>%
  mutate(GMGRN = ifelse(GMGRN == "Mutiple Grantees", LeadGRN, GMGRN))

# Trimws LeadGRN
old %<>%
  mutate(LeadGRN = trimws(LeadGRN))

## Join old combiner to GMS (LeadGRN == GMGRN) ----
old %<>%
  left_join(db, by = c("LeadGRN" = "GMGRN", "ProgrammeID" = "ProgrammeID"))

## Active in variable ----
old %<>%
  mutate(!!active_year := ifelse(is.na(.data[[active_year]]), "No", .data[[active_year]]))


## Duplicate cumulative for current year ----
subset <- old %>%
  filter(Exercise == old_exercise_cumulative)

# Copying grants no longer active or active with covid data (this data goes in another template)
subset %<>%
  filter(
    .data[[active_year]] == "No" |
      (.data[[active_year]] == "Yes" & Covid19related == "Covid-19 related data")
  )

# Current year cumulative
subset %<>%
  mutate(Exercise = exercise_cumulative)

# Bind with combiner and duplicated subset
df <- rbind(old, subset)

## General cleaning and headers ----
# Parse as numeric
df$Value <- as.numeric(gsub(",", "", df$Value))

# df %<>%
#   select(-c(Total))

# # Rename to match clean (this is positional: do not delete columns otherwise change approach)
# names(df) <- c(
#   "LeadGRN",
#   "GMGRN",
#   "Typeofinvestment",
#   "Country",
#   "Granteeorganization",
#   "Startdate",
#   "Currentenddate",
#   "Covid19related",
#   "OutcomeOutputNumbering",
#   "Level",
#   "ResultStatement",
#   "Indicator",
#   "ProgramSpecificIndicator",
#   "IndicatorPSIMerged",
#   "ECWComments",
#   "GranteeResponsetoMEComment",
#   "AlignmentWithHRP_RRP_TEP_ESPIndicators",
#   "CountributingAgencies",
#   "DateIndicatorProposed",
#   "UnitOfMeasurement",
#   "SourceOfVerification",
#   "ReportingWindow",
#   "Year",
#   "ReportEndDate",
#   "Gender",
#   "ChildrenWithDisabilities",
#   "OtherLevelsOfDisaggregation",
#   "Comments",
#   "AnalysisCode",
#   "InterventionCategoryFromLibrary",
#   "IndicatorFromLibrary",
#   "IndicatorApplicability",
#   "DisaggregatedBySex",
#   "INdicatorADMIN",
#   "DUplicatedIndicator",
#   "DataAvailability",
#   "LinkECWResult",
#   "Exercise",
#   "SourceOfData",
#   "Activein2023",
#   "OmitForBeneficiaryAnalysis",
#   "OmitForEduLevelAnalysis",
#   "MnEReview",
#   "GRID",
#   "FileName",
#   "ProgrammeID",
#   "Activein2024",
#   "ReportingRole",
#   "Value")

# # Parse dates
# df %<>%
#   rename(CurrentEndDate = Currentenddate) %>%
#   mutate(CurrentEndDate = as.Date(CurrentEndDate))

## Remove active previous year
df %<>% {
  # 1. Identify all columns that follow the ActiveinYYYY pattern
  active_cols <- grep("^Activein\\d{4}$", names(.), value = TRUE)
  if (length(active_cols) > 0) {
    # 2. Extract years as numbers
    active_years <- as.numeric(str_extract(active_cols, "\\d{4}"))
    latest_year <- max(active_years, na.rm = TRUE)
    
    # 3. Keep all columns except those ActiveinYYYY older than (latest_year - 1)
    keep_cols <- names(.)[
      !grepl("^Activein\\d{4}$", names(.)) |
        as.numeric(str_extract(names(.), "\\d{4}")) >= latest_year - 1
    ]
    
    select(., all_of(keep_cols))
  } else {
    .
  }
}

# Save output ----
write.csv(df, "data/arr/pr/pr historical cleaned.csv", row.names = FALSE)

rm(list = ls())