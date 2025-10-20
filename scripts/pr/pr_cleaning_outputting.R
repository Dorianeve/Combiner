# PR Flow - 04 - Cleaning outputting ----


## Prep env ----
source("config.yml")
source("requirements/libraries.R")

## Load latest _pr cleaned.csv ----
folder <- "data/arr/pr/cleaned/"
pattern <- "_pr cleaned.csv$"

# Get the latest file based on date in the filename
latest_file <- list.files(path = folder, pattern = pattern, full.names = TRUE) |>
  sort(decreasing = TRUE) |>
  (\(x) x[1])()

# Read the CSV
df <- read.csv(latest_file)

## Cleaning ----
### Dates ----
df <- df %>%
  mutate(
    DateIndicatorProposed = case_when(
      grepl("^[0-9]+$", as.character(DateIndicatorProposed)) ~ 
        as.Date(as.numeric(DateIndicatorProposed), origin = "1899-12-30"),
      TRUE ~ as.Date(DateIndicatorProposed)
    )
  )


df <- df %>%
  mutate(
    ReportEndDate = case_when(
      grepl("^[0-9]+$", as.character(ReportEndDate)) ~ 
        as.Date(as.numeric(ReportEndDate), origin = "1899-12-30"),
      # US-style MM/DD/YYYY
      grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", as.character(ReportEndDate)) ~ 
        mdy(ReportEndDate),
      TRUE ~ as.Date(ReportEndDate)
    )
  )

### Value ----
df %<>%
  mutate(Value = as.numeric(Value))

### AnalysisCode ----
df <- df %>%
  mutate(
    AnalysisCode = trimws(AnalysisCode),
    AnalysisCode = na_if(AnalysisCode, ""),
    AnalysisCode = tolower(AnalysisCode),
    
    # Standardize known variants
    AnalysisCode = recode(AnalysisCode,
                          "do not count" = "do_not_count",
                          "do not cout"  = "do_not_count",
                          "do_not count" = "do_not_count",
                          
                          "enter code" = "enter_code",
                          
                          "no indicator" = "no_indicator",
                          
                          "e46 drr" = "e46_drr",
                          "e46 gender" = "e46_gender",
                          "e46 learning" = "e46_learning",
                          "e46 mhpss" = "e46_mhpss",
                          "e46 wash" = "e46_wash",
                          "e46 climate" = "e46_climate",
                          "z4 climate" = "z4_climate"
    ),
    
    # Catch any value starting with "outlier" and standardize it
    AnalysisCode = ifelse(str_starts(AnalysisCode, "outlier"), "outlier_incorrect", AnalysisCode)
  )

### Disaggregated by sex -----
df <- df %>%
  mutate(
    DisaggregatedBySex = trimws(DisaggregatedBySex),       # remove extra spaces
    DisaggregatedBySex = tolower(DisaggregatedBySex),      # make lowercase
    DisaggregatedBySex = na_if(DisaggregatedBySex, ""),     # convert "" to NA
    DisaggregatedBySex = case_when(
      DisaggregatedBySex %in% c("yes", "y", "yesd", "yes ", "yesy", "yess") ~ "yes",
      DisaggregatedBySex %in% c("no", "no indicator", "n", "yno") ~ "no",
      TRUE ~ DisaggregatedBySex  # preserve other values (e.g. NA)
    )
  )

### Unit ----
df <- df %>%
  mutate(
    UnitOfMeasurement = trimws(UnitOfMeasurement),
    UnitOfMeasurement = na_if(UnitOfMeasurement, ""),  # convert empty to NA
    UnitOfMeasurement = na_if(UnitOfMeasurement, "N/A"),        # "N/A" → NA
    UnitOfMeasurement = case_when(
      UnitOfMeasurement %in% c("yes/nos", "yes/no", "Yes (1), No (0)", "Yes/No") ~ "Yes/No",
      UnitOfMeasurement %in% c("number", "Number", "Number #") ~ "# Number",
      grepl("^us\\$?$", tolower(UnitOfMeasurement)) ~ "US$",
      UnitOfMeasurement %in% c("%", "percentage", "Percentage %", "% Percentage") ~ "% Percentage",
      TRUE ~ UnitOfMeasurement
    )
  )

# Save output (xlsx) ----
write.xlsx(df, file = "data/arr/pr/cleaned/pr_clean.xlsx", overwrite = TRUE)

rm(list = ls())