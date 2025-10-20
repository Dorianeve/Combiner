# LOAD ECW reports tracker ----


## Prep env ----
source("config.yml")
source("requirements/libraries.R")

# Find out the new file
file_path <- retired_report_tracker

## Open the file ----
# Read the sheet called "Grants DB"
df <- read_excel(file_path, sheet = "Grants DB")


## Processing ----
# Set header and clean
names(df) <- df[3,]
df <- df[-(1:3), ]
df <- df[ ,-(35:65)]

# Basic cleaning
df %<>%
  mutate(across(
    everything(),
    ~ ifelse(
      suppressWarnings(!is.na(as.numeric(.x))),
      format(as.Date(as.numeric(.x), origin = "1899-12-30"), "%Y-%m-%d"),
      .x
    )
  ))

# Rename ProgrammeID
df %<>%
  rename(ProgrammeID = `Programme ID`)

## Reporting dates pivoting ----
# Pivot to get report dates
df %<>%
  pivot_longer(
    cols = c(`Annual results report 01`, `Annual results report 02`,
             `Annual results report 03`, `Annual results report 04`,
             `Annual results report 05`, `Final results report`),  # columns to unpivot
    names_to = "Attribute",                  # new column for former column names
    values_to = "Reportduedate"                      # new column for their values
  )


# Filter report not needed
df %<>%
  filter(Reportduedate != "Not needed")

# Cleaning of values in Typeofreport
df %<>%
  mutate(Typeofreport = case_when(
    grepl("final", Attribute, ignore.case = TRUE) ~ "Final results",
    grepl("annual", Attribute, ignore.case = TRUE) ~ "Annual results",
    TRUE ~ NA
  ))


## Load reporting windows for join ----
rw <- read.csv("data/arr/rw/reporting_window.csv", encoding = "UTF-8")

# Filter
rw %<>%
  filter(report_year != "Application")

# Slice first to get most recent reporting period
rw %<>%
  group_by(ProgrammeID, GMGRN) %>%
  arrange(desc(reporting_period_up_to)) %>%
  slice(1) %>%
  select(ProgrammeID,GMGRN, reporting_period_up_to)

# Join
df %<>% 
  left_join(rw, by = c("GM GRN"= "GMGRN"))

# LatestApprovedRW
df %<>%
  mutate(LatestApprovedRW = case_when(
    !is.na(reporting_period_up_to) ~ reporting_period_up_to,
    TRUE ~ "No"
  ))

## Annual / cumulative ----
df %<>%
  mutate(ARR24Annual = case_when(
    reporting_period_up_to >= "2024-01-01" ~ "Yes",
    TRUE ~ "No"
  ))

df %<>%
  mutate(ARR24Cumulative = case_when(
    reporting_period_up_to >= "2024-01-01" ~ "Yes",
    TRUE ~ "No"
  ))

## Save ----
write.csv(df, "data/arr/reports_tracker.csv", row.names = FALSE)

rm(list = ls())
