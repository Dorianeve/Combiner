# CR Flow - 04 - Export subset ----
# File to output for GM
# TO fix

## Prep env ----
source("config.yml")
source("requirements/libraries.R")

## Load current root ----
clean <- read.csv("data/arr/cr/cr cleaned root.csv", encoding = "UTF-8")

# GM Grants Management version ----
# Load report tracker
file_path <- retired_report_tracker

# Read the sheet called "Results reports"
rt <- read_xlsx(file_path, sheet = "Grants DB")

# Identifying the table
names(rt) <- rt[3,]
rt <- rt[-(1:3), ]
rt <- rt[ ,-(35:65)]

rt %<>%
  select(`Programme ID`, `GM GRN`, `Reporting role`,
         `GM check status`)
# rt %<>%
#   group_by(`Programme ID`) %>%
#   filter(
#     `GM check status` == "4- Completed" |
#       (`GM check status` == "6- Non-lead" &
#          any(`Reporting role` == "Lead" & `GM check status` == "4- Completed"))
#   ) %>%
#   ungroup() %>%
#   select(`GM GRN`) %>% distinct()

# Get the list of completed reporting grants
rt %<>%
  group_by(`Programme ID`) %>%
  filter(
    `GM Status` == "Completed" ) %>%
  ungroup() %>%
  select(`GM GRN`) %>% distinct()

# Filter CR clean based on the list
cleanGM <- clean %>%
  filter(GMGRN %in% rt$`GM GRN`)

## Add Cumulative / Annual ----
cleanGM %<>%
  mutate(ARR24Annual = "Yes",
         ARR24Cumulative = "Yes")

cleanGM %<>%
  pivot_longer(
    cols = c(ARR24Cumulative, ARR24Annual),  # columns to unpivot
    names_to = "Exercise",                  # new column for former column names
    values_to = "Value"                      # new column for their values
  )

cleanGM %<>%
  mutate(Exercise = case_when(
    grepl("cumulative", Exercise, ignore.case = TRUE) ~ "ARR24 Cumulative",
    grepl("annual", Exercise, ignore.case = TRUE) ~ "ARR24 Annual",
    TRUE ~ Exercise
  ))

cleanGM %<>%
  select(-c(Value))

## Import and bind the old bombiner ----
old_combiner <- read.csv("data/cr historical cleaned.csv", encoding = "UTF-8")

cleanGM <- bind_rows(cleanGM, old_combiner)


## Save output ----
write.csv(cleanGM, paste0("data/arr/cr/cleaned/", today(),"_cr cleaned GM.csv"), row.names = FALSE)


# For MnE Tentative ----
# Get the list of the new reports received for filtering
file_path <- report_approval


# Read the sheet called "Results reports"
ra <- read_xlsx(file_path, sheet = report_approval_sheet)
names(ra) <- ra[3,]
ra <- ra[-(1:3), ]

# Select relevant columns
ra %<>%
  select(`Programme ID`, `Type of report`, ME_status_tentative, `Type of investment`)

# Filter tentative grants
ra %<>%
  filter(`Type of investment` != "AF" &
           (`Type of report` == "Annual results" | `Type of report` == "Final results") &
           (ME_status_tentative == "Approved" | ME_status_tentative  == "Tentative"))


# Filter clean based on the list
cleanMnEtentative <- clean %>%
  filter(ProgrammeID %in% ra$`Programme ID`)

## Add Cumulative / Annual ----
cleanMnEtentative %<>%
  mutate(ARR24Annual = "Yes",
         ARR24Cumulative = "Yes")

cleanMnEtentative %<>%
  pivot_longer(
    cols = c(ARR24Cumulative, ARR24Annual),  # columns to unpivot
    names_to = "Exercise",                  # new column for former column names
    values_to = "Value"                      # new column for their values
  )

cleanMnEtentative %<>%
  mutate(Exercise = case_when(
    grepl("cumulative", Exercise, ignore.case = TRUE) ~ "ARR24 Cumulative",
    grepl("annual", Exercise, ignore.case = TRUE) ~ "ARR24 Annual",
    TRUE ~ Exercise
  ))

cleanMnEtentative %<>%
  select(-c(Value))

## Import and bind old combiner ----
old_combiner <- read.csv("data/arr/cr/cr historical cleaned.csv", encoding = "UTF-8")

# Bind / old + new
cleanMnEtentative <- bind_rows(cleanMnEtentative, old_combiner)

## Save output ----
write.csv(cleanMnEtentative, paste0("data/arr/cr/cleaned/", today(),"_cr cleaned MnE tentative.csv"), row.names = FALSE)

rm(list = ls())