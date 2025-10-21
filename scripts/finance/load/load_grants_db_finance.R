# LOAD GMS for AF and DCM ----

## Prep env ----
source("config.yml")
source("requirements/libraries.R")

# Define your credentials ----
email <- username
password <- password

# API request ----
## GET request ----
resp <- GET(
  url = "https://gms.educationcannotwait.org/api/grants-db/export",
  authenticate(user = email, password = password)
)

# Check status
print(status_code(resp))

## Parse response ----
df <- content(resp, as = "parsed")

## Basic cleaning ----
# Convert dates
df <- df %>%
  mutate(across(contains("date"), ~ dmy(.x, quiet = TRUE)))

# Work on headers
names(df) <- trimws(gsub("\\.", "", names(df)))
names(df) <- gsub("\\s+", "", names(df))

## Activein2023 variable ----
df %<>%
  mutate(Activein2023 =
           ifelse(Startdate <= "2023-12-31" &
                    Currentenddate >= "2023-01-01", "Yes", "No"))

## Activein2024 variable ----
df %<>%
  mutate(Activein2024 =
           ifelse(Startdate <= "2024-12-31" &
                    Currentenddate >= "2024-01-01", "Yes", "No"))

## Crisis context and type of emergency variables ----
df %<>%
  mutate(Crisis_Protracted = ifelse(grepl("Protracted crisis", Crisiscontext, ignore.case = TRUE), "Yes", "No"),
         Crisis_New_sudden_onset = ifelse(grepl("New or sudden on-set emergency", Crisiscontext, ignore.case = TRUE), "Yes", "No"),
         Crisis_Escalation = ifelse(grepl("Escalation of an existing crisis", Crisiscontext, ignore.case = TRUE), "Yes", "No"),
         Crisis_New_displacement = ifelse(grepl("New displacement of populations", Crisiscontext, ignore.case = TRUE), "Yes", "No"),
         Crisis_Anticipatory_action = ifelse(grepl("Anticipatory action", Crisiscontext, ignore.case = TRUE), "Yes", "No"),
         Emergency_Conflict = ifelse(grepl("Conflict/violence", Typeofemergency, ignore.case = TRUE), "Yes", "No"),
         Emergency_Natural_disaster = ifelse(grepl("Natural/environmental disaster", Typeofemergency, ignore.case = TRUE), "Yes", "No"),
         Emergency_Climate = ifelse(grepl("Climate", Typeofemergency, ignore.case = TRUE), "Yes", "No"),
         Emergency_Public_health = ifelse(grepl("Public health", Typeofemergency, ignore.case = TRUE), "Yes", "No"),
         Emergency_Economic = ifelse(grepl("Economic", Typeofemergency, ignore.case = TRUE), "Yes", "No"),
         Emergency_Other = ifelse(grepl("Other", Typeofemergency, ignore.case = TRUE), "Yes", "No"))

# Save output ----
dir.create("data/finance combiner", recursive = TRUE, showWarnings = FALSE)
write.csv(df, "data/finance combiner/grants_db finance.csv", row.names = FALSE)

rm(list = ls())
