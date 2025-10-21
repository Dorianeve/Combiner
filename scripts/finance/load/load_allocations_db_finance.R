# LOAD Allocations DB finance template ----

## Prep env ----
source("config.yml")
source("requirements/libraries.R")

# Define your credentials
email <- username
password <- password

## Send API request ----
# Make the GET request with basic authentication
resp <- GET(
  url = "https://gms.educationcannotwait.org/api/allocations-db/export",
  authenticate(user = email, password = password)
)

# Check status
print(status_code(resp))

# Parse response
df <- content(resp, as = "parsed")

## Clean data ----
# Parse dates
df <- df %>%
  mutate(across(contains("date"), ~ dmy(.x, quiet = TRUE)))

# Clean headers
names(df) <- trimws(gsub("\\.", "", names(df)))
names(df) <- gsub("\\s+", "", names(df))

# Save output ----
dir.create("data/finance combiner/", recursive = TRUE, showWarnings = FALSE)
write.csv(df, "data/finance combiner/allocations_db finance.csv", row.names = FALSE)

rm(list = ls())