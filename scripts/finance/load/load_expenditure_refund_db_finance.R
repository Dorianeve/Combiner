# LOAD Expenditure refund DB finance template ----


## Prep env ----
source("config.yml")
source("requirements/libraries.R")

# Define your credentials
email <- username
password <- password

## Send API request ----
# Make the GET request with basic authentication
resp <- GET(
  url = "https://gms.educationcannotwait.org/api/expenditures_refund-db/export",
  authenticate(user = email, password = password)
)

# Check status
print(status_code(resp))

## Clean data ----
# Parse response
df <- content(resp, as = "parsed")

# Parse dates
df <- df %>%
  mutate(across(contains("date"), ~ dmy(.x, quiet = TRUE)))

# Clean headers
names(df) <- trimws(gsub("\\.", "", names(df)))
names(df) <- gsub("\\s+", "", names(df))

# Save output ----
write.csv(df, "data/finance combiner/expenditure_refund_db finance.csv", row.names = FALSE)

rm(list = ls())