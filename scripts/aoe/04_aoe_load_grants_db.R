library(httr)

# Define your credentials
email = "cmanili@unicef.org"
password = "R8v!m2Xz"

# Make the GET request with basic authentication
resp <- GET(
  url = "https://gms.educationcannotwait.org/api/grants-db/export",
  authenticate(user = email, password = password)
)

# Check status
print(status_code(resp))

# Parse response
df <- content(resp, as = "parsed")

names(df)
str(df)


library(dplyr)
library(lubridate)

df <- df %>%
  mutate(across(contains("date"), ~ dmy(.x, quiet = TRUE)))

names(df) <- trimws(gsub("\\.", "", names(df)))
names(df) <- gsub("\\s+", "", names(df))

df %<>%
  mutate(Activein2023 =
           ifelse(Startdate <= "2023-12-31" &
                    Currentenddate >= "2023-01-01", "Yes", "No"))

df %<>%
  mutate(Activein2024 =
           ifelse(Startdate <= "2024-12-31" &
                    Currentenddate >= "2024-01-01", "Yes", "No"))

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

df %<>% filter(Processingstatus == "Active" | Processingstatus == "Closed")


write.csv(df, "data/arr/aoe/aoe_grants_db.csv", row.names = FALSE)

