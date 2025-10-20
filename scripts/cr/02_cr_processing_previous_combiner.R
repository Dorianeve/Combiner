# CR Flow - 02 - Processing previous combiner ----


## Prep env ----
source("config.yml")
source("requirements/libraries.R")

## Load files ----
# from env variables
old <- read.csv(old_cr_combiner, encoding = "UTF-8")
db <- read.csv("data/arr/grants_db.csv", encoding = "UTF-8")

## Select GMS relevant columns ----
db %<>%
  select(GMGRN, ProgrammeID, all_of(active_year), ActiveStrategicPlan, Reportingrole)

## Process old combiner ----
# Get correct GMGRN
old %<>%
  mutate(GMGRN = ifelse(GMGRN == "Mutiple Grantees", LeadGRN, GMGRN))

old %<>%
  mutate(GMGRN = ifelse(GMGRN == "N/A", LeadGRN, GMGRN))

# Delete old_combiner columns to be updated
old %<>%
  select(-c(ActiveStrategicPlan, Reportingrole))

## Join old combiner with GMS ----
# This is necessary to get PID and other information (till 2023 combiner)
old %<>%
  left_join(db, by = c("ProgrammeID", "GMGRN"))

## Active in variable ----
old %<>%
  mutate(!!active_year := ifelse(is.na(.data[[active_year]]), "No", .data[[active_year]]))

## Process to create Cumulative and Annual ----
# This duplicates the non active previous programs and tages them as Cumulative current year
subset <- old %>%
  filter(Exercise == old_exercise_cumulative)

# copying grants no longer active or active with covid data (this data goes in another template)
subset %<>%
  filter(
    .data[[active_year]] == "No" |
      (.data[[active_year]] == "Yes" & Covid19relateddata == "Covid-19 related data")
  )

subset %<>%
  filter(Exercise == old_exercise_cumulative)

subset %<>%
  mutate(Exercise = exercise_cumulative)

df <- rbind(old, subset)

## Final cleaning ----
# Parsing numbers
df$Number <- as.numeric(gsub(",", "", df$Number))

# # Rename to match clean (this is positional, so DO NOT add columns, otherwise change of appproach is needed)
# names(df) <- c(
#   "LeadGRN",
#   "GMGRN",
#   "Typeofinvestment",
#   "Country",
#   "Granteeorganization",
#   "Startdate",
#   "Currentenddate",
#   "Covid19relatedgrant",
#   "Covid19relateddata",
#   "Typeofreporting",
#   "Typeofeducation",
#   "Indicator",
#   "Status",
#   "Typeofbeneficiary",
#   "Levelofeducation",
#   "Gender",
#   "Number",
#   "Commentsandcontext",
#   "Dateofreporting",
#   "Sourceofdata",
#   "Exercise",
#   "MnEReview",
#   "GRID",
#   "Filename",
#   old_active_year,
#   "ProgrammeID",
#   active_year,
#   "ActiveStrategicPlan",
#   "Reportingrole"
# )


## Filter Covidrelateddata variable ----
df %<>% filter(Covid19relateddata == "Non-Covid-19 related data" | 
                (Covid19relateddata == "Covid-19 related data" & grepl("cumulative", Exercise, ignore.case = TRUE)))

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
write.csv(df, "data/arr/cr/cr historical cleaned.csv", row.names = FALSE)

rm(list = ls())
