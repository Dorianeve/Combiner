# CR Flow - 01 - Joins ----


## Prep env ----
source("config.yml")
source("requirements/libraries.R")

## Load CR, GMS, RW ----
cr <- read.csv("data/arr/cr/cr source data.csv", encoding = "UTF-8")
grants_db <- read.csv("data/arr/grants_db.csv", encoding = "UTF-8")
rw <- read.csv("data/arr/rw/reporting_window.csv", encoding = "UTF-8")

## General preps for joining CR / GMS ----
# Fixing the LeadGRN for multiple Grantees in CR
cr %<>%
  mutate(grn = case_when(
    grantee == "Mutiple Grantees" ~ LeadGRN,
    TRUE ~ grn
  )
  ) %>%
  mutate(grn = trimws(grn)) # trimws() erases all white spaces at the beginning and end of the strings

# Trimws for GMS before join
grants_db %<>%
  mutate(GMGRN = trimws(GMGRN))


## Join CR and GMS ----
# Join using GRN from CR and GMGRN from GMS
cr %<>%
  left_join(grants_db, by = c("grn" = "GMGRN"))

cr %<>%
  mutate(GMGRN = grn)

### Tweaks post CR-GMS join ----
# Create 'Covid related report' necessary for filtering old data from GMS info
cr %<>%
  mutate(
    covid_19_related_report = if_else(
      str_detect(file_name, "Covid"),
      "Yes",
      "No"
    )
  )


## General preps for joining RW and CR ----
# Create Covid flag into RW using FileName
rw %<>%
  mutate(covid_flag = ifelse(grepl("covid", FileName, ignore.case =  TRUE), "Yes", "No"))

# Select the necessary variables from RW
rw %<>% 
  select(ProgrammeID, reporting_period_up_to, report_year, m_e_review, covid_flag)

# Filter out reporting periods
rw %<>%
  filter(reporting_period_up_to < reporting_window_cutoffdate)

# Get unique RW with Covid variable
rw_unique <- rw %>%
  arrange(desc(reporting_period_up_to)) %>%
  group_by(ProgrammeID, covid_flag) %>%
  slice(1) %>%
  ungroup()

# Create CR Covid flag
cr %<>%
  mutate(covid_flag = ifelse(grepl("covid", file_name, ignore.case =  TRUE), "Yes", "No"))

## Join CR and RW for latest RW with Covid flag and Programme ID ----
cr %<>%
  left_join(rw_unique, by = c("ProgrammeID", "covid_flag"))

## Prep and clean up for saving ----
cr  %<>%
  select(ProgrammeID,
         LeadGRN,
         GMGRN,
         Reportingrole,
         Granteeorganization,
         Country,
         Typeofinvestment,
         Startdate,
         Enddate,
         Currentenddate,
         type_of_education, 
         indicator, 
         type_of_beneficiary,
         pre_primary_targeted_female,
         pre_primary_targeted_male,
         primary_targeted_female,
         primary_targeted_male,
         secondary_targeted_female,
         secondary_targeted_male,
         unknown_level_of_education_targeted_female,
         unknown_level_of_education_targeted_male,
         total_female_targeted,
         total_male_targeted,
         total_beneficiaries_targeted,
         pre_primary_reached_female,
         pre_primary_reached_male,
         primary_reached_female,
         primary_reached_male,
         secondary_reached_female,
         secondary_reached_male,
         unknown_level_of_education_reached_female,
         unknown_level_of_education_reached_male,
         total_female_reached,
         total_male_reached,
         total_beneficiaries_reached,
         comments_and_context,
         reporting_stage,
         date_of_reporting,
         GRID,
         tab,
         grn_reference,
         grantee_reference,
         target_line_reference,
         reached_line_reference,
         comments_line_reference,
         file_name,
         Covid.19related,
         covid_19_related_report,
         reporting_period_up_to,
         report_year,
         m_e_review,
         all_of(old_active_year),
         all_of(active_year),
         ActiveStrategicPlan,
         Excomapprovaldate,
         Reportingrole,
         ExportDate)

## Save output ----
write.csv(cr, "data/arr/cr/cr joined.csv", row.names = FALSE)

rm(list = ls())
