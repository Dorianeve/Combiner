# DCM flow - 01 - Joins ----

## Prep env ----
source("config.yml")
source("requirements/libraries.R")

# load DCM, GMS, RW ----
dcm <- read.csv("data/finance combiner/dcm/dcm source data.csv", encoding = "UTF-8")
grants_db <- read.csv("data/finance combiner/grants_db finance.csv", encoding = "UTF-8")
rw <- read.csv("data/arr/rw/reporting_window.csv", encoding = "UTF-8")

## Processing dataframes ----
# Extract GMGRN form GRID
dcm %<>%
  mutate(
    GMGRN = str_extract(
      grid,
      "\\b\\d{2}-[A-Z]{3}(?:-[A-Z0-9]{2,5}){3,}\\b"
    )
  )

# Select necessary fields from GMS
grants_db %<>%
  select(GMGRN, Typeofinvestment, Country, Granteeorganization, ProgrammeID, Startdate, Currentenddate, Covid.19related, Activein2024)

## Join ----
# Join GMS to DCM with the necessary information
dcm %<>%
  left_join(grants_db, by = "GMGRN")

## Second processing ----
#### LocalTransfer variable ----
dcm %<>%
  mutate(LocalTransfer = ifelse(consortium_partner_type %in% 
                                  c("National NGO", "Government", "Local NGO", "In-country Organizations"),
                                "Yes", "No"))

## Select necessary fields ----
dcm %<>%
  select(Typeofinvestment,
         Country, 
         Granteeorganization,
         ProgrammeID, 
         GMGRN,
         Startdate,
         Currentenddate,
         Covid.19related,
         consortium_partner,
         consortium_partner_type,
         total_amount_allocated_to_be_disbursed_to_consortium_partner,
         cumulative_expenditure_by_consortium_partner,
         cumulative_ecw_funds_transferred_to_partner_in_us_since_inception,
         date_last_updated,
         LocalTransfer,
         women_led_organization,
         refugee_led_organization,
         organization_of_persons_with_disabilities,
         comments_remarks,
         working_with_implementing_partners,
         sub_grantee_ip_name_please_list_only_direct_sub_grantees,
         sub_grantee_ip_type,
         source_of_data,
         grid,
         file_name)

## Rename variables ----
dcm %<>%
  rename(Covid19Related = Covid.19related,
         ConsortiumPartner = consortium_partner,
         ConsortiumPartnerType = consortium_partner_type,
         TotalAmountAllocatedToBeDiscursedToConsortiumPartner = total_amount_allocated_to_be_disbursed_to_consortium_partner,
         CumulativeExpenditureByConsortiumPartner = cumulative_expenditure_by_consortium_partner,
         CumulativeECWFundsTransferredToPartnerinUSSinceInception = cumulative_ecw_funds_transferred_to_partner_in_us_since_inception,
         DateLastUpdated = date_last_updated,
         WomenLedOrganization = women_led_organization,
         RefugeeLedOrganization = refugee_led_organization,
         OrganizationOfPWD = organization_of_persons_with_disabilities,
         CommentsRemarks = comments_remarks,
         WorkingWithImplementingPartners = working_with_implementing_partners,
         SubgranteeIPName = sub_grantee_ip_name_please_list_only_direct_sub_grantees,
         SubgranteeIPType = sub_grantee_ip_type,
         Source = source_of_data,
         GRID = grid,
         FileName = file_name)

## Process numeric variables ----
numbers <- c("TotalAmountAllocatedToBeDiscursedToConsortiumPartner",
             "CumulativeExpenditureByConsortiumPartner",
             "CumulativeECWFundsTransferredToPartnerinUSSinceInception")

dcm %<>%
  mutate(across(
    all_of(numbers),
    ~ round(as.numeric(gsub("[^0-9.]", "", .x)), 2),
    .names = "{.col}"
  ))

# Prevent scientific notation when printing
options(scipen = 999)

# Save output ----
write.csv(dcm, "data/finance combiner/dcm/dcm raw clean.csv", row.names = FALSE)


rm(list = ls())
