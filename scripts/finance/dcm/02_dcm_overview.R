# DCM flow - 02 - Overview ----

## Prep env ----
source("config.yml")
source("requirements/libraries.R")



# load files ----
dcm <- read.csv("data/finance combiner/dcm/dcm raw clean.csv", encoding = "UTF-8")
grants_db <- read.csv("data/finance combiner/grants_db finance.csv", encoding = "UTF-8")
allocations_db <- read.csv("data/finance combiner/allocations_db finance.csv", encoding = "UTF-8")
exp_refund_db <- read.csv("data/finance combiner/expenditure_refund_db finance.csv", encoding = "UTF-8")
finance_db <- read.csv("data/finance combiner/finance_db.csv", encoding = "UTF-8")
fin_exp_overview <- read.csv("data/finance combiner/finance_expenditure_overview.csv", encoding = "UTF-8")


# base grants DB ----
grants_db %<>%
  select(
    Typeofinvestment,
    Country,
    Granteeorganization,
    GMGRN, 
    ProgrammeID,
    Startdate,
    Currentenddate, 
    ExComapproved,
    Excomapprovaldate,
    Covid.19related,
    Activein2024
  )

# Allocations DB prep ----
total_grant_allocations <- allocations_db %>% 
  group_by(GMGRN) %>%
  summarize(TotalGrantAllocation = sum(AmountinUSD, na.rm = TRUE))

# Expenditures Refunds DB prep ----
total_disbursed <- exp_refund_db %>% 
  group_by(GMGRN) %>%
  summarize(TotalDisbursed = sum(AmountinUSD, na.rm = TRUE))

# Finance DB prep ----
total_expenditures <- finance_db %>%
  group_by(GMGRN) %>%
  summarize(Totalexpenditures = sum(MAXExp, na.rm = TRUE))

# Finance Expenditure Overview prep

fin_exp_overview %<>%
  filter(description != "A. Funds received (USD)" &
           description != "Total approved budget")

total_exp_with_cash <- fin_exp_overview %>%
  group_by(GMGRN) %>%
  summarize(TotalExpendituresWithCashEdvances = sum(cumulative_expenditure, na.rm = TRUE))
  

df <- grants_db %>%
  left_join(total_grant_allocations, by = "GMGRN" ) %>%
  left_join(total_disbursed, by = "GMGRN") %>%
  left_join(total_expenditures, by = "GMGRN") %>%
  left_join(total_exp_with_cash, by = "GMGRN")

df <- df %>%
  mutate(
    PerExpenditureVsAllocations = ifelse(
      # Check for division by zero (resulting in Inf or -Inf)
      is.infinite(TotalExpendituresWithCashEdvances / TotalGrantAllocation) |
        # Check for NA results (e.g., NA / X, X / NA, or NA / NA)
        is.na(TotalExpendituresWithCashEdvances / TotalGrantAllocation),
      "", # If an error or NA, set to an empty string
      TotalExpendituresWithCashEdvances / TotalGrantAllocation * 100) # Otherwise, calculate and format as percentage
    )


df <- df %>%
  mutate(
    PerExpenditureVsDisbursed = ifelse(
      # Check for division by zero (resulting in Inf or -Inf)
      is.infinite(TotalExpendituresWithCashEdvances / TotalDisbursed) |
        # Check for NA results (e.g., NA / X, X / NA, or NA / NA)
        is.na(TotalExpendituresWithCashEdvances / TotalDisbursed),
      "", # If an error or NA, set to an empty string
      TotalExpendituresWithCashEdvances / TotalDisbursed * 100) # Otherwise, calculate and format as percentage
  )

# DCM prep ----
WorkingWithIPs <- dcm %>%
  group_by(GMGRN, WorkingWithImplementingPartners) %>%
  select(GMGRN, WorkingWithImplementingPartners) %>%
  unique()

WorkingWithIPs %<>%
  mutate(WorkingWithImplementingPartners = case_when(
    grepl("yes", WorkingWithImplementingPartners, ignore.case = TRUE) ~ "Yes",
    is.na(WorkingWithImplementingPartners) ~ "No template/data",
    TRUE ~ WorkingWithImplementingPartners
  ))

total_transfer_partners <- dcm %>%
  group_by(GMGRN) %>%
  mutate(CumulativeExpenditureByConsortiumPartner = as.numeric(CumulativeExpenditureByConsortiumPartner)) %>%
  summarize(TotalTransferredToAllPartners = sum(CumulativeExpenditureByConsortiumPartner, na.rm = TRUE))

total_transfer_local_partners <- dcm %>%
  filter(ConsortiumPartnerType != "UN Agency" &
           ConsortiumPartnerType != "International NGO") %>%
  group_by(GMGRN) %>%
  mutate(CumulativeExpenditureByConsortiumPartner = as.numeric(CumulativeExpenditureByConsortiumPartner)) %>%
  summarize(TotalTransferredToLocalPartners = sum(CumulativeExpenditureByConsortiumPartner, na.rm = TRUE))

df %<>%
  left_join(WorkingWithIPs, by = "GMGRN") %>%
  left_join(total_transfer_partners, by = "GMGRN") %>%
  left_join(total_transfer_local_partners, by = "GMGRN")

df <- df %>%
  mutate(
    PerTransferredToAll = ifelse(
      # Check for division by zero (resulting in Inf or -Inf)
      is.infinite(TotalTransferredToAllPartners / TotalExpendituresWithCashEdvances) |
        # Check for NA results (e.g., NA / X, X / NA, or NA / NA)
        is.na(TotalTransferredToAllPartners / TotalExpendituresWithCashEdvances),
      "", # If an error or NA, set to an empty string
      TotalTransferredToAllPartners / TotalExpendituresWithCashEdvances * 100) # Otherwise, calculate and format as percentage
  )

df <- df %>%
  mutate(
    PerTransferredToLocalPartners = ifelse(
      # Check for division by zero (resulting in Inf or -Inf)
      is.infinite(TotalTransferredToLocalPartners / TotalExpendituresWithCashEdvances) |
        # Check for NA results (e.g., NA / X, X / NA, or NA / NA)
        is.na(TotalTransferredToLocalPartners / TotalExpendituresWithCashEdvances),
      "", # If an error or NA, set to an empty string
      TotalTransferredToLocalPartners / TotalExpendituresWithCashEdvances * 100) # Otherwise, calculate and format as percentage
  )

df <- df %>%
  mutate(
    PerExpensesTransferredToLocalPartners = ifelse(
      # Check for division by zero (resulting in Inf or -Inf)
      is.infinite(TotalTransferredToLocalPartners / Totalexpenditures) |
        # Check for NA results (e.g., NA / X, X / NA, or NA / NA)
        is.na(TotalTransferredToLocalPartners / Totalexpenditures),
      "", # If an error or NA, set to an empty string
      TotalTransferredToLocalPartners / Totalexpenditures * 100) # Otherwise, calculate and format as percentage
  )

df %<>%
  mutate(TransferToPartner = ifelse(TotalTransferredToAllPartners > 0, "Yes", "No transfer"))

df %<>%
  mutate(CheckTemplate = ifelse(GMGRN %in% dcm$GMGRN | ProgrammeID %in% dcm$ProgrammeID, "Template in SP", "No template"))

org_type <- dcm %>%
  select(GMGRN, ConsortiumPartnerType)

df %<>%
  left_join(org_type, by = "GMGRN") %>%
  mutate(TypeOfOrganization = ConsortiumPartnerType)

# Reorder ----
df %<>%
  select(
    Typeofinvestment,
    Country,
    Granteeorganization,
    GMGRN, 
    ProgrammeID,
    TypeOfOrganization,
    Startdate,
    Currentenddate, 
    ExComapproved,
    Excomapprovaldate,
    Covid.19related,
    Activein2024,
    TotalGrantAllocation,
    TotalDisbursed,
    Totalexpenditures,
    TotalExpendituresWithCashEdvances,
    PerExpenditureVsAllocations,
    PerExpenditureVsDisbursed,
    WorkingWithImplementingPartners,
    TotalTransferredToAllPartners,
    PerTransferredToAll,
    TotalTransferredToLocalPartners,
    PerTransferredToLocalPartners,
    PerExpensesTransferredToLocalPartners,
    TransferToPartner,
    CheckTemplate
  )

write.csv(df, "data/finance combiner/dcm/dcm overview.csv", row.names = FALSE)
