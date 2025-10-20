rm(list = ls())

source("scripts/finance/load/load_grants_db_finance.R")
source("scripts/load/load_reporting_windows.R")
source("scripts/finance/load/load_dcm_template.R")
source("scripts/finance/dcm/01_dcm_joins.R")

# DCM overview for the creation of the complete file
# Precondition for this script, is the execution of the following files in load:
# load_allocations_db_finance.R 
# load_expenditure_refund_db_finance.R 
# load_finance_db.R 
# load_finance_expenditure_overview.R
# source("scripts/finance/dcm/02_dcm_overview.R")