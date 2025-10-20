rm(list = ls())

# load new data ----
source("scripts/finance/load/load_aligned_funding_template.R")
source("scripts/finance/load/load_grants_db_finance.R")
# join with GMS to get the complete Aligned Funding file
source("scripts/finance/aligned funding/01_af_joins.R")