rm(list = ls())

# load new data ----
source("scripts/load/load_grants_db.R")
source("scripts/load/load_reporting_windows.R")
source("scripts/load/load_ecw_reports_tracker.R")
source("scripts/load/load_cr_template.R")

# process cr ----
source("scripts/cr/01_cr_joins.R")
source("scripts/cr/02_cr_processing_previous_combiner.R")
source("scripts/cr/03_cr_pivoting.R")

# open ARR flow for checks ----
