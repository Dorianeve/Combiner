rm(list = ls())
# load new data ----
source("scripts/load/load_grants_db.R")
source("scripts/load/load_reporting_windows.R")
source("scripts/load/load_ecw_reports_tracker.R")
source("scripts/load/load_pr_standard_indicators_template.R")
source("scripts/load/load_pr_results_template.R")
source("scripts/load/load_pr_programme_template.R")

# process pr ----
source("scripts/pr/pr_processing_previous_combiner.R")
source("scripts/pr/pr_merge.R")
source("scripts/pr/pr_pivoting.R")
source("scripts/pr/pr_cleaning_outputting.R")

# open ARR flow for checks ----