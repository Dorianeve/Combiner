# AF Flow - 01 - Joins ----

## Prep env ----
source("config.yml")
source("requirements/libraries.R")

# Load AF and GMS ----
df <- read.csv("data/finance combiner/aligned funding/af source data.csv", encoding = "UTF-8")
grants_db <- read.csv("data/finance combiner/grants_db finance.csv", encoding = "UTF-8")

# Select necessary fields ----
grants_db %<>%
  select(GMGRN, Typeofinvestment, Country, Granteeorganization, ProgrammeID, Startdate, Currentenddate, Covid.19related)

# Join with GMS by LeadGRN == GMGRN ----
df %<>%
  left_join(grants_db, by = c("LeadGRN" = "GMGRN"))

# Alignment score table ----
# 1. Create the reference lookup table
alignment_lookup <- tibble::tribble(
  ~strategy_alignment, ~engagement_with_myrp, ~combination,    ~aligned,
  "No",                "No",                  "NoNo",          "Not aligned",
  "Weak",              "No",                  "WeakNo",        "Not aligned",
  "No",                "Weak",                "NoWeak",        "Not aligned",
  "Strong",            "Weak",                "StrongWeak",    "Strongly aligned",
  "Weak",              "Strong",              "WeakStrong",    "Strongly aligned",
  "Strong",            "Strong",              "StrongStrong",  "Strongly aligned",
  "Weak",              "Weak",                "WeakWeak",      "Weakly aligned",
  "Strong",            "No",                  "StrongNo",      "Weakly aligned",
  "No",                "Strong",              "NoStrong",      "Weakly aligned"
)

alignment_lookup %<>%
  mutate(combination = tolower(trimws(combination))) %>%
  select(-c(strategy_alignment, engagement_with_myrp))

# 2. Join
df <- df %>%
  mutate(
    combination = paste0(
      tolower(trimws(to_what_extent_is_the_programme_aligned_with_the_myrp_strategy)),
      tolower(trimws(to_what_extent_has_there_been_coordination_between_the_myrp_and_the_programme))
    )
  ) %>%
  left_join(alignment_lookup, by = "combination") %>%
  mutate(alignment_score = aligned) %>%
  select(-c(aligned, combination))  # remove the redundant joined column

# SUM Total funding ----
new_col <- "for_new_programmes_only_how_much_of_the_funding_amount_in_usd_is_targeting_the_same_type_of_beneficiaries_as_the_myrp"
pre_col <- "for_pre_existing_programmes_only_how_much_of_the_unspent_funding_amount_in_usd_is_targeting_the_same_type_of_beneficiaries_as_the_myrp"
type_col <- "new_pre_existing_programme_in_relation_to_myrp_start_year"

df <- df %>%
  mutate(
    sum_total_funding = case_when(
      !!sym(type_col) == "New" ~ as.numeric(!!sym(new_col)),
      !!sym(type_col) == "Pre-existing" ~ as.numeric(!!sym(pre_col)),
      TRUE ~ rowSums(across(all_of(c(new_col, pre_col)), ~ as.numeric(.)), na.rm = TRUE)
    )
  )

# Reorder ----
# List of columns in desired order
preferred_order <- c(
  "Typeofinvestment",
  "Country",
  "Granteeorganization",
  "ProgrammeID",
  "LeadGRN",
  "Startdate",
  "Currentenddate",
  "Covid.19related",
  "donor",
  "recipient_agency",
  "programme_name",
  "programme_start_year",
  "programme_end_year",
  "programme_total_funding_in_original_currency",
  "original_currency",
  "programme_total_funding_in_usd",
  "new_pre_existing_programme_in_relation_to_myrp_start_year",
  "for_new_programmes_only_how_much_of_the_funding_amount_in_usd_is_targeting_the_same_type_of_beneficiaries_as_the_myrp",
  "for_pre_existing_programmes_only_how_much_of_the_unspent_funding_amount_in_usd_is_targeting_the_same_type_of_beneficiaries_as_the_myrp",
  "target_group_alignment",  # if this is meant to be included — can replace with target_group_alignment_comments if preferred
  "to_what_extent_is_the_programme_aligned_with_the_myrp_strategy",
  "strategic_alignment_comments",
  "to_what_extent_has_there_been_coordination_between_the_myrp_and_the_programme",
  "coordination_engagement_comments",
  "alignment_score",
  "for_new_programmes_only_to_what_extent_has_the_myrp_contributed_to_leverage_the_new_programme",
  "contribution_to_leveraging_comments",
  "source_of_information",
  "file_name",
  "sum_total_funding"
)

# Reorder df: preferred columns + the rest
df <- df %>%
  select(any_of(preferred_order), everything())


# Save output
write.csv(df, "data/finance combiner/aligned funding/af raw clean.csv", row.names = FALSE)

rm(list = ls())

