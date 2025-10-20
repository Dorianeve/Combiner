
source("scripts/aoe/aoe_kobo_processdata.R")


# 1. Identify all cleaned sub-objectives starting with 'wow'
wow_sheets <- ls(pattern = "^wow.*_clean$")

# 2. Prepare a list to hold results
wow_results <- vector("list", length = length(wow_sheets))
names(wow_results) <- paste0(wow_sheets, "_word")

for (sheet in wow_sheets) {
  sub_df <- get(sheet, envir = .GlobalEnv)
  
  # Derive prefix (e.g. 'wowcoord' from 'wowcoord1_clean')
  prefix <- str_extract(sheet, "^wow[a-z]+")
  
  # Core vars to always select from df_clean
  core_vars <- c("index", "validate", "subtime", "country", "investment_type", "pid")
  
  merged <- df_clean %>%
    # A) select core + all columns with that prefix
    select(all_of(core_vars), starts_with(paste0(prefix, "_"))) %>%
    # B) strip the prefix_ from those columns
    rename_with(
      .cols = starts_with(paste0(prefix, "_")),
      .fn   = ~ str_remove(.x, paste0("^", prefix, "_"))
    ) %>%
    # C) join with the sub-objective df
    right_join(sub_df, by = "index") %>%
    select(-index)
  
  # D) conditional arrange by 'theme'
  if ("theme" %in% names(merged)) {
    merged <- merged %>% group_by(pid) %>% arrange(theme, .by_group = TRUE) %>% ungroup()
  }
  
  # E) filter
  wow_results[[ paste0(sheet, "_word") ]] <- merged %>%
    filter(subtime > cutoff_date, validate == "not_approved")
}

# Finally, load to global environment
list2env(wow_results, envir = .GlobalEnv)

