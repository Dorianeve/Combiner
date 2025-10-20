source("scripts/aoe/aoe_kobo_processdata.R")

# WOW columns unpivoting ----
wow_df <- df_clean %>%
  select(index, starts_with("wow"))

df_clean %<>%
  select(-c(starts_with("wow")))


wow_df %<>%
  select(-c(ends_with("likert23")))

wow_df <- wow_df %>%
  rename_with(~ gsub("0", "", .)) %>%  # Step 1 → remove _0 in the middle
  rename(wowengage_evavail24 = wowengage1_evavail,
         wowlocal_evavail24 = wowlocal1_evavail) %>%
  rename_with(~ str_replace(., "_likert24$", "_evcalc")) %>%  # Step 2 → rename _likert24 → _evcalc
  rename_with(~ str_replace(., "_likert24_alt$", "_method_rating_gen")) %>%  # Step 3 → rename _likert24_alt → _method_rating_gen
  rename_with(~ str_replace(., "_evavail$", "_evavail24"))  # Step 4 → rename _evavail → _evavail24

wow_dfs_names <- ls(pattern = "^wow.*_clean$")

for (df_name in wow_dfs_names) {
  # Get the dataframe
  df_temp <- get(df_name)
  
  # Extract prefix from df_name (remove "_clean" suffix)
  df_prefix <- sub("_clean$", "", df_name)
  
  # Select columns in wow_df that start with this prefix, plus "index"
  wow_df_subset <- wow_df %>%
    select(index, starts_with(paste0(df_prefix, "_")))
  
  # Perform the left join
  df_joined <- df_temp %>%
    left_join(wow_df_subset, by = "index")
  
  # Now: remove prefix from newly joined columns (but keep index)
  df_joined <- df_joined %>%
    rename_with(
      ~ ifelse(
        grepl("_evavail24$", .), 
        .,  # keep as is
        sub(paste0("^", df_prefix, "_"), "", .)  # remove prefix
      ),
      .cols = starts_with(paste0(df_prefix, "_"))
    )
  # Save back to environment with new name
  assign(df_name, df_joined, envir = .GlobalEnv)
  
  # Optional: message
  message("Joined: ", df_name, 
          " [columns joined: ", paste(colnames(wow_df_subset)[-1], collapse = ", "), "]")
}

#  CHDOTH unpivoting ----
chdoth_clean <- df_clean %>%
  select(index, chdoth_evavail24, chdoth_skill1, chdoth_method) %>%
  mutate(sub_objective_prefix = "chdoth") %>%
  rename(skill1 = chdoth_skill1,
         method = chdoth_method)

# remove these columns from df_clean
df_clean %<>%
  select(-c(chdoth_evavail24, chdoth_skill1, chdoth_method))






# APPENDING PROCESS ---
# 1️⃣ Identify which *_clean dfs to process
dfs_to_merge <- setdiff(ls(pattern = "_clean$"), "df_clean")

# 2️⃣ Prepare list to collect joined dfs
joined_list <- list()

# 3️⃣ Loop through dfs
for (df_name in dfs_to_merge) {
  df_sub <- get(df_name)
  
  # sub_objective_prefix (remove _clean suffix)
  sub_obj_prefix <- sub("_clean$", "", df_name)
  
  # Correct direction: sub df LEFT JOIN df_clean
  df_joined <- df_sub %>%
    left_join(df_clean, by = "index") %>%
    mutate(sub_objective_prefix = sub_obj_prefix)
  
  # Add to list
  joined_list[[df_name]] <- df_joined
  
  message("Joined ", df_name)
}

# 4️⃣ Combine all into one big df
df_all <- bind_rows(joined_list)

# 5️⃣ Optional: check result
glimpse(df_all)


# 1. Subtext lookup table
subtext_lookup <- tribble(
  ~prefix,         ~sub_text,                                     ~indicator_code,
  "eaenroll",      "1.1 | Enrollment",                                "3",
  "eaattend",      "1.2 | Attendance",                                "3",
  "rpcretent",     "1.3 | Retention",                                 "4",
  "rpcprogress",   "1.4 | Progression",                               "4",                        
  "rpccomplete",   "1.5 | Completion",                                "4",
  "transit",       "1.6 | Transition",                                "5",
  "hloacd",        "2.1 | Academic",                                  "6",
  "hlosel",        "2.2 | Social and emotional",                      "7",
  "hlogen",        "2.4 | Gender outcomes",                           "8",
  "chdoth",        "2.5 | Other children’s outcomes",                 NA,
  "osteach",       "3.1 | Teacher outcomes",                          NA,
  "osschool",      "3.2 | School and classroom outcomes",             NA,
  "ospolicy",      "3.3 | National policy and systems",               NA,
  "osother",       "3.4 | Other opportunities and services",          NA,
  "wowcoord1",     "4.2 | Coordination - Programmatic",               "11",
  "wowcoord2",     "4.2 | Coordination - Sector",                     "11",
  "wowdata1",      "4.3 | Data - Programmatic - outcomes",            "17",
  "wowdata2",      "4.3 | Data - Programmatic – marginalized groups", "17",
  "wowdata3",      "4.3 | Data - Sector",                             "17",
  "wowlocal",      "4.1 | Localization",                              "13",
  "wowengage",     "4.4 | Accountability",                            "14"
)


df_all$sub_objective <- sapply(df_all$sub_objective_prefix, function(obj) {
  matched <- subtext_lookup %>%
    filter(prefix == obj) %>%
    slice(1)
  matched$sub_text %||% NA_character_
})


df_all$indicator <- sapply(df_all$sub_objective_prefix, function(obj) {
  matched <- subtext_lookup %>%
    filter(prefix == obj) %>%
    slice(1)
  matched$indicator_code %||% NA_character_
})


df <- df_all %>%
  mutate(sub_objective = as.character(sub_objective),
         indicator = as.character(indicator))


# FLAGS ----
groups_flag <- list(
  eaenroll = "group1",
  eaattend = "group1",
  rpcretent = "group1",
  rpcprogress = "group1",
  rpccomplete = "group1",
  transit = "group1",
  hloacd = "group2",
  hlosel = "group2",
  hlogen = "group2",
  wowcoord1 = "group3",
  wowcoord2 = "group4",
  wowdata1 = "group5",
  wowdata2 = "group6",
  wowdata3 = "group7",
  wowlocal = "group8",
  wowengage = "group8"
)




# 1️⃣ Add "group" column → mapped from sub_objective_prefix
df <- df %>%
  mutate(group = sapply(sub_objective_prefix, function(x) groups_flag[[x]]))

# 2️⃣ For each rule → create flag columns

# Rule 1 → Groups 1 & 2 → flag_evavail24
df <- df %>%
  mutate(
    flag_evavail24 = case_when(
      group %in% c("group1", "group2") &
        across(ends_with("_evavail24"), ~ .x %in% c(NA, "Unsure")) %>%
        pmap_lgl(any) ~ TRUE,
      TRUE ~ FALSE
    )
  )

# # Rule 2 → Groups 1 & 2 → flag_include_count
# df <- df %>%
#   rowwise() %>%
#   mutate(
#     include_count = sum(c_across(ends_with("_include_01") | ends_with("_include_02") | ends_with("_include_03") | ends_with("_include_04") | ends_with("_include_05")), ~ !is.na(.))),
# evnumb_value = first(c_across(ends_with("_evnumb"))),
# flag_include_count = if_else(group %in% c("group1", "group2") & !is.na(evnumb_value) & include_count != evnumb_value, TRUE, FALSE)
# ) %>%
#   ungroup()

# Rule 3 → Group 8 → flag_evavail24_group8
df <- df %>%
  mutate(
    flag_evavail24_group8 = case_when(
      group == "group8" &
        across(ends_with("_evavail24"), ~ .x %in% c(NA, "Unsure")) %>%
        pmap_lgl(any) ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Rule 4 → Groups 3-7 → flag_evavail_myrp + flag_evchange_myrp

# Find columns dynamically
evavail_cols  <- grep("_evavail$", names(df), value = TRUE)
# For evchange → your column is simply "evchange"
evchange_cols <- grep("^evchange$", names(df), value = TRUE)

df <- df %>%
  mutate(
    flag_evavail_myrp = case_when(
      group %in% c("group3", "group4", "group5", "group6", "group7") &
        investment_type == "MYRP" & length(evavail_cols) > 0 ~
        apply(select(., all_of(evavail_cols)), 1, function(x) any(x %in% c(NA, "Unsure"))),
      group %in% c("group3", "group4", "group5", "group6", "group7") &
        investment_type == "MYRP" ~ FALSE,
      TRUE ~ FALSE
    ),
    flag_evchange_myrp = case_when(
      group %in% c("group3", "group4", "group5", "group6", "group7") &
        investment_type == "MYRP" & length(evchange_cols) > 0 ~
        apply(select(., all_of(evchange_cols)), 1, function(x) any(x %in% c(NA, "Rating not provided"))),
      group %in% c("group3", "group4", "group5", "group6", "group7") &
        investment_type == "MYRP" ~ FALSE,
      TRUE ~ FALSE
    )
  )

# Rule 5 → Group 8 → flag_likert_group8
# Rule 5 → Group 8 → flag_likert_group8 (safe version)
likert_cols <- grep("_likert", names(df), value = TRUE)

df <- df %>%
  mutate(
    flag_likert_group8 = case_when(
      group == "group8" & length(likert_cols) > 0 ~
        apply(select(., all_of(likert_cols)), 1, function(x) any(x %in% c(NA, "Rating not provided"))),
      group == "group8" ~ FALSE,
      TRUE ~ FALSE
    )
  )

# DONE 🚀

# ORDER ----
# Dynamic matching of columns
evnumb_cols    <- grep("_evnumb($|_)", names(df), value = TRUE)
evavail23_cols <- grep("_evavail23$", names(df), value = TRUE)
evavail24_cols <- grep("_evavail24$", names(df), value = TRUE)

# Build desired order
desired_order <- c(
  "index",
  "repeatindex",
  "validate",
  "name_selected",
  "country",
  "investment_type",
  "pid",
  "active23_man",
  "indicator",
  "sub_objective_prefix",
  "sub_objective",
  evnumb_cols,
  evavail23_cols,
  evavail24_cols,
  "skill1"
)

# Reorder safely
existing_order <- desired_order[desired_order %in% names(df)]

df <- df %>%
  select(all_of(existing_order), everything())

# VLOOKUPS ----
## evnumb ----
# 1️⃣ Define mapping
evnumb_mapping <- list(
  eaenroll    = "eaenroll_evnumb",
  eaattend    = "eaattend_evnumb",
  rpcretent   = "rpcretent_evnumb",
  rpcprogress = "rpcprogress_evnumb",
  rpccomplete = "rpccomplete_evnumb",
  transit     = "transit_evnumb",
  hloacd      = "hloacd_evnumb",
  hlosel      = "hlosel_evnumb",
  hlogen      = "hlogen_evnumb",
  wowcoord1   = "wowcoord1_evnumb",
  wowcoord2   = "wowcoord2_evnumb",
  wowdata1    = "wowdata1_evnumb",
  wowdata2    = "wowdata2_evnumb",
  wowdata3    = "wowdata3_evnumb",
  wowengage   = "wowengage_evnumb",
  wowlocal    = "wowlocal_evnumb"
)

# 2️⃣ Add new column
df <- df %>%
  rowwise() %>%
  mutate(
    evnumb = ifelse(
      !is.null(evnumb_mapping[[sub_objective_prefix]]) &&
        evnumb_mapping[[sub_objective_prefix]] %in% names(df),
      get(evnumb_mapping[[sub_objective_prefix]]),
      NA_character_
    )
  ) %>%
  ungroup()


# 3️⃣ Determine columns to delete
cols_to_delete <- intersect(unique(unlist(evnumb_mapping)), names(df))

# Optional: print message
message("Deleting columns: ", paste(cols_to_delete, collapse = ", "))

# 4️⃣ Remove them
df <- df %>%
  select(-all_of(cols_to_delete))

## evavail23 ----
evavail23_mapping <- list(
  eaenroll    = "eaenroll_evavail23",
  eaattend    = "eaattend_evavail23",
  rpcretent   = "rpcretent_evavail23",
  rpcprogress = "rpcprogress_evavail23",
  rpccomplete = "rpccomplete_evavail23",
  transit     = "transit_evavail23",
  hloacd      = "hloacd_evavail23",
  hlosel      = "hlosel_evavail23",
  hlogen      = "hlogen_evavail23",
  wowcoord1   = "wowcoord1_evavail23",
  wowcoord2   = "wowcoord2_evavail23",
  wowdata1    = "wowdata1_evavail23",
  wowdata2    = "wowdata2_evavail23",
  wowdata3    = "wowdata3_evavail23",
  wowengage   = "wowengage_evavail23",
  wowlocal    = "wowlocal_evavail23"
)

# 2️⃣ Add new column
df <- df %>%
  rowwise() %>%
  mutate(
    evavail23 = ifelse(
      !is.null(evavail23_mapping[[sub_objective_prefix]]) &&
        evavail23_mapping[[sub_objective_prefix]] %in% names(df),
      get(evavail23_mapping[[sub_objective_prefix]]),
      NA_character_
    )
  ) %>%
  ungroup()


# 3️⃣ Determine columns to delete
cols_to_delete <- intersect(unique(unlist(evavail23_mapping)), names(df))

# Optional: print message
message("Deleting columns: ", paste(cols_to_delete, collapse = ", "))

# 4️⃣ Remove them
df <- df %>%
  select(-all_of(cols_to_delete))

## evavail24 ----
evavail24_mapping <- list(
  eaenroll    = "eaenroll_evavail24",
  eaattend    = "eaattend_evavail24",
  rpcretent   = "rpcretent_evavail24",
  rpcprogress = "rpcprogress_evavail24",
  rpccomplete = "rpccomplete_evavail24",
  transit     = "transit_evavail24",
  hloacd      = "hloacd_evavail24",
  hlosel      = "hlosel_evavail24",
  hlogen      = "hlogen_evavail24",
  chdoth      = "chdoth_evavail24",
  wowcoord1   = "wowcoord1_evavail24",
  wowcoord2   = "wowcoord2_evavail24",
  wowdata1    = "wowdata1_evavail24",
  wowdata2    = "wowdata2_evavail24",
  wowdata3    = "wowdata3_evavail24",
  wowengage   = "wowengage_evavail24",
  wowlocal    = "wowlocal_evavail24"
)

# 2️⃣ Add new column
df <- df %>%
  rowwise() %>%
  mutate(
    evavail24 = ifelse(
      !is.null(evavail24_mapping[[sub_objective_prefix]]) &&
        evavail24_mapping[[sub_objective_prefix]] %in% names(df),
      get(evavail24_mapping[[sub_objective_prefix]]),
      NA_character_
    )
  ) %>%
  ungroup()

# 3️⃣ Determine columns to delete
cols_to_delete <- intersect(unique(unlist(evavail24_mapping)), names(df))

# Optional: print message
message("Deleting columns: ", paste(cols_to_delete, collapse = ", "))

# 4️⃣ Remove them
df <- df %>%
  select(-all_of(cols_to_delete))

# Build desired order
desired_order <- c(
  "index",
  "repeatindex",
  "validate",
  "name_selected",
  "country",
  "investment_type",
  "pid",
  "active23_man",
  "indicator",
  "sub_objective_prefix",
  "sub_objective",
  "evnumb",
  "ea_evnumb",  
  "rpc_evnumb", 
  "evavail23", 
  "evavail24"
)



# Reorder safely
existing_order <- desired_order[desired_order %in% names(df)]

df <- df %>%
  select(all_of(existing_order), everything())

# Move all columns starting with flag_ to the end
flag_cols <- grep("^flag_", names(df), value = TRUE)
other_cols <- setdiff(names(df), flag_cols)

df <- df %>%
  select(all_of(other_cols), all_of(flag_cols))

# fix the text ----
df %<>%
  mutate(text = ifelse(is.na(text), text2, text)) %>%
  select(-c(text2))

df %<>%
  mutate(group = as.character(group))

write.csv(df, "aoe_kobo_clean_tentative.csv", row.names = FALSE)
