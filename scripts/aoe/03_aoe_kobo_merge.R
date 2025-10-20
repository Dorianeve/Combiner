source("scripts/aoe/02_aoe_kobo_processdata.R")

# WOW columns unpivoting ----
wow_df <- df_clean %>%
  select(index, starts_with("wow"))

wow_df %<>%
  select(-c(ends_with("likert23"), ends_with("evavail")))

wow_df <- wow_df %>%
  rename_with(~ gsub("0", "", .)) %>%  # Step 1 → remove _0 in the middle
  rename_with(~ str_replace(., "_likert24$", "_evcalc")) %>%  # Step 2 → rename _likert24 → _evcalc
  rename_with(~ str_replace(., "_likert24_alt$", "_method_rating_gen"))  # Step 3 → rename _likert24_alt → _method_rating_gen

wow_dfs_names <- ls(pattern = "^wow.*_clean$")

for (df_name in wow_dfs_names) {
  df_temp <- get(df_name)
  df_base <- sub("_clean$", "", df_name)
  df_prefix <- sub("\\d+$", "", df_base)
  df_prefix_full <- paste0(df_prefix, "_")
  
  wow_df_subset <- wow_df %>%
    select(index, starts_with(df_prefix_full))
  
  df_joined <- df_temp %>%
    left_join(wow_df_subset, by = "index")
  
  # DEBUG: which columns will be renamed?
  debug_columns <- grep(paste0("^", df_prefix_full), colnames(df_joined), value = TRUE)
  print(paste("DEBUG - columns to rename in", df_name, ":", paste(debug_columns, collapse = ", ")))
  
  df_joined <- df_joined %>%
    rename_with(
      .fn = ~ paste0(df_base, "_", sub(paste0("^", df_prefix_full), "", .x)),
      .cols = starts_with(df_prefix_full)
    )
  
  assign(df_name, df_joined, envir = .GlobalEnv)
  
  joined_columns <- grep(paste0("^", df_base, "_"), colnames(df_joined), value = TRUE)
  
  message("Joined: ", df_name, 
          " [columns joined: ", paste(joined_columns, collapse = ", "), "]")
}

# clean headers
for (df_name in wow_dfs_names) {
  # Get the dataframe
  df_temp <- get(df_name)
  
  # Extract df_base = df_name without "_clean"
  df_base <- sub("_clean$", "", df_name)
  
  # Now the prefix is exactly df_base + "_"
  df_prefix_full <- paste0(df_base, "_")
  
  # Rename columns → remove this full prefix
  df_temp <- df_temp %>%
    rename_with(
      .fn = ~ sub(paste0("^", df_prefix_full), "", .x),
      .cols = starts_with(df_prefix_full)
    )
  
  # Save back
  assign(df_name, df_temp, envir = .GlobalEnv)
  
  # Message
  message("Cleaned headers in: ", df_name)
}


#  CHDOTH unpivoting ----
chdoth_clean <- df_clean %>%
  select(index, chdoth_skill1, chdoth_method) %>%
  mutate(sub_objective_prefix = "chdoth") %>%
  rename(skill1 = chdoth_skill1,
         method = chdoth_method)

# DF_CLEAN EVAVAIL / EVNUMB PIVOT ----
df_clean %<>%
  select(-c(ends_with("evavail23")))

df_clean %<>%
  rename_with(~ str_replace(., "_evavail24$", "_evavail"))

# First: identify columns to pivot
cols_to_pivot <- names(df_clean) %>%
  str_subset("(_evavail|_evnumb)$") %>%   # match those endings
  setdiff(c("ea_evnumb", "rpc_evnumb"))   # exclude these

# Pivot longer
df_long <- df_clean %>%
  pivot_longer(
    cols = all_of(cols_to_pivot),
    names_to = c("sub_objective_prefix", ".value"),
    names_pattern = "^(.*)_(evavail|evnumb)$"
  )

df_long %<>%
  mutate(sub_objective_prefix = gsub("0", "", sub_objective_prefix)) %>%
  mutate(sub_objective_prefix = case_when(
    sub_objective_prefix == "wowengage1" ~ "wowengage",
    sub_objective_prefix == "wowlocal1" ~ "wowlocal",
    TRUE ~ sub_objective_prefix
  ))
         

df_long %<>%
  mutate(concat_id = paste0(index, "|", sub_objective_prefix))
         
df_long %<>%
  select(-c(starts_with("wow"), starts_with("chdoth")))


# CHECK 1 - Evavail != NA ----
evavailNA <- df_long %>%
  filter(is.na(evavail)) %>%
  select(concat_id, sub_objective_prefix, index, pid, evavail)
evavailNA

# List Evavail YES for checks later ----
lsEvavailYes <- df_long %>%
  filter(evavail == "Yes") %>%
  select(concat_id, sub_objective_prefix, index, pid, evavail)

# Subset of evavail NO for appending ----
df_no <- df_long %>%
  filter(evavail == "No") %>%
  mutate(source_df = "df_evavail_no")

# JOINS ----
# 1️⃣ Identify *_clean dfs to process (excluding df_clean)
dfs_to_merge <- setdiff(ls(pattern = "_clean$"), "df_clean")

# 2️⃣ Prepare list to collect joined dfs
joined_list <- list()

# 3️⃣ Prepare df_long with concat_id - NOT NEEDED AS IT EXISTS ALREADY
# df_long %<>%
#   mutate(concat_id = paste0(index, "|", sub_objective_prefix))

# 4️⃣ Loop through dfs
for (df_name in dfs_to_merge) {
  df_sub <- get(df_name)
  
  # sub_objective_prefix (remove _clean suffix + possible trailing number)
  sub_obj_prefix <- sub("_clean$", "", df_name)
  
  # Create concat_id in df_sub
  df_sub <- df_sub %>%
    mutate(
      sub_objective_prefix = sub_obj_prefix,  # optional if not already present
      concat_id = paste0(index, "|", sub_objective_prefix)
    ) %>%
    select(-c(index, sub_objective_prefix))
  
  # Perform LEFT JOIN on concat_id
  df_joined <- df_sub %>%
    left_join(df_long, by = "concat_id")
  
  # Add to list
  joined_list[[df_name]] <- df_joined
  
  message("Joined ", df_name)
}

df_merged <- bind_rows(joined_list, .id = "source_df")

# check if all PID avail YEs are correctly merged (0 equals GOOD)
evavailYESrepeat <- lsEvavailYes %>%
  filter(!pid %in% df_merged$pid)

# merging subset ----
df_merged <- bind_rows(df_merged, df_no)

# Sub-orjecting and Indicator table ----
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


df_merged$sub_objective <- sapply(df_merged$sub_objective_prefix, function(obj) {
  matched <- subtext_lookup %>%
    filter(prefix == obj) %>%
    slice(1)
  matched$sub_text %||% NA_character_
})


df_merged$indicator <- sapply(df_merged$sub_objective_prefix, function(obj) {
  matched <- subtext_lookup %>%
    filter(prefix == obj) %>%
    slice(1)
  matched$indicator_code %||% NA_character_
})

df <- df_merged %>%
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




# fix the text ----
df %<>%
  mutate(text = ifelse(is.na(text), text2, text)) %>%
  select(-c(text2))

df %<>%
  mutate(group = as.character(group))

# fix the indicator eval ----
df %<>%
  mutate(evnumb_indicator = case_when(
    indicator == 3 ~ ea_evnumb,
    indicator == 4  ~ rpc_evnumb,
    indicator %in% 5:8 ~ evnumb,
    TRUE ~  NA
  )) %>%
  select(-c(ea_evnumb, rpc_evnumb)) %>%
  rename(evnumb_sub_objective = evnumb)


# fix the wow evcalcs ----
df %<>%
  group_by(index, indicator) %>%
  mutate(evcalc = if (first(indicator) %in% c(11, 13, 14, 17)) {
    # Only fill if there is at least one non-NA
    if (all(is.na(evcalc))) {
      evcalc  # return as is — all NA
    } else {
      zoo::na.locf(zoo::na.locf(evcalc, na.rm = FALSE), fromLast = TRUE)
    }
  } else {
    evcalc
  }) %>%
  ungroup()



# ORDER ----
# Build desired order
desired_order <- c(
  "validate",
  "pid",
  "repeatindex",
  "name_selected",
  "country",
  "investment_type",
  "active23_man",
  "indicator",
  "sub_objective",
  "evnumb_indicator",
  "evnumb_sub_objective",
  "evavail",
  "evcalc",
  "skill1",
  "text",
  "base",
  "end",
  "basetype",
  "endtype",
  "basef",
  "basem",
  "endf",
  "endm",
  "match1",
  "matchlab",
  "same",
  "rf1",
  "calc2",
  "calc3",
  "calc4",
  "calc5",
  "setting",
  "pop",
  "georep",
  "method_rating_gen",
  "method_rating_change",
  "method_prog",
  "disag",
  "method",
  "method1",
  "method_sample",
  "method_measure",
  "method9b",
  "cwd",
  "setting2",
  "skill2",
  "theme",
  "themeo",
  "outcome",
  "outcomeo",
  "hlosel_activity",
  "hlogen_activity",
  "climate_1",
  "coh_1",
  "active_child",
  "active_child_clim",
  "active_child_coh",
  "evchange",
  "evstrength",
  "evdirect",
  "group",
  "concat_id",
  "index",
  "sub_objective_prefix",
  "subtime",
  "source_df"
)


# Reorder safely
existing_order <- desired_order[desired_order %in% names(df)]

df <- df %>%
  select(all_of(existing_order), everything())

write.csv(df, "data/arr/aoe/aoe_kobo_clean.csv", row.names = FALSE)

listApproved <- df %>%
  filter(validate == "approved")

evavailNA %<>%
  filter(pid %in% listApproved$pid)

evavailYESrepeat %<>%
  filter(pid %in% listApproved$pid)

