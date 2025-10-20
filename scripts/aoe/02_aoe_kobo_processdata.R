source("scripts/aoe/01_aoe_kobo_prepenv.R")

# Run for each df ---- 

specs <- list(
  df = list(
    keep_helpers = list(
      quo(ends_with("_label")),
      quo(contains("calc")),
      quo(-starts_with("os")),
      quo(-contains("des"))
    ),
    keep_names   = c("_validation_status", "_submission_time", "_index"),
    rename       = c(
      validate = "_validation_status",
      subtime  = "_submission_time",
      index    = "_index",
      method_prog = "method"
    ),
    prefix                 = NULL,
    suffix_replacements = list(
      list(suffix = "_evavail$", replacement = "_evavail24"),
      list(suffix = "_evnumb_calc$",    replacement = "_evnumb"),
      list(suffix = "_calc$",    replacement = "_evavail23"),        # generic _calc
      list(suffix = "_calc3$",    replacement = "_likert23"),
      list(suffix = "_likert$",    replacement = "_likert24"),
      list(suffix = "_likert2$",    replacement = "_likert24_alt")
    ),
    rename_patterns = list(
      list(pattern     = "^wowcoord_evavail_(\\d+)$",
           replacement = "wowcoord\\1_evavail"),
      list(pattern     = "^wowdata_evavail_(\\d+)$",
           replacement = "wowdata\\1_evavail"),
      list(pattern     = "^wowlocal_evavail_(\\d+)$",
           replacement = "wowlocal\\1_evavail"),
      list(pattern     = "^wowengage_evavail_(\\d+)$",
           replacement = "wowengage\\1_evavail")
    )
  ),
  
  group1 = list(
    prefix                  = c("eaenroll","eaattend", "rpcretent", "rpcprogress", "rpccomplete", "transit"),
    prefix_exclude_pattern  = NULL,
    keep_names   = c("text2", "matchlab", "repeatindex", "_parent_index", "base", "end", "basef", "basem", "endf", "endm"),
    keep_helpers            = list(
      quo(ends_with("_label")),
      quo(contains("calc")),
      quo(-contains("note")),
      quo(-matches("/\\d+$")),
      quo(-matches("\\d+[b-d]_label$"))
    ),
    exceptions = c("method8b_label"), 
    rename       = c(
      index    = "_parent_index",
      method1 = "method",
      georep = "method1a",
      method_sample = "method2a",
      method_collection = "method3a",
      method_source = "method4a",
      method_indicator = "method5a",
      method_other = "method6a",
      method_rating_gen = "method8a",
      method_rating_change = "method8b",
      method_measure = "method9a",
      method_measure_other = "method9ao",
      method_psychometric = "method9d"
    ),
    suffix_replacements = NULL,
    rename_patterns = NULL 
  ), 
  
  group2 = list(
    prefix                  = c("hloacd","hlosel", "hlogen"),
    prefix_exclude_pattern  = NULL,
    keep_names   = c("text2", "matchlab", "repeatindex", "_parent_index", "base", "end", "basef", "basem", "endf", "endm"),
    keep_helpers            = list(
      quo(ends_with("_label")),
      quo(contains("calc")),
      quo(-contains("note")),
      quo(-matches("/\\d+$")),
      quo(-matches("\\d+[b-d]_label$"))
    ),
    exceptions = c("method8b_label", "method9b_label"), 
    rename       = c(
      index    = "_parent_index",
      georep = "method1a",
      method_sample = "method2a",
      method_collection = "method3a",
      method_source = "method4a",
      method_indicator = "method5a",
      method_other = "method6a",
      method_rating_gen = "method8a",
      method_rating_change = "method8b",
      method_measure = "method9a",
      method_measure_other = "method9ao",
      method_psychometric = "method9d"
    ),
    suffix_replacements = NULL,
    rename_patterns = NULL 
  ), 
  
  group3 = list(
    prefix                  = c("wowcoord"),
    prefix_exclude_pattern  = NULL,
    keep_names   = c("repeatindex_01", "_parent_index", "text_01", "themeo_01", "outcomeo_01"),
    keep_helpers            = list(
      quo(-matches("/\\d+$")), 
      quo(ends_with("label"))
    ),
    exceptions = NULL, 
    rename       = c(
      index    = "_parent_index"
    ),
    suffix_replacements = list(
      list(suffix = "_01$", replacement = "")
    ), 
    rename_patterns = NULL 
  ), 
  
  group4 = list(
    prefix                  = c("wowcoord"),
    prefix_exclude_pattern  = NULL,
    keep_names   = c("repeatindex_02", "_parent_index", "text_02", "themeo_02"),
    keep_helpers            = list(
      quo(-matches("/\\d+$")),
      quo(ends_with("_label"))
    ),
    exceptions = NULL, 
    rename       = c(
      index    = "_parent_index"
    ),
    suffix_replacements    = list(
      list(suffix = "_01$", replacement = ""),
      list(suffix = "_02$", replacement = ""),
      list(suffix = "_03$", replacement = "")  # add any other expected suffixes
    ),
    rename_patterns = NULL 
  ),
  
  group5 = list(
    prefix                  = c("wowdata"),
    prefix_exclude_pattern  = NULL,
    keep_names   = c("repeatindex_01", "_parent_index", "text_01", "outcomeo_01"),
    keep_helpers            = list(
      quo(-matches("/\\d+$")),
      quo(ends_with("_label"))
    ),
    exceptions = NULL, 
    rename       = c(
      index    = "_parent_index"
    ),
    suffix_replacements    = list(
      list(suffix = "_01$", replacement = ""),
      list(suffix = "_02$", replacement = ""),
      list(suffix = "_03$", replacement = "")  # add any other expected suffixes
    ),
    rename_patterns = NULL 
  ),
  
  group6 = list(
    prefix                  = c("wowdata"),
    prefix_exclude_pattern  = NULL,
    keep_names   = c("repeatindex_02", "_parent_index", "text_02", "text2_02"),
    keep_helpers            = list(
      quo(-matches("/\\d+$")),
      quo(ends_with("_label"))
    ),
    exceptions = NULL, 
    rename       = c(
      index    = "_parent_index"
    ),
    suffix_replacements    = list(
      list(suffix = "_01$", replacement = ""),
      list(suffix = "_02$", replacement = ""),
      list(suffix = "_03$", replacement = "")  # add any other expected suffixes
    ),
    rename_patterns = NULL 
  ),
  
  group7 = list(
    prefix                  = c("wowdata"),
    prefix_exclude_pattern  = NULL,
    keep_names   = c("repeatindex_03", "_parent_index", "text_03", "themeo_03"),
    keep_helpers            = list(
      quo(-matches("/\\d+$")),
      quo(ends_with("_label"))
    ),
    exceptions = NULL, 
    rename       = c(
      index    = "_parent_index"
    ),
    suffix_replacements    = list(
      list(suffix = "_01$", replacement = ""),
      list(suffix = "_02$", replacement = ""),
      list(suffix = "_03$", replacement = "")  # add any other expected suffixes
    ),
    rename_patterns = NULL 
  ),
  
  group8 = list(
    prefix                  = c("wowlocal", "wowengage"),
    prefix_exclude_pattern  = NULL,
    keep_names   = c("repeatindex_01", "_parent_index", "text_01", "themeo_01"),
    keep_helpers            = list(
      quo(-matches("/\\d+$")),
      quo(ends_with("_label"))
    ),
    exceptions = NULL, 
    rename       = c(
      index    = "_parent_index"
    ),
    suffix_replacements    = list(
      list(suffix = "_01$", replacement = ""),
      list(suffix = "_02$", replacement = ""),
      list(suffix = "_03$", replacement = "")  # add any other expected suffixes
    ),
    rename_patterns = NULL 
  )
  
  
  
)

sheet_to_spec <- list(
  df       = "df",
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


# Assume specs and sheet_to_spec are defined as before

cleaned_list <- vector("list", length = length(sheet_to_spec))
names(cleaned_list) <- paste0(names(sheet_to_spec), "_clean")


# Iterate through each sheet name without overwriting originals
for (nm in names(sheet_to_spec)) {
  # 1. Fetch raw data
  raw_df <- get(nm, envir = .GlobalEnv)
  
  # 2. Apply labels
  lab_df <- apply_labels(raw_df, survey_map, choice_maps)
  
  # 3. Spec & prefix
  sp <- specs[[ sheet_to_spec[[nm]] ]]
  p  <- sp$prefix
  
  # 4. Core processing pipeline
  df_clean <- lab_df %>%
    { # A: Strip prefixes if any
      if (length(p %||% NULL) > 0) {
        prefix_regex <- paste0('^(', paste(p, collapse = '|'), ')_')
        rename_with(., .cols = matches(prefix_regex), .fn = ~ str_remove(.x, prefix_regex))
      } else {
        .
      }
    } %>%
    # B: Select required variables & helpers
    select(all_of(sp$keep_names), !!!sp$keep_helpers, any_of(sp$exceptions)) %>%
    # C: Drop '_label' suffix
    rename_with(.cols = ends_with('_label'), .fn = ~ str_remove(.x, '_label$'))
  
  # D: Suffix replacements
  if (!is.null(sp$suffix_replacements)) {
    for (rep in sp$suffix_replacements) {
      # find columns matching this suffix
      cols_to_fix <- grep(rep$suffix, names(df_clean), value = TRUE)
      if (length(cols_to_fix) > 0) {
        df_clean <- df_clean %>%
          rename_with(
            .cols = all_of(cols_to_fix),
            .fn   = ~ str_replace(.x, rep$suffix, rep$replacement)
          )
      }
    }
  }
  
  # E: Pattern-based renames
  if (!is.null(sp$rename_patterns) && length(sp$rename_patterns) > 0) {
    for (rp in sp$rename_patterns) {
      df_clean <- df_clean %>%
        rename_with(.cols = matches(rp$pattern), .fn = ~ str_replace(.x, rp$pattern, rp$replacement))
    }
  }
  
  # F: Manual renames — SAFE VERSION
  existing_rename <- sp$rename[sp$rename %in% names(df_clean)]
  df_clean <- df_clean %>% rename(!!!existing_rename)
  # OPTIONAL: Reorder columns so index comes first (if present)
  desired_order <- c("index", "repeatindex")
  existing_order <- desired_order[desired_order %in% names(df_clean)]
  
  df_clean <- df_clean %>%
    select(all_of(existing_order), everything())
  
  # G: Final meta fixes
  if ('validate' %in% names(df_clean)) {
    df_clean <- df_clean %>% mutate(validate = str_remove(validate, '^validation_status_'))
  }
  if ('subtime' %in% names(df_clean)) {
    df_clean <- df_clean %>%
      mutate(subtime = str_replace(subtime, 'T', ' ') %>% ymd_hms() %>% as_date())
  }
  # Optional: change value labels of evchange
  if ('evchange' %in% names(df_clean)) {
    df_clean <- df_clean %>%
      mutate(evchange = ifelse(evchange == "No (e.g., baseline)", "No", evchange))
    }
  
  # Optional: change value labels of evchange
  if ('method_prog' %in% names(df_clean)) {
    df_clean <- df_clean %>%
      mutate(method_prog = case_when(
        method_prog == "No or very limited methodological detail provided" ~ "No",
        method_prog == "General methodological detail provided - not about specific outcomes or evidence" ~ "Some",
        method_prog == "Methodological detail provided about at least one specific outcome"~ "Yes",
        TRUE ~ NA,
      ))
  }
  # 5. Store in list (safe, no overwriting)
  cleaned_list[[ paste0(nm, '_clean') ]] <- df_clean
}

# 6. Load all cleaned dataframes into global environment
list2env(cleaned_list, .GlobalEnv)

## reorder ----

# 1) Define which cleaned objects belong to group1
group_sheets <- list(
  group1 = c("eaenroll_clean", "eaattend_clean", "rpcretent_clean", "rpcprogress_clean", "rpccomplete_clean", "transit_clean", "hloacd_clean", "hlosel_clean", "hlogen_clean"),
  group2 = c("wowcoord2_clean", "wowdata3_clean", "wowlocal_clean", "wowengage_clean"), 
  group3 = c("wowcoord1_clean"), 
  group4 = c("wowdata1_clean"),
  group5 = c("wowdata2_clean")
)


# Define desired column order per group
reorder_columns <- list(
  group1 = c("index", "repeatindex", "evcalc", "text2", "base", "end", "basef", "basem", "endf", "endm", "method8a", "method8b", "match1", "matchlab"),
  group2 = c("index", "repeatindex", "text", "theme", "themeo"),
  group3 = c("index", "repeatindex", "text", "theme", "themeo", "outcome", "outcomeo"),
  group4 = c("index", "repeatindex", "text", "outcome", "outcomeo"),
  group5 = c("index", "repeatindex", "text", "pop", "cwd", "text2")
)

# Smart reorder loop — works for all groups!
for (group in names(group_sheets)) {
  for (df_name in group_sheets[[group]]) {
    if (exists(df_name)) {
      df <- get(df_name)
      
      # Get desired order for this group
      desired_order <- reorder_columns[[group]]
      
      # Safe filtering: only columns that exist in this df
      existing_order <- desired_order[desired_order %in% names(df)]
      
      # Safe select
      df_reordered <- df %>%
        select(all_of(existing_order), everything())
      
      # Save back to global env
      assign(df_name, df_reordered, envir = .GlobalEnv)
      
      message("Reordered ", df_name)
    }
  }
}

# # 2) Loop through them
# for (df_name in group_sheets$group1) {
#   if (exists(df_name)) {
#     df <- get(df_name)
#     df_reordered <- df %>%
#       select(
#         index,
#         repeatindex, 
#         evcalc,
#         text2,
#         base,
#         end,
#         basef,
#         basem,
#         endf,
#         endm,
#         method8a,
#         method8b,
#         match1,
#         matchlab,
#         everything()
#       )
#     assign(df_name, df_reordered, envir = .GlobalEnv)
#   }
# }
# 
# for (df_name in group_sheets$group2) {
#   if (exists(df_name)) {
#     df <- get(df_name)
#     df_reordered <- df %>%
#       select(
#         index,
#         repeatindex, 
#         text,
#         theme,
#         themeo
#       )
#     assign(df_name, df_reordered, envir = .GlobalEnv)
#   }
# }
# 
# for (df_name in group_sheets$group3) {
#   if (exists(df_name)) {
#     df <- get(df_name)
#     df_reordered <- df %>%
#       select(
#         index,
#         repeatindex, 
#         text,
#         theme,
#         themeo,
#         outcome,
#         outcomeo
#       )
#     assign(df_name, df_reordered, envir = .GlobalEnv)
#   }
# }
# 
# for (df_name in group_sheets$group4) {
#   if (exists(df_name)) {
#     df <- get(df_name)
#     df_reordered <- df %>%
#       select(
#         index,
#         repeatindex, 
#         text,
#         outcome,
#         outcomeo
#       )
#     assign(df_name, df_reordered, envir = .GlobalEnv)
#   }
# }
# 
# for (df_name in group_sheets$group5) {
#   if (exists(df_name)) {
#     df <- get(df_name)
#     df_reordered <- df %>%
#       select(
#         index,
#         repeatindex, 
#         text,
#         pop,
#        cwd,
#        text2
#       )
#     assign(df_name, df_reordered, envir = .GlobalEnv)
#   }
# }


# 1) Define which dataframes to keep
dfs_to_keep <- c("df_clean", "eaenroll_clean", "eaattend_clean", "rpcretent_clean", "rpcprogress_clean", "rpccomplete_clean", "transit_clean", "hloacd_clean", "hlosel_clean", "hlogen_clean", "wowcoord1_clean", "wowcoord2_clean", "wowdata1_clean", "wowdata2_clean", "wowdata3_clean", "wowlocal_clean", "wowengage_clean")

# 1) List all objects in the global environment
all_objs <- ls(envir = .GlobalEnv)

# 2) Identify objects that are data.frames or lists
objs_to_consider <- Filter(
  function(x) {
    obj <- get(x, envir = .GlobalEnv)
    is.data.frame(obj) || is.list(obj)
  },
  all_objs
)

# 3) Define which to keep (must be character vector of object names)
# Example:
# dfs_to_keep <- c("important_df", "keep_this_list")

# 4) Determine which objects to remove (not in the keep list)
objs_to_remove <- setdiff(objs_to_consider, dfs_to_keep)

# 5) Remove them
if (length(objs_to_remove) > 0) {
  rm(list = objs_to_remove, envir = .GlobalEnv)
  message("Removed objects: ", paste(objs_to_remove, collapse = ", "))
} else {
  message("No objects found to remove.")
}
