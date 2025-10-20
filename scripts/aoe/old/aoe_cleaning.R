rm(list = ls())
library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(writexl)
library(lubridate)

# Load 'choices' and 'survey' from your XLSForm
choices <- read_excel("data/utilities/aoe_kobo_codes.xlsx", sheet = "choices") %>%
  filter(!is.na(list_name), !is.na(name), !is.na(label)) %>%
  mutate(name = as.character(name),
         label = as.character(label))

survey <- read_excel("data/utilities/aoe_kobo_codes.xlsx", sheet = "survey") %>%
  filter(str_detect(type, "select_(one|multiple)\\s"))

# Extract question name and associated list_name
survey_map <- survey %>%
  mutate(
    list_name = str_extract(type, "(?<=select_(one|multiple)\\s).*"),
    question_type = case_when(
      str_detect(type, "select_one") ~ "select_one",
      str_detect(type, "select_multiple") ~ "select_multiple"
    )
  ) %>%
  select(name, list_name, question_type)

# Create a lookup list: list_name → code → label
lookup_list <- split(choices, choices$list_name) %>%
  map(~ setNames(.x$label, .x$name))

# Load all sheets from Kobo export
export_path <- "data/aoe_kobo_export.xlsx"
sheet_names <- excel_sheets(export_path)
export_data <- set_names(sheet_names, sheet_names) %>%
  map(~ read_excel(export_path, sheet = .x))

# Function to recode select_multiple: space-separated → label1|label2
recode_multi_column <- function(column, code_to_label) {
  sapply(column, function(x) {
    if (is.na(x)) return(NA_character_)
    parts <- str_split(as.character(x), "\\s+")[[1]]
    labels <- code_to_label[parts]
    labels[is.na(labels)] <- parts[is.na(labels)]
    paste(labels, collapse = "|")
  })
}

# Function to recode select_one: single value → label
recode_single_column <- function(column, code_to_label) {
  sapply(column, function(x) {
    if (is.na(x)) return(NA_character_)
    val <- as.character(x)
    code_to_label[val] %||% val
  })
}

export_cleaned <- map(export_data, function(df) {
  for (q in survey_map$name) {
    if (q %in% names(df)) {
      row_index <- match(q, survey_map$name)
      list_name <- survey_map$list_name[[row_index]]
      q_type <- survey_map$question_type[[row_index]]
      if (!is.na(list_name) && list_name %in% names(lookup_list)) {
        df[[q]] <- if (q_type == "select_multiple") {
          recode_multi_column(df[[q]], lookup_list[[list_name]])
        } else {
          recode_single_column(df[[q]], lookup_list[[list_name]])
        }
      }
    }
  }
  df
})


# Remove dummy / columns (they come from select_multiple checkbox expansions)
remove_dummy_columns <- function(df) {
  df[, !grepl("/", names(df))]
}

# Apply dummy removal
export_data_cleaned <- lapply(export_cleaned, remove_dummy_columns)


dfs <- export_data_cleaned

rm(export_cleaned, export_data, export_data_cleaned, lookup_list, survey, survey_map, choices)

# remove dummies
dfs <- lapply(dfs, function(df) {
  df[, !grepl("/", names(df))]
})

# Previx sub_objective variable
# dfs <- lapply(dfs, function(df) {
#   # Find columns with an underscore
#   underscore_cols <- names(df)[grepl("^[^_]+_", names(df))]
#   
#   if (length(underscore_cols) == 0) return(df)  # Skip if no matches
#   
#   # Extract prefix (before first underscore)
#   first_col <- underscore_cols[1]
#   prefix <- sub("_.*$", "", first_col)  # take the part before the first "_"
#   
#   # Rename columns: remove prefix and underscore
#   names(df) <- gsub(paste0("^", prefix, "_"), "", names(df))
#   
#   # Add sub_objective column
#   df$sub_objective <- prefix
#   
#   return(df)
# })

# drop unnecessary columns
dfs <- lapply(dfs, function(df) {
  df[, !grepl("note|display|repeat", names(df), ignore.case = TRUE)]
})

# drop "_repeat" from the names in the list
names(dfs) <- gsub("_repeat", "", names(dfs))

# Group definitions — exact mapping of prefixes to each group
group_map <- list(
  "1"   = c("eaenroll", "eaattend", "rpcretent", "rpcprogress", "rpccomplete", "transit"),
  "2"   = c("hloacd", "hlosel", "hlogen", "hlooth"),
  "3.1" = c("osteach", "osschool"),
  "3.2" = c("ospolicy", "osother", "osschool"),
  "4.1" = c("wowcoord_01", "wowdata_01"),
  "4.2" = c("wowcoord_02", "wowdata_03", "wowlocal_01", "wowengage_01"),
  "4.3" = c("wowdata_02")
)

# Function to get and bind matched data frames, with "prefix" column
get_group_members <- function(group_prefixes, dfs) {
  matched_names <- names(dfs)[
    sapply(names(dfs), function(nm) {
      any(startsWith(nm, group_prefixes))
    })
  ]
  
  # Bind rows and add "prefix" column
  bind_rows(lapply(matched_names, function(nm) {
    df <- dfs[[nm]]
    df$prefix <- nm
    df
  }))
}

# Apply to each group
grouped_outputs <- map(group_map, ~ get_group_members(.x, dfs))

root <- dfs[[1]]

dfs <- grouped_outputs

# Columns to join from df
join_cols <- c("_index", "_parent_index", "pid", "country", "intestment_type", "active23_calc", "active23_man", "name_selected", "evavail")

# Filter df to only those columns (safe in case some are missing)
df_join_data <- root %>%
  select(any_of(join_cols))

# Join into each data frame in dfs
dfs <- lapply(dfs, function(sub_df) {
  if ("_parent_index" %in% names(sub_df)) {
    left_join(sub_df, df_join_data, by = ("_parent_index" = "_index"))
  } else {
    sub_df  # leave unchanged if no _parent_index
  }
})



# # output dfs
# # Start counter
# i <- 1
# 
# # Loop through grouped_outputs and create df1, df2, ..., adding the group column
# for (group_name in names(dfs)) {
#   df <- dfs[[group_name]]
#   df$group <- group_name  # Add group name as column
#   assign(paste0("df", i), df, envir = .GlobalEnv)  # Save as df1, df2, ...
#   i <- i + 1
# }



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
  "hlooth",        "2.5 | Other children’s outcomes",                 NA,
  "osteach",       "3.1 | Teacher outcomes",                          NA,
  "osschool",      "3.2 | School and classroom outcomes",             NA,
  "ospolicy",      "3.3 | National policy and systems",               NA,
  "osother",       "3.4 | Other opportunities and services",          NA,
  "wowcoord_01",   "4.2 | Coordination - Programmatic",               "11",
  "wowcoord_02",   "4.2 | Coordination - Sector",                     "11",
  "wowdata_01",    "4.3 | Data - Programmatic - outcomes",            "17",
  "wowdata_02",    "4.3 | Data - Programmatic – marginalized groups", "17",
  "wowdata_03",    "4.3 | Data - Sector",                             "17",
  "wowlocal_01",   "4.1 | Localization",                              "13",
  "wowengage_01",  "4.4 | Accountability",                            "14"
)

dfs <- lapply(dfs, function(df) {
  if (!"prefix" %in% names(df)) return(df)  # skip if column missing
  
  # For each row, match sub_objective to the prefix column in the lookup
  df$sub_objective <- sapply(df$prefix, function(obj) {
    matched <- subtext_lookup %>%
      filter(prefix == obj) %>%
      slice(1)
    matched$sub_text %||% NA_character_
  })
  
  df
})


dfs <- lapply(dfs, function(df) {
  if (!"prefix" %in% names(df)) return(df)  # skip if column missing
  
  # For each row, match indicator to the prefix column in the lookup
  df$indicator_code <- sapply(df$prefix, function(obj) {
    matched <- subtext_lookup %>%
      filter(prefix == obj) %>%
      slice(1)
    if (nrow(matched) == 0) NA_character_ else matched$indicator_code
  })
  
  df
})


# Step 2: Assign group = df name
dfs <- imap(dfs, function(df, df_name) {
  df$group <- df_name
  df
})

# dfs <- lapply(dfs, function(df) {
#   if ("sub_objective" %in% names(df)) {
#     df <- df %>% rename(prefix = sub_objective)
#   }
#   df
# })

# # concat
# dfs <- lapply(dfs, function(df) {
#   if (all(c("group", "sub_text") %in% names(df))) {
#     df$sub_objective <- paste(df$group, df$sub_text, sep = " | ")
#   }
#   df
# })

# Reorder columns in each df
order_cols <- c("_index", "_parent_index", "pid", "country", "intestment_type", "active23_calc", "active23_man", "name_selected",
                "prefix", "group", "sub_objective")
dfs <- lapply(dfs, function(df) {
  cols_present <- intersect(order_cols, names(df))  # only keep those actually present
  other_cols <- setdiff(names(df), cols_present)
  df[, c(cols_present, other_cols)]
})

df <- bind_rows(dfs)

df %<>%
  rename(validate = `_submission__validation_status`) %>%  
  mutate(validate = str_remove(validate, "^validation_status_")) 

df %<>%
  rename(subtime = `_submission__submission_time`) %>%  
  mutate(subtime = ymd_hms(gsub("T", " ", subtime)) %>%  
  as_date()) 

# write.csv(df, "for_Carly_to_check.csv", row.names = FALSE)
