# LOAD DCM template ----

## Prep env ----
source("config.yml")
source("requirements/libraries.R")


## Import ----

# Step 1: Define your folder path
main_folder <- ecw_db

# Step 2: Find all Excel files recursively that contain 
all_files <- list.files(
  main_folder,
  pattern = "(ECW (Finance|Programme) template|Programme template).*\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

# Step 3: Define function to load the target table only
load_ecw_results_table <- function(file_path) {
  # Load workbook
  wb <- wb_load(file_path)
  
  # Check for the target table
  target_tables <- wb$tables %>%
    dplyr::filter(tab_name == "ecw_finance_dcm")
  
  if (nrow(target_tables) == 0) {
    message("⚠️ No target table in: ", basename(file_path))
    return(NULL)
  }
  
  # Extract tables (usually 1, but safe for multiple)
  purrr::map(seq_len(nrow(target_tables)), function(i) {
    tbl_info <- target_tables[i, ]
    sheet_name <- wb$sheet_names[tbl_info$tab_sheet]
    range <- tbl_info$tab_ref
    
    message("📥 Reading: ", basename(file_path), " → ", sheet_name, " @ ", range)
    
    df <- tryCatch({
      df_raw <- wb_to_df(wb, sheet = sheet_name, range = range)
      
      # # 🧩 Fix column names if NA or blank
      # colnames(df_raw) <- ifelse(
      #   is.na(colnames(df_raw)) | trimws(colnames(df_raw)) == "",
      #   paste0("col_", seq_along(colnames(df_raw))),
      #   colnames(df_raw)
      # )
      
      # 🧩 Fix and deduplicate column names
      colnames(df_raw) <- make.unique(
        ifelse(
          is.na(colnames(df_raw)) | trimws(colnames(df_raw)) == "",
          paste0("col_", seq_along(colnames(df_raw))),
          trimws(colnames(df_raw))
        )
      )
      
      df_raw %>%
        dplyr::mutate(
          file_name = basename(file_path),
          table_index = i
        )
      
    }, error = function(e) {
      message("⛔ Error in: ", basename(file_path), " — ", e$message)
      return(NULL)
    })
    
  }) %>% purrr::compact()
}

# Step 4: Process all files with progress bar
library(progressr)
handlers(global = TRUE)
handlers("txtprogressbar")

all_results_list <- with_progress({
  p <- progressor(along = all_files)
  result <- purrr::map(all_files, function(file) {
    p(message = basename(file))
    load_ecw_results_table(file)
  }) %>% purrr::compact() %>% purrr::flatten()
  result
})


# Step 5: Check results
length(all_results_list)  # ✅ You now have a list of data frames!

# Cleaning of tables with different headers ----
## First TEMPLATE cleaning ----
### Header col1 ----
clean_dcm_table1 <- function(df) {
  # Step 1: Prepare column 1 for scanning (clean and lower case)
  col1_clean <- df[[1]] %>%
    as.character() %>%
    stringr::str_replace_all("[\r\n\t]", " ") %>%    # remove \n
    stringr::str_replace_all("\\s+", " ") %>%        # collapse spaces
    trimws() %>%
    tolower()
  
  # Step 2: Find the first row number that contains the header — use regex!
  header_row_num <- which(str_detect(col1_clean, regex("sub-grantee/ip name", ignore_case = TRUE)))[1]
  
  # Step 3: If not found, warn and return original
  if (is.na(header_row_num)) {
    warning("No header row found with 'sub-grantee/ip name' in column 1. Returning original dataframe.")
    return(df)
  }
  
  # Step 4: Extract header row as character vector
  header_row <- as.character(df[header_row_num, ])
  
  # Step 5: Slice data — remove rows up to and including header row, drop first column
  df_clean <- df[-(1:header_row_num), , drop = FALSE]
  
  # Step 6: Apply clean column names
  colnames(df_clean) <- header_row %>%
    trimws() %>%
    janitor::make_clean_names()
  
  # Step 7: Clean up data — trim whitespace, remove empty rows
  df_clean <- df_clean %>%
    dplyr::mutate(across(everything(), ~ trimws(as.character(.)))) %>%
    dplyr::filter(!if_all(everything(), ~ .x == "" | is.na(.x)))
  
  return(df_clean)
}


cleaned_tables <- lapply(all_results_list, clean_dcm_table1)


### Header col2 ----
clean_dcm_table2 <- function(df) {
  # Step 1: Prepare column 2 for scanning (clean and lower case)
  col2_clean <- df[[2]] %>%
    as.character() %>%
    stringr::str_replace_all("[\r\n\t]", " ") %>%    # remove \n
    stringr::str_replace_all("\\s+", " ") %>%        # collapse spaces
    trimws() %>%
    tolower()
  
  # Step 2: Find the first row number that contains the header — use regex!
  header_row_num <- which(str_detect(col2_clean, regex("sub-grantee/ip.*name", ignore_case = TRUE)))[1]
  
  # Step 3: If not found, warn and return original
  if (is.na(header_row_num)) {
    warning("No header row found with 'sub-grantee/ip name' in column 1. Returning original dataframe.")
    return(df)
  }
  
  # Step 4: Extract header row as character vector
  header_row <- as.character(df[header_row_num, ])
  
  # Step 5: Slice data — remove rows up to and including header row, drop first column
  df_clean <- df[-(1:header_row_num), -1, drop = FALSE]
  
  # Step 6: Apply clean column names
  colnames(df_clean) <- header_row[-1] %>%
    trimws() %>%
    janitor::make_clean_names()
  
  # Step 7: Clean up data — trim whitespace, remove empty rows
  df_clean <- df_clean %>%
    dplyr::mutate(across(everything(), ~ trimws(as.character(.)))) %>%
    dplyr::filter(!if_all(everything(), ~ .x == "" | is.na(.x)))
  
  return(df_clean)
}

cleaned_tables <- lapply(cleaned_tables, clean_dcm_table2)

# Check headers ----
# Check if first column is coherent
column_check <- map_lgl(cleaned_tables, ~ names(.x)[1] == "sub_grantee_ip_name_please_list_only_direct_sub_grantees")

# View results
print(column_check)

# if TRUE all the same first header
all(column_check)
problematic <- map_chr(cleaned_tables, ~ names(.x)[1])
problematic[which(problematic != "sub_grantee_ip_name_please_list_only_direct_sub_grantees")]

## SECOND TEMPLATE cleaning ----
### header col1 ----
clean_dcm_table1_second_template <- function(df) {
  # Step 1: Prepare column 1 for scanning (clean and lower case)
  col1_clean <- df[[1]] %>%
    as.character() %>%
    stringr::str_replace_all("[\r\n\t]", " ") %>%    # remove \n
    stringr::str_replace_all("\\s+", " ") %>%        # collapse spaces
    trimws() %>%
    tolower()
  
  # Step 2: Find the first row number that contains the header — use regex!
  header_row_num <-which(col1_clean == "consortium partner" | col1_clean == "consortium partners")[1]
  
  # Step 3: If not found, warn and return original
  if (is.na(header_row_num)) {
    warning("No header row found with 'consortium' in column 1. Returning original dataframe.")
    return(df)
  }
  
  # Step 4: Extract header row as character vector
  header_row <- as.character(df[header_row_num, ])
  
  # Step 5: Slice data — remove rows up to and including header row, drop first column
  df_clean <- df[-(1:header_row_num), , drop = FALSE]
  
  # Step 6: Apply clean column names
  colnames(df_clean) <- header_row %>%
    trimws() %>%
    janitor::make_clean_names()
  
  # Step 7: Clean up data — trim whitespace, remove empty rows
  df_clean <- df_clean %>%
    dplyr::mutate(across(everything(), ~ trimws(as.character(.)))) %>%
    dplyr::filter(!if_all(everything(), ~ .x == "" | is.na(.x)))
  
  return(df_clean)
}


cleaned_tables <- lapply(cleaned_tables, clean_dcm_table1_second_template)


## THIRD TEMPLATE cleaning ----
## header col1 ----
clean_dcm_table1_third_template <- function(df) {
  # Step 1: Prepare column 1 for scanning (clean and lower case)
  col1_clean <- df[[1]] %>%
    as.character() %>%
    stringr::str_replace_all("[\r\n\t]", " ") %>%    # remove \n
    stringr::str_replace_all("\\s+", " ") %>%        # collapse spaces
    trimws() %>%
    tolower()
  
  # Step 2: Find the first row number that contains the header — use regex!
  header_row_num <-which(col1_clean == "grantee")[1]
  
  # Step 3: If not found, warn and return original
  if (is.na(header_row_num)) {
    warning("No header row found with 'grantee' in column 1. Returning original dataframe.")
    return(df)
  }
  
  # Step 4: Extract header row as character vector
  header_row <- as.character(df[header_row_num, ])
  
  # Step 5: Slice data — remove rows up to and including header row, drop first column
  df_clean <- df[-(1:header_row_num), , drop = FALSE]
  
  # drop first row
  df_clean <- df[-1,]
  
  # Step 6: Apply clean column names
  colnames(df_clean) <- header_row %>%
    trimws() %>%
    janitor::make_clean_names()
  
  # Step 7: Clean up data — trim whitespace, remove empty rows
  df_clean <- df_clean %>%
    dplyr::mutate(across(everything(), ~ trimws(as.character(.)))) %>%
    dplyr::filter(!if_all(everything(), ~ .x == "" | is.na(.x)))
  
  return(df_clean)
}


cleaned_tables <- lapply(cleaned_tables, clean_dcm_table1_third_template)

# Other cleaning ----
clean_after_grantee <- function(df) {
  
  # Step 1: Clean column 1 (remove \n, lower case, trim)
  col1_clean <- df[[1]] %>%
    as.character() %>%
    stringr::str_replace_all("[\r\n\t]", " ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    trimws()
  
  # Step 2: Find all row numbers where value == "Grantee" (case-insensitive)
  grantee_rows <- which(stringr::str_detect(col1_clean, regex("^grantee$", ignore_case = TRUE)))
  
  # Step 3: Check if we found at least 2 "Grantee" rows
  if (length(grantee_rows) < 2) {
    warning("⚠️ Less than 2 'Grantee' rows found — returning original dataframe.")
    return(df)
  }
  
  # Step 4: Get index of second "Grantee"
  cut_row <- grantee_rows[2]
  
  # Step 5: Slice dataframe — remove rows up to and including the second Grantee row
  df_clean <- df[-(1:cut_row), , drop = FALSE]
  
  message(glue::glue("✅ Cleaned — removed first {cut_row} rows (up to 2nd 'Grantee')"))
  
  return(df_clean)
}

cleaned_tables <- lapply(cleaned_tables, clean_after_grantee)

# check headers ---
# Check if first column is coherent
column_check <- map_lgl(cleaned_tables, ~ names(.x)[1] == "grantee" |
                          names(.x)[1] == "consortium_partner" |
                          names(.x)[1] == "consortium_partners" |
                          names(.x)[1] == "sub_grantee_ip_name_please_list_only_direct_sub_grantees")

# View results
print(column_check)

# if TRUE all the same first header
all(column_check)
problematic <- map_chr(cleaned_tables, ~ names(.x)[1])
problematic[which(problematic != "sub_grantee_ip_name_please_list_only_direct_sub_grantees" &
                    problematic != "grantee" &
                    problematic != "consortium_partner" &
                    problematic != "consortium_partners")]


# SPLIT IN LISTS to check header consistency by group ----
# Group 1: grantee
grantee_tables <- cleaned_tables %>%
  purrr::keep(~ names(.x)[1] == "grantee")

# Group 2: consortium_partner or consortium_partners
consortium_tables <- cleaned_tables %>%
  purrr::keep(~ names(.x)[1] %in% c("consortium_partner", "consortium_partners"))

# Group 3: sub_grantee_ip_name_please_list_only_direct_sub_grantees
subgrantee_tables <- cleaned_tables %>%
  purrr::keep(~ names(.x)[1] == "sub_grantee_ip_name_please_list_only_direct_sub_grantees")


# Header consistency by group ----
check_headers_consistency <- function(df_list, group_name = "Group") {
  # Step 1: Get sorted column names for each data frame
  colnames_list <- purrr::map(df_list, ~ sort(names(.x)))
  
  # Step 2: Detect unique sets of column names
  unique_colnames <- unique(colnames_list)
  
  # Step 3: Reporting
  if (length(unique_colnames) == 1) {
    message("✅ ", group_name, ": All tables have consistent headers.")
  } else {
    message("⚠️ ", group_name, ": Inconsistent headers found. Number of unique sets: ", length(unique_colnames))
    
    # Pick the first as reference
    ref <- unique_colnames[[1]]
    
    # Step 4: Compare each to reference
    purrr::iwalk(colnames_list, function(cols, idx) {
      if (!identical(cols, ref)) {
        message("🔍 Table index ", idx, " differs:")
        
        missing <- setdiff(ref, cols)
        extra <- setdiff(cols, ref)
        
        if (length(missing) > 0) message("   ❌ Missing columns: ", paste(missing, collapse = ", "))
        if (length(extra) > 0) message("   ➕ Extra columns: ", paste(extra, collapse = ", "))
      }
    })
  }
}


# Clean each df in consortium_tables
consortium_tables <- map(consortium_tables, function(df) {
  
  # Step 1: Rename columns
  df <- df %>%
    rename_with(~ str_replace_all(., "woman_organization", "women_led_organization")) %>%
    rename_with(~ str_replace_all(., "consortium_parnter_type", "consortium_partner_type")) %>%
    rename_with(~ str_replace_all(., "consortium_partners", "consortium_partner"))
  
  # Step 2: Remove columns starting with "na"
  df <- df %>%
    select(-matches("^na"))
  
  # Step 3: Rename column starting with "ecw_programme" to "file_name"
  prog_col <- names(df)[str_detect(names(df), "^ecw_programme")]
  
  if (length(prog_col) == 1) {
    df <- df %>%
      rename(file_name = all_of(prog_col))
  } else if (length(prog_col) > 1) {
    warning("⚠️ More than one 'ecw_programme' column found — not renaming.")
  } else {
    warning("⚠️ No 'ecw_programme' column found — skipping rename.")
  }
  
  # Done
  return(df)
})


check_headers_consistency(consortium_tables, "consortium_tables")


# Clean each df in grantee
grantee_tables <- map(grantee_tables, function(df) {
  # Step 1: Rename columns
  df <- df %>%
    rename_with(~ str_replace_all(., "woman_organization", "women_led_organization")) %>%
    rename_with(~ str_replace_all(., "women_organization", "women_led_organization"))
  
  # Step 2: Remove columns starting with "na"
  df <- df %>%
    select(-matches("^na"))
  
  # Step 3: Rename column starting with "ecw_programme" to "file_name"
  prog_col <- names(df)[str_detect(names(df), "^ecw_programme")]
  
  if (length(prog_col) == 1) {
    df <- df %>%
      rename(file_name = all_of(prog_col))
  } else if (length(prog_col) > 1) {
    warning("⚠️ More than one 'ecw_programme' column found — not renaming.")
  } else {
    warning("⚠️ No 'ecw_programme' column found — skipping rename.")
  }
  
  # Done
  return(df)
})

check_headers_consistency(grantee_tables, "grantee_tables")


# Clean each df in grantee
subgrantee_tables <- map(subgrantee_tables, function(df) {
  # Step 1: Rename columns
  df <- df %>%
    rename_with(~ str_replace_all(., "woman_organization", "women_led_organization")) %>%
    rename_with(~ str_replace_all(., "women_organization", "women_led_organization")) %>%
    rename(grid = matches("grid"))
  
  # Step 2: Ensure required columns exist
  check <- c("organization_of_persons_with_disabilities", 
             "refugee_led_organization",
             "women_led_organization")
  
  for (i in check) {
    if (!(i %in% names(df))) {
      df <- df %>%
        mutate(!!sym(i) := NA)
    }
  }
  
  # Step 3: Remove columns starting with "na"
  df <- df %>%
    select(-matches("^na"))
  
  # Step 4: Rename column starting with "ecw_programme" to "file_name"
  prog_col <- names(df)[grepl("ecw_", names(df)) & grepl("_template_", names(df))]
  
  if (length(prog_col) == 1) {
    df <- df %>%
      rename(file_name = all_of(prog_col))
  } else if (length(prog_col) > 1) {
    warning("⚠️ More than one 'ecw_programme' column found — not renaming.")
  } else {
    warning("⚠️ No 'ecw_programme' column found — skipping rename.")
  }
  
  # Done
  return(df)
})



check_headers_consistency(subgrantee_tables, "subgrantee_tables")


# Bind into one big dataframe ----
sub_grantees <- dplyr::bind_rows(subgrantee_tables)
consortium <- dplyr::bind_rows(consortium_tables)
grantees <- dplyr::bind_rows(grantee_tables)

dfs <- c("sub_grantees", "consortium", "grantees")

for (df_name in dfs) {
  df <- get(df_name) %>%
    filter(!is.na(grid))
  
  assign(df_name, df)
}

## Assign source of data ----
sub_grantees %<>%
  mutate(source_of_data = "Finance | DCM | subgrantees" )
grantees %<>%
  mutate(source_of_data = "Finance | DCM | grantees" )
consortium %<>%
  mutate(source_of_data = "Finance | DCM | consortium partners" )

df <- bind_rows(sub_grantees, grantees, consortium)

# Save output ----
dir.create("data/finance combiner/dcm", recursive = TRUE, showWarnings = FALSE)
write.csv(df, "data/finance combiner/dcm/dcm source data.csv", row.names = FALSE)

rm(list = ls())
