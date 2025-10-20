# LOAD CR Template ----


## Prep env ----
source("config.yml")
source("requirements/libraries.R")

## Import ----

# Step 1: Define your folder path form config file
main_folder <- ecw_db

# Step 2: Find all Excel files recursively that contain "ECW (Results) template" in GrantsDB
all_files <- list.files(main_folder, pattern = "ECW (Results|Programme) template.*\\.xlsx$", 
                        recursive = TRUE, 
                        full.names = TRUE,
                        ignore.case = TRUE)

# Step 3: Define function to load the "ecw_ar_children" table only
load_ecw_cr_table <- function(file_path) {
  # Load workbook
  wb <- wb_load(file_path)
  
  # Check for the target table
  target_tables <- wb$tables %>%
    dplyr::filter(tab_name == "ecw_ar_children")
  
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
      
      # 🧩 Fix column names if NA or blank
      colnames(df_raw) <- ifelse(
        is.na(colnames(df_raw)) | trimws(colnames(df_raw)) == "",
        paste0("col_", seq_along(colnames(df_raw))),
        colnames(df_raw)
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

# Step 5: use the function to extract cr template
all_results_list <- with_progress({
  p <- progressor(along = all_files)
  result <- purrr::map(all_files, function(file) {
    p(message = basename(file))
    load_ecw_cr_table(file)
  }) %>% purrr::compact() %>% purrr::flatten()
  result
})


# Step 5: Check results
length(all_results_list)

## Processing ----
# Define function to detect first column and row based on name
clean_cr_table <- function(df) {
  # Step 1: Prepare column 2 for scanning (clean and lower case)
  col2_clean <- df[[2]] %>%
    as.character() %>%
    trimws() %>%
    tolower()
  
  # Step 2: Find the first row number that contains "type of education"
  header_row_num <- which(col2_clean == "type of education")[1]
  
  # Step 3: If not found, warn and return original
  if (is.na(header_row_num)) {
    warning("No header row found with 'Type of education' in column 2. Returning original dataframe.")
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

# Use function on the list
cleaned_tables <- lapply(all_results_list, clean_cr_table)

## Check first column header ----
# Check if first column is "outcome_output_numbering" in all data frames
column_check <- map_lgl(cleaned_tables, ~ names(.x)[1] == "type_of_education")

# View results
print(column_check)

# if TRUE all the same first header
all(column_check)

# Spot the inconsistencies (if 0, means all good)
problematic <- map_chr(cleaned_tables, ~ names(.x)[1])
problematic[which(problematic != "type_of_education")]


## Clean headers ----
# Function to clean headers 
cleaning_names <- function(df) {
  # Find ecw_results_template_ columns
  ecw_cols <- grep("^ecw_programme_template_|ecw_results_", names(df), value = TRUE)
  
  # Find mistyped analysis_code column
  analysis_code_cols <- names(df)[names(df) %in% c("analysis_code", "aysis_code", "ecw_anays_code", "anaysis_code",
                                                   "anays_code")]
  
  df_clean <- df %>%
    dplyr::select(-dplyr::starts_with("na_")) %>%
    # Conditionally rename ecw_results_template_* to file_name
    { if (length(ecw_cols) == 1) dplyr::rename(., file_name = !!ecw_cols) else . } %>%
    # Conditionally rename ar_grid_2021 to GRID
    { if ("ar_grid_2021" %in% names(.)) dplyr::rename(., GRID = ar_grid_2021) else . } %>%
    { if ("grid" %in% names(.)) dplyr::rename(., GRID = grid) else . } %>%
    # Conditionally rename analysis_code typos to analysis_code
    { if (length(analysis_code_cols) == 1) dplyr::rename(., analysis_code = !!analysis_code_cols) else . }
  
  return(df_clean)
}

# Use function to clean headers
cleaned_tables <- purrr::map(cleaned_tables, cleaning_names)

## Check headers across tables ----
# Define function to check headers consistency
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

# Use function to check if all good
check_headers_consistency(cleaned_tables, "Cleaned Tables")

## Bind all dfs into one -----
final_results_df <- dplyr::bind_rows(cleaned_tables)

df <- final_results_df

## Filters for cleaning ----
# Formal and Non-formal Edu
df %<>%
  filter(type_of_education == "Formal education" |
           type_of_education == "Non-formal education")

# drop the GRN that are NA as they come from the longer templates
df %<>%
  filter(!is.na(grn))

## NA cleaning ----
# Count before cleaning
empty_before <- sum(df == "" | str_trim(df) == " ", na.rm = TRUE)
empty_before

# Clean 
df <- df %>%
  mutate(across(
    everything(),
    ~ ifelse(str_trim(.) == "" | . %in% c("", " ", "  ", "   ", "    ", "     "), NA, .)
  ))

# Count after
empty_after <- sum(is.na(df))

message("🧹 Cleaning complete. Empty values replaced with NA: ", empty_after - empty_before)

## Get LeadGRN from GRID ----
df %<>%
  mutate(LeadGRN = str_split(GRID, "\\|") %>% map_chr(~ str_trim(.x[5])))

## Assign source of data ----
df %<>%
  mutate(source_of_data = paste0("ARR", as.numeric(reporting_year) %% 100, " - CR"))

## Export date column ----
df %<>%
  mutate(ExportDate = today())

## Save ----
dir.create("data/arr/cr", recursive = TRUE, showWarnings = FALSE)
write.csv(df, "data/arr/cr/cr source data.csv", row.names = FALSE)

rm(list = ls())
