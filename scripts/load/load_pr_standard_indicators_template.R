# LOAD PR Standard Indicators ----

## Prep env ----
source("config.yml")
source("requirements/libraries.R")

### Import ----
# Step 1: Define your folder path
main_folder <- ecw_db

# Step 2: Find all Excel files recursively that contain "ECW (Results) template"
all_files <- list.files(main_folder, pattern = "ECW (Results|Programme) template.*\\.xlsx$", 
                        recursive = TRUE, 
                        full.names = TRUE,
                        ignore.case = TRUE)

# Step 3: Define function to load the target table only
load_ecw_repstandards_table <- function(file_path) {
  # Load workbook
  wb <- wb_load(file_path)
  
  # Check for the target table
  target_tables <- wb$tables %>%
    dplyr::filter(tab_name == "ecw_reporting_standard_indicators")
  
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

all_results_list <- with_progress({
  p <- progressor(along = all_files)
  result <- purrr::map(all_files, function(file) {
    p(message = basename(file))
    load_ecw_repstandards_table(file)
  }) %>% purrr::compact() %>% purrr::flatten()
  result
})


## Clean tables ----
# Header detection: header 6 line col 2
clean_table <- function(df) {
  # Extract header row
  header_row <- df[8, ] %>% as.character()
  # Skip the first column and header row
  df_clean <- df[-(1:8), -1, drop = FALSE]
  # Apply header
  colnames(df_clean) <- header_row[-1] %>% janitor::make_clean_names()
  # Clean up: remove empty rows, trim spaces
  df_clean <- df_clean %>%
    dplyr::mutate(across(everything(), ~ trimws(as.character(.)))) %>%
    dplyr::filter(!if_all(everything(), ~ .x == "" | is.na(.x)))
  return(df_clean)
}


cleaned_tables <- lapply(all_results_list, clean_table)


# Check if first column is "outcome_output_numbering" in all data frames
column_check <- map_lgl(cleaned_tables, ~ names(.x)[1] == "level")

# View results
print(column_check)

# if TRUE all the same first header
all(column_check)
problematic <- map_chr(cleaned_tables, ~ names(.x)[1])
problematic[which(problematic != "level")]

## Clean headers ----
cleaning_names <- function(df) {
  # Find ecw_results_template_ columns
  ecw_cols <- grep("^ecw_results_template_", names(df), value = TRUE)
  
  # Find mistyped analysis_code column
  analysis_code_cols <- names(df)[names(df) %in% c("analysis_code", "aysis_code", "ecw_anays_code", "anaysis_code",
                                                   "anays_code")]
  
  df_clean <- df %>%
    dplyr::select(-dplyr::starts_with("na_")) %>%
    # Conditionally rename ecw_results_template_* to file_name
    { if (length(ecw_cols) == 1) dplyr::rename(., file_name = !!ecw_cols) else . } %>%
    # Conditionally rename ar_grid_2021 to GRID
    { if ("ar_grid_2021" %in% names(.)) dplyr::rename(., GRID = !!sym("ar_grid_2021")) else . } %>%
    # Conditionally rename analysis_code typos to analysis_code
    { if (length(analysis_code_cols) == 1) dplyr::rename(., analysis_code = !!analysis_code_cols) else . }
  
  return(df_clean)
}

cleaned_tables <- purrr::map(cleaned_tables, cleaning_names)

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


check_headers_consistency(cleaned_tables, "Cleaned Tables")


# Binding the clean tables in the list into a big one
final_results_df <- dplyr::bind_rows(cleaned_tables)

df <- final_results_df

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

## Get LeadGRN ----
# From GRID
df %<>%
  mutate(LeadGRN = str_split(GRID, "\\|") %>% map_chr(~ str_trim(.x[5])))

## Source of data ----
df %<>%
  mutate(source_of_data = paste0("ARR", as.numeric(reporting_year) %% 100, " - SI"))

## Export date column ----
df %<>%
  mutate(ExportDate = today())

# Save output ----
write.csv(df, "data/arr/pr/pr_standard_indicators.csv", row.names = FALSE)

rm(list = ls())

