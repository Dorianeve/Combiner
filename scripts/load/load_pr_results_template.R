# LOAD PR Results Template ----

## Prep env ----
source("config.yml")
source("requirements/libraries.R")


## Load data ----

# Step 1: Define your folder path
main_folder <- ecw_db

# Step 2: Find all Excel files recursively that contain "ECW (Results) template"
all_files <- list.files(main_folder, pattern = "ECW (Results) template.*\\.xlsx$", 
                        recursive = TRUE, 
                        full.names = TRUE, 
                        ignore.case = TRUE)

# Step 3: Define function to load the target table only
load_ecw_results_table <- function(file_path) {
  # Load workbook
  wb <- wb_load(file_path)
  
  # Check for the target table
  target_tables <- wb$tables %>%
    dplyr::filter(tab_name == "ecw_ar_results_framework")
  
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
    load_ecw_results_table(file)
  }) %>% purrr::compact() %>% purrr::flatten()
  result
})


# Step 5: Check results
length(all_results_list)  # ✅ You now have a list of data frames!


## Clean tables ----
clean_pr_table <- function(df) {
  # Step 1: Prepare column 2 for scanning (clean and lower case)
  col2_clean <- df[[2]] %>%
    as.character() %>%
    trimws() %>%
    tolower()
  
  # Step 2: Find the first row number that contains "type of education"
  header_row_num <- which(col2_clean == "outcome / output numbering")[2]
  
  # Step 3: If not found, warn and return original
  if (is.na(header_row_num)) {
    warning("No header row found with 'Outcome / output numbering' in column 2. Returning original dataframe.")
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

# use the function on the list of clean dataframes
cleaned_tables <- lapply(all_results_list, clean_pr_table)

# Check if first column is "outcome_output_numbering" in all data frames
column_check <- map_lgl(cleaned_tables, ~ names(.x)[1] == "outcome_output_numbering")

# View results
print(column_check)

# if TRUE all the same first header
all(column_check)
problematic <- map_chr(cleaned_tables, ~ names(.x)[1])
problematic[which(problematic != "outcome_output_numbering")]


# Clean in the other column
clean_pr_table_col2 <- function(df) {
  # Step 1: Prepare column 2 for scanning (clean and lower case)
  col2_clean <- df[[2]] %>%
    as.character() %>%
    trimws() %>%
    tolower()
  
  # Step 2: Find the first row number that contains "type of education"
  header_row_num <- which(col2_clean == "outcome / output numbering")[1]
  
  # Step 3: If not found, warn and return original
  if (is.na(header_row_num)) {
    warning("No header row found with 'Outcome / output numbering' in column 2. Returning original dataframe.")
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


cleaned_tables <- lapply(cleaned_tables, clean_pr_table_col2)

# Check if first column is "outcome_output_numbering" in all data frames
column_check <- map_lgl(cleaned_tables, ~ names(.x)[1] == "outcome_output_numbering")

# View results
print(column_check)

# if TRUE all the same first header
all(column_check)
problematic <- map_chr(cleaned_tables, ~ names(.x)[1])
problematic[which(problematic != "outcome_output_numbering")]

# Remove rows where the first column has "Outcome / output numbering"
cleaned_tables <- lapply(cleaned_tables, function(df) {
  df[is.na(df[[1]]) | df[[1]] != "Outcome / output numbering", ]
})

# Check if first row is not "Outcome / output numbering" in all data frames
first_row_check <- map_lgl(cleaned_tables, ~ {
  val <- .x[1, 1]
  is.na(val) || val != "Outcome / output numbering"
})

# View results
print(first_row_check)

# if TRUE all the same first header
all(first_row_check)
problematic <- map_chr(cleaned_tables, ~ .x[1, 1])
problematic[which(problematic == "Outcome / output numbering")]

# Cleaning headers
cleaning_headers_results_template <- function(df) {
  # ✨ Apply your rules:
  df_clean <- df %>%
    # Remove columns starting with "na_"
    dplyr::select(-c(dplyr::starts_with("na_"))) %>%
    # Rename ecw_results_template_* to FileName
    dplyr::rename_with(~ "file_name", .cols = tidyselect::matches("^ecw_results_template_")) %>%
    # Rename ar_grid_2021 to GRID
    dplyr::rename(GRID = ar_grid_2021)
  return(df_clean)
}

cleaned_tables <- purrr::map(cleaned_tables, cleaning_headers_results_template)

## Clean headers -----
# Headers consistency per group
check_headers_consistency <- function(df_list, group_name = "Group") {
  # Get column names per table
  colnames_list <- purrr::map(df_list, ~ sort(names(.x)))
  
  # Get unique column name sets
  unique_colnames <- unique(colnames_list)
  
  # Report
  if (length(unique_colnames) == 1) {
    message("✅ ", group_name, ": All tables have consistent headers.")
  } else {
    message("⚠️ ", group_name, ": Inconsistent headers found. Number of unique sets: ", length(unique_colnames))
    
    # Compare all to the first set
    ref <- unique_colnames[[1]]
    purrr::walk2(colnames_list, seq_along(colnames_list), function(cols, idx) {
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

# Launch function
check_headers_consistency(cleaned_tables, "Cleaned Tables")

# |> is the equivalent as %>%
# this gets a list of unique column as the template is messy
unique_columns <- cleaned_tables |>
  purrr::map(names) |>
  unlist() |>
  unique() |>
  sort()
print(unique_columns)

# function to add the missing column to each dataframe with the diagnostics output
add_missing_columns_with_diagnostics <- function(df_list, group_name, all_columns) {
  # Initialize log
  diagnostics <- tibble::tibble()
  
  # Process each table in the list
  patched_list <- purrr::map(df_list, function(df) {
    # Detect missing columns
    missing_cols <- setdiff(all_columns, names(df))
    
    # If missing, add to diagnostics
    if (length(missing_cols) > 0) {
      diagnostics <<- dplyr::bind_rows(
        diagnostics,
        tibble::tibble(
          group = group_name,
          table = if ("file_name" %in% names(df)) unique(df$file_name) else "unknown_file",
          added_columns = paste(missing_cols, collapse = ", ")
        )
      )
    }
    
    # Add missing columns with NA
    df[missing_cols] <- NA
    
    # Reorder columns
    df <- df %>% dplyr::select(all_of(all_columns))
    
    return(df)
  })
  
  # Return both the patched list and diagnostics
  list(data = patched_list, diagnostics = diagnostics)
}

# launch the function on all the dataframes
cleaned_tables <- add_missing_columns_with_diagnostics(cleaned_tables, "Cleaned Tables", unique_columns)

# View diagnostics
print(cleaned_tables$diagnostics)

# Flatten the list
all_clean_tables <- purrr::list_flatten(list(cleaned_tables$data))

# Bind into one df ----
final_results_df <- dplyr::bind_rows(all_clean_tables)

df <- final_results_df

# Additional headers housekeeping ----
df %<>%
  mutate(source_of_verification = ifelse(!is.na(source_of_verification), 
                                         paste0(source_of_verification, "|", means_of_verification),
                                         means_of_verification)) %>%
  select(-c(means_of_verification))

df %<>%
  rename(ecw_analysis_code = ecw_anaysis_code) %>%
  select(-c(analysis_code))

df %<>%
  mutate(comments_rw1 = paste0(comments_rw1, " | ", comments_rw1_2),
         comments_rw2 = paste0(comments_rw2, " | ", comments_rw2_2),
         comments_rw3 = paste0(comments_rw3, " | ", comments_rw3_2),
         comments_rw4 = paste0(comments_rw4, " | ", comments_rw4_2),
         comments_rw5 = paste0(comments_rw5, " | ", comments_rw5_2),
         comments_rw6 = paste0(comments_rw6, " | ", comments_rw6_2),
         comments_baseline = paste0(comments_baseline, " | ", baseline_comments),
         comments_target = paste0(comments_target, " | ", target_comments)) %>%
  select(-c(comments_rw1_2, comments_rw2_2, comments_rw3_2,
            comments_rw4_2, comments_rw5_2, comments_rw6_2, baseline_comments, target_comments))


# Get LeadGRN ----
df %<>%
  mutate(LeadGRN = str_split(GRID, "\\|") %>% map_chr(~ str_trim(.x[5])))


# NA cleaning ----
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



# Source of data variable ----
df %<>%
  mutate(source_of_data = paste0("ARR", as.numeric(reporting_year) %% 100, " - RT"))

## Export date column ----
df %<>%
  mutate(ExportDate = today())

# Output ----
write.csv(df, "data/arr/pr/pr_results.csv", row.names = FALSE)

rm(list = ls())
