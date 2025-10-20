rm(list = ls())
source("config.yml")
library(openxlsx)
library(dplyr)
library(purrr)
library(janitor)
library(magrittr)
library(openxlsx2)
library(stringr)


### IMPORT ---

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


clean_pr_table <- function(df) {
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


# THis is to use when I group in a list of lists. But this does not work here
# # Flatten the list to extract data frames
# flattened_tables <- all_results_list %>%
#   map(~ .x[[1]])  # this takes the first (and usually only) data frame inside each sublist
cleaned_tables <- lapply(all_results_list, clean_pr_table)


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
  df[!df[[1]] == "Outcome / output numbering", ]
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


# Headers consistency per group ----
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

check_headers_consistency(cleaned_tables, "Cleaned Tables")

unique_columns <- cleaned_tables |>
  purrr::map(names) |>
  unlist() |>
  unique() |>
  sort()
print(unique_columns)


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

cleaned_tables <- add_missing_columns_with_diagnostics(cleaned_tables, "Cleaned Tables", unique_columns)

# View diagnostics
print(cleaned_tables$diagnostics)

all_clean_tables <- purrr::list_flatten(list(cleaned_tables$data))

# Bind into one big dataframe
final_results_df <- dplyr::bind_rows(all_clean_tables)


df <- final_results_df

# names housekeeping
df %<>%
  mutate(source_of_verification = ifelse(!is.na(source_of_verification), 
                                         paste0(source_of_verification, "|", means_of_verification),
                                         means_of_verification)) %>%
           select(-c(means_of_verification))

df %<>%
  rename(ecw_analysis_code = ecw_anaysis_code) %>%
  select(-c(analysis_code))

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

# get LeadGRN
df %<>%
  mutate(LeadGRN = str_split(GRID, "\\|") %>% map_chr(~ str_trim(.x[5])))

df %<>%
  mutate(source_of_data = "ARR24 - RT")


# Fix headers
mapping <- data.frame(
  Results = c(
    "adapted_accessible", "adapted_accessible_2", "adapted_accessible_3",
    "adapted_accessible_4", "adapted_accessible_5", "adapted_accessible_6",
    "adapted_accessible_7", "adapted_accessible_8", "adapted_accessible_9",
    
    "comments", "comments_2", "comments_3", "comments_4",
    "comments_5", "comments_6", "comments_7",
    
    "comments_details_rw1", "comments_details_rw2", "comments_details_rw3",
    "comments_details_rw4", "comments_details_rw5", "comments_details_rw6",
    
    "ecw_analysis_code",
    
    "female", "female_2", "female_3", "female_4",
    "female_5", "female_6",
    
    "male", "male_2", "male_3", "male_4",
    "male_5", "male_6",
    
    "other_levels_of_disaggregation", "other_levels_of_disaggregation_2",
    "other_levels_of_disaggregation_3", "other_levels_of_disaggregation_4",
    "other_levels_of_disaggregation_5", "other_levels_of_disaggregation_6",
    "other_levels_of_disaggregation_7",
    
    "total", "total_2", "total_3", "total_4", "total_5", "total_6"
  ),
  RevisedResults = c(
    "baseline_adapted_accessible", "adapted_accessible_target", "adapted_accessible_revised_target",
    "adapted_accessible_rw1", "adapted_accessible_rw2", "adapted_accessible_rw3",
    "adapted_accessible_rw4", "adapted_accessible_rw5", "adapted_accessible_rw6",
    
    "comments_rw1", "comments_rw2", "comments_rw3", "comments_rw4",
    "comments_rw5", "comments_rw6", "comments_rw7",
    
    "comments_details_rw1", "comments_details_rw2", "comments_details_rw3",
    "comments_details_rw4", "comments_details_rw5", "comments_details_rw6",
    
    "analysis_code",
    
    "female_rw1", "female_rw2", "female_rw3", "female_rw4",
    "female_rw5", "female_rw6",
    
    "male_rw1", "male_rw2", "male_rw3", "male_rw4",
    "male_rw5", "male_rw6",
    
    "other_levels_of_disaggregation_revised_target", "other_levels_of_disaggregation_rw1",
    "other_levels_of_disaggregation_rw2", "other_levels_of_disaggregation_rw3",
    "other_levels_of_disaggregation_rw4", "other_levels_of_disaggregation_rw5",
    "other_levels_of_disaggregation_rw6",
    
    "total_rw1", "total_rw2", "total_rw3", "total_rw4", "total_rw5", "total_rw6"
  ),
  stringsAsFactors = FALSE
)

# Copy original dataframe
df_merged <- df

# Apply renaming and merging
for (i in seq_len(nrow(mapping))) {
  from_col <- mapping$Results[i]
  to_col <- mapping$RevisedResults[i]
  
  if (!is.na(from_col) && !is.na(to_col) && from_col %in% names(df_merged)) {
    if (to_col %in% names(df_merged)) {
      # Merge non-NA values
      df_merged[[to_col]] <- coalesce(df_merged[[to_col]], df_merged[[from_col]])
      df_merged[[from_col]] <- NULL
    } else {
      # Rename column
      names(df_merged)[names(df_merged) == from_col] <- to_col
    }
  }
}

write.csv(df_merged, "data/arr/pr/pr_results.csv", row.names = FALSE)
