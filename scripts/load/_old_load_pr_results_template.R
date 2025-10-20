library(openxlsx)
library(dplyr)
library(purrr)
library(janitor)
library(magrittr)
library(openxlsx2)
library(stringr)


### IMPORT ---

# Step 1: Define your folder path
main_folder <- "C:/Users/claud/Education Cannot Wait/ECW Grants - Grants DB"

# Step 2: Find all Excel files recursively that contain "ECW (Results) template"
all_files <- list.files(main_folder, pattern = "ECW (Results) template.*\\.xlsx$", recursive = TRUE, full.names = TRUE)

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

# Optional: inspect the first one
glimpse(all_results_list[[1]])

# Assign a structure ID to each table
column_structures <- all_results_list %>%
  map(~ names(.x)) %>%
  map_chr(~ paste(sort(.x), collapse = "|"))

structure_mapping <- tibble(
  table_index = seq_along(all_results_list),
  structure_id = factor(column_structures)
)

# Split the list by structure
tables_by_structure <- split(all_results_list, structure_mapping$structure_id)

names(tables_by_structure) <- c("group_1", "group_2", "group_3", "group_4",
                                "group_5", "group_6", "group_7", "group_8", "group_9")

clean_group1_table <- function(df) {
  # Extract header row
  header_row <- df[5, ] %>% as.character()
  # Skip the first column and header row
  df_clean <- df[-(1:5), -1, drop = FALSE]
  # Apply header
  colnames(df_clean) <- header_row[-1] %>% janitor::make_clean_names()
  # Clean up: remove empty rows, trim spaces
  df_clean <- df_clean %>%
    dplyr::mutate(across(everything(), ~ trimws(as.character(.)))) %>%
    dplyr::filter(!if_all(everything(), ~ .x == "" | is.na(.x)))
  return(df_clean)
}

clean_group_1 <- purrr::map(tables_by_structure[["group_1"]], clean_group1_table)

cleaning_names_group1_table <- function(df) {
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

clean_group_1 <- purrr::map(clean_group_1, cleaning_names_group1_table)

clean_group2_table <- function(df) {
  # Extract header row
  header_row <- df[5, ] %>% as.character()
  # Skip the first column and header row
  df_clean <- df[-(1:5), -1, drop = FALSE]
  # Apply header
  colnames(df_clean) <- header_row[-1] %>% janitor::make_clean_names()
  # Clean up: remove empty rows, trim spaces
  df_clean <- df_clean %>%
    dplyr::mutate(across(everything(), ~ trimws(as.character(.)))) %>%
    dplyr::filter(!if_all(everything(), ~ .x == "" | is.na(.x)))
  return(df_clean)
}

clean_group_2 <- purrr::map(tables_by_structure[["group_2"]], clean_group2_table)

cleaning_names_group2_table <- function(df) {
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

clean_group_2 <- purrr::map(clean_group_2, cleaning_names_group2_table)

clean_group3_table <- function(df) {
  # Extract header row
  header_row <- df[6, ] %>% as.character()
  # Skip the first column and header row
  df_clean <- df[-(1:6), -1, drop = FALSE]
  # Apply header
  colnames(df_clean) <- header_row[-1] %>% janitor::make_clean_names()
  # Clean up: remove empty rows, trim spaces
  df_clean <- df_clean %>%
    dplyr::mutate(across(everything(), ~ trimws(as.character(.)))) %>%
    dplyr::filter(!if_all(everything(), ~ .x == "" | is.na(.x)))
  return(df_clean)
}

clean_group_3 <- purrr::map(tables_by_structure[["group_3"]], clean_group3_table)

cleaning_names_group3_table <- function(df) {
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

clean_group_3 <- purrr::map(clean_group_3, cleaning_names_group3_table)


clean_group4_table <- function(df) {
  # Extract header row
  header_row <- df[5, ] %>% as.character()
  # Skip the first column and header row
  df_clean <- df[-(1:5), -1, drop = FALSE]
  # Apply header
  colnames(df_clean) <- header_row[-1] %>% janitor::make_clean_names()
  # Clean up: remove empty rows, trim spaces
  df_clean <- df_clean %>%
    dplyr::mutate(across(everything(), ~ trimws(as.character(.)))) %>%
    dplyr::filter(!if_all(everything(), ~ .x == "" | is.na(.x)))
  return(df_clean)
}

clean_group_4 <- purrr::map(tables_by_structure[["group_4"]], clean_group4_table)

cleaning_names_group4_table <- function(df) {
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

clean_group_4 <- purrr::map(clean_group_4, cleaning_names_group4_table)

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

check_headers_consistency(clean_group_1, "Group 1")
check_headers_consistency(clean_group_2, "Group 2")
check_headers_consistency(clean_group_3, "Group 3")
check_headers_consistency(clean_group_4, "Group 4")

compare_group_headers <- function(group_list, group_names = names(group_list)) {
  # Extract and sort unique column names per group
  headers <- purrr::map(group_list, ~ sort(names(.x[[1]])))  # assuming all tables in group already harmonized
  
  # Compare groups pairwise
  for (i in seq_along(headers)) {
    for (j in seq_along(headers)) {
      if (i < j) {
        cat("\n📊 Comparison: ", group_names[i], " vs ", group_names[j], "\n")
        
        missing_in_j <- setdiff(headers[[i]], headers[[j]])
        missing_in_i <- setdiff(headers[[j]], headers[[i]])
        
        if (length(missing_in_j) == 0 && length(missing_in_i) == 0) {
          cat("✅ Headers are identical!\n")
        } else {
          if (length(missing_in_j) > 0) cat("❌ In ", group_names[j], " but missing: ", paste(missing_in_j, collapse = ", "), "\n")
          if (length(missing_in_i) > 0) cat("❌ In ", group_names[i], " but missing: ", paste(missing_in_i, collapse = ", "), "\n")
        }
      }
    }
  }
}

group_list <- list(
  "Group 1" = clean_group_1,
  "Group 2" = clean_group_2,
  "Group 3" = clean_group_3,
  "Group 4" = clean_group_4
)

compare_group_headers(group_list)


# Step 1: Extract column names per group
group_columns <- list(
  group_1 = unique(unlist(purrr::map(clean_group_1, names))),
  group_2 = unique(unlist(purrr::map(clean_group_2, names))),
  group_3 = unique(unlist(purrr::map(clean_group_3, names))),
  group_4 = unique(unlist(purrr::map(clean_group_4, names)))
)

# Step 2: Create full union of columns
all_columns <- unique(unlist(group_columns))

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

# Process each group
patched_group_1 <- add_missing_columns_with_diagnostics(clean_group_1, "group_1", all_columns)
patched_group_2 <- add_missing_columns_with_diagnostics(clean_group_2, "group_2", all_columns)
patched_group_3 <- add_missing_columns_with_diagnostics(clean_group_3, "group_3", all_columns)
patched_group_4 <- add_missing_columns_with_diagnostics(clean_group_4, "group_4", all_columns)

# Combine all diagnostics
all_diagnostics <- dplyr::bind_rows(
  patched_group_1$diagnostics,
  patched_group_2$diagnostics,
  patched_group_3$diagnostics,
  patched_group_4$diagnostics
)

# View diagnostics
print(all_diagnostics)

# Flatten all lists into one list of dataframes
all_clean_tables <- purrr::list_flatten(list(
  patched_group_1$data,
  patched_group_2$data,
  patched_group_3$data,
  patched_group_4$data
))

# Bind into one big dataframe
final_results_df <- dplyr::bind_rows(all_clean_tables)

df <- final_results_df

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

write.csv(df, "data/pr_results.csv", row.names = FALSE)
