# LOAD AF template ----


## Prep env ----
source("config.yml")
source("requirements/libraries.R")


## Import ----

# Step 1: Define your folder path (from config file)
main_folder <- ecw_db

# Step 2: Find all Excel files recursively that contain "ECW (Programme/ Aligned Funding) template"
all_files <- list.files(main_folder, 
                        pattern = "ECW (Programme|Aligned funding) template.*\\.xlsx$", 
                        recursive = TRUE,
                        full.names = TRUE,
                        ignore.case = TRUE
                        )

# Step 3: Define function to load the target table only "ecw_leverage_funding"
load_ecw_results_table <- function(file_path) {
  # Load workbook
  wb <- wb_load(file_path)
  
  # Check for the target table
  target_tables <- wb$tables %>%
    dplyr::filter(tab_name == "ecw_leverage_funding")
  
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


## Processing ----
### Identify table ----
## Function to get specific table range using textual search on column 2
clean_table <- function(df) {
  # Find the row where column 2 is 'donor' or 'donateur'
  header_row_index <- which(
    tolower(trimws(as.character(df[[2]]))) %in% c("donor", "donateur")
  )
  
  if (length(header_row_index) == 0) {
    stop("Header row not found in any row of column 2.")
  }
  
  header_row_index <- header_row_index[1]  # use the first match
  header_row <- df[header_row_index, ] %>% as.character()
  
  # Remove all rows up to and including the header, drop first column
  df_clean <- df[-(1:header_row_index), -1, drop = FALSE]
  
  # Apply cleaned headers
  colnames(df_clean) <- header_row[-1] %>% janitor::make_clean_names()
  
  # Clean up: trim spaces and remove empty rows
  df_clean <- df_clean %>%
    dplyr::mutate(across(everything(), ~ trimws(as.character(.)))) %>%
    dplyr::filter(!if_all(everything(), ~ .x == "" | is.na(.x)))
  
  return(df_clean)
}

# iterate over the list
cleaned_tables <- lapply(all_results_list, clean_table)

# Check if first column is "donor"|"donateur" in all data frames
column_check <- map_lgl(cleaned_tables, ~ names(.x)[1] == "donor" | names(.x)[1] == "donateur")

# View results
print(column_check)

# if TRUE all the same first header, means OK
all(column_check)
problematic <- map_chr(cleaned_tables, ~ names(.x)[1])
problematic[which(problematic != "donor" | problematic != "donateur")]

# adjust french headers
adjust_headers_french <- function(tbl_list) {
  map(tbl_list, function(df) {
    first_col <- names(df)[1]
    
    if (first_col == "donateur") {
      # Take second row as new header
      new_header <- as.character(df[1, ])
      
      # Set column names
      names(df) <- new_header
      header_row <- names(df)
      colnames(df) <- header_row %>% janitor::make_clean_names()
      
      # Remove first two rows (old header + row used for header)
      df <- df[-1, ]
      
      # Reset row numbers
      rownames(df) <- NULL
    }
    
    return(df)
  })
}

cleaned_tables <- adjust_headers_french(cleaned_tables)

# adjust headers second line "donor"
adjust_headers <- function(tbl_list) {
  map(tbl_list, function(df) {
    first_cell <- df[1, 1, drop = TRUE]
    
    if (!is.na(first_cell) && (first_cell == "Donor" | first_cell == "Donateur" | first_cell == "donor" | first_cell == "donateur")) {
      # Take second row as new header
      new_header <- as.character(df[1, ])
      
      # Set column names
      names(df) <- new_header
      
      # Clean column names
      names(df) <- names(df) %>% janitor::make_clean_names()
      
      # Remove first two rows (old header + row used for header)
      df <- df[-1, ]
      
      # Reset row numbers
      rownames(df) <- NULL
    }
    
    return(df)
  })
}



cleaned_tables <- adjust_headers(cleaned_tables)

# Verification first col header
# Check if first column is "donor"|"donateur in all data frames
column_check <- map_lgl(cleaned_tables, ~ names(.x)[1] == "donor" | names(.x)[1] == "donateur")

# View results
print(column_check)

### Header consistency ----
# Function to get if all the headers are the same in each dataframe
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

# clean headers
clean_columns <- function(tbl_list) {
  map(tbl_list, function(df) {
    
    # 1️⃣ Remove columns starting with "na"
    df <- df %>% select(-starts_with("na"))
    
    # 2️⃣ Rename columns starting with "ecw_programme" to "file_name"
    df <- df %>% rename(file_name = matches("^ecw_"))
    
    # Add US funding cuts column
    if (!"please_specify_the_total_amount_and_percentage_of_dedicated_awards_impacted_by_the_u_s_funding_cuts_for_2025_relative_to_the_overall_grant" %in% names(df)) {
      df <- df %>%
        dplyr::mutate(
          please_specify_the_total_amount_and_percentage_of_dedicated_awards_impacted_by_the_u_s_funding_cuts_for_2025_relative_to_the_overall_grant = NA
        )
    }
    
    # fix grid
    df <- df %>% rename(grid = matches("^ar_grid_"))
    
    return(df)
  })
}

cleaned_tables <- clean_columns(cleaned_tables)
check_headers_consistency(cleaned_tables, "Cleaned Tables")

## Binding dataframes ----
# Bind into one big dataframe
final_results_df <- dplyr::bind_rows(cleaned_tables)

df <- final_results_df

## Additional cleaning ----
# Drop the GRN that are NA as they come from the longer templates
df %<>%
  filter(!is.na(grantee_lead_grn))

## Get LeadGRN form GRID ----
df %<>%
  mutate(LeadGRN = str_split(grid, "\\|") %>% map_chr(~ str_trim(.x[5])))


# NA cleaning
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


## Specify Source of Data ----
df %<>%
  mutate(source_of_data = "Finance Aligned Funding")

## Save output ----
dir.create("data/finance combiner/aligned funding", recursive = TRUE, showWarnings = FALSE)
write.csv(df, "data/finance combiner/aligned funding/af source data.csv", row.names = FALSE)

rm(list = ls())
