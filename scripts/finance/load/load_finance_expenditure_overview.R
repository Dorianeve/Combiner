# LOAD Finance expenditure overview template ----

## Prep env ----
source("config.yml")
source("requirements/libraries.R")


### Import ----

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
load_ecw_finance_expenditures_overview_table <- function(file_path) {
  # Load workbook
  wb <- wb_load(file_path)
  
  # Check for the target table
  target_tables <- wb$tables %>%
    dplyr::filter(tab_name == "ecw_finance_expenditures_overview")
  
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
    load_ecw_finance_expenditures_overview_table(file)
  }) %>% purrr::compact() %>% purrr::flatten()
  result
})


# Step 5: Check results
length(all_results_list)  # ✅ You now have a list of data frames!

## Clean tables ----
# Header detection at column 2
clean_header_col2 <- function(df) {
  # Step 1: Prepare column 2 for scanning (clean and lower case)
  col2_clean <- df[[2]] %>%
    as.character() %>%
    stringr::str_replace_all("[\r\n\t]", " ") %>%    # remove \n
    stringr::str_replace_all("\\s+", " ") %>%        # collapse spaces
    trimws() %>%
    tolower()
  
  # Step 2: Find the first row number that contains the header — use regex!
  header_row_num <- which(str_detect(col2_clean, regex("description", ignore_case = TRUE)))[1]
  
  # Step 3: If not found, warn and return original
  if (is.na(header_row_num)) {
    warning("No header row found with 'description' in column 2. Returning original dataframe.")
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

  df_clean <- df_clean[1:6,]
  return(df_clean)
}

# Launch function
cleaned_tables <- lapply(all_results_list, clean_header_col2)

## Check headers consistency ----
# Check if first column is coherent
column_check <- map_lgl(cleaned_tables, ~ names(.x)[1] == "description")

# View results
print(column_check)

# if TRUE all the same first header
all(column_check)
problematic <- map_chr(cleaned_tables, ~ names(.x)[1])
problematic[which(problematic != "description")]

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


# Additional cleaning
cleaned_tables <- map(cleaned_tables, function(df) {
  # Step 1: Rename columns
  df <- df %>%
    rename(grid = matches("grid"))
  
  # Step 2: Remove columns starting with "na"
  df <- df %>%
    select(-matches("^na")) %>%
    select(-matches("^x")) %>%
    select(-c(helper))
  
  if ("column1" %in% names(df)) {
    df <- df %>%
      select(-column1)
  }
  
  if (!"grid" %in% names(df)) {
    df <- df %>%
      mutate(grid = NA)
  }
  
  # Step 3: Rename column starting with "ecw_programme" to "file_name"
  prog_col <- names(df)[str_detect(names(df), "ecw_")]
  
  if (length(prog_col) == 1) {
    df <- df %>%
      rename(file_name = all_of(prog_col))
  } else if (length(prog_col) > 1) {
    warning("⚠️ More than one 'ecw_' column found — not renaming.")
  } else {
    warning("⚠️ No 'ecw_' column found — skipping rename.")
  }
  
  # Done
  return(df)
})


check_headers_consistency(cleaned_tables, "Cleaned tables")

## Bind dfs into one big dataframe ----
df <- dplyr::bind_rows(cleaned_tables)


## Get GMGRN ----
df %<>%
  mutate(
    GMGRN = str_extract(
      grid,
      "\\b\\d{2}-[A-Z]{3}(?:-[A-Z0-9]{2,5}){3,}\\b"
    )
  )

# Addirional cleaning
df %<>%
  mutate(percentage = ifelse(percentage == "N/A", NA, percentage),
         percentage = gsub("[^0-9.]", "", percentage), # Remove non-numeric, non-decimal characters
         percentage = as.numeric(percentage), 
         cumulative_expenditure = ifelse(cumulative_expenditure == "N/A", NA, cumulative_expenditure),
         cumulative_expenditure = gsub("[^0-9.]", "", cumulative_expenditure), # Remove non-numeric, non-decimal characters
         cumulative_expenditure = as.numeric(cumulative_expenditure))          # Convert to numeric (float)

# Save output ----
dir.create("data/finance combiner/", recursive = TRUE, showWarnings = FALSE)
write.csv(df, "data/finance combiner/finance_expenditure_overview.csv", row.names = FALSE)

rm(list = ls())