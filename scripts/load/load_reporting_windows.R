# LOAD Reporting Windows ----

## Prep env ----
source("config.yml")
source("requirements/libraries.R")

# Define path and file list
data_path <- ecw_db

## Extraction ----
# Define files domain for table search
files <- list.files(
  path = data_path,
  pattern = "ECW (Results|Programme) template.*\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

# # Step 1: Check which files contain 'ecw_reporting_windows' table embedded
# check_for_table <- function(file_path) {
#   tryCatch({
#     wb <- wb_load(file_path)
#     has_table <- "ecw_reporting_windows" %in% wb$tables$tab_name
#     if (has_table) {
#       message("✔ Found in: ", basename(file_path))
#       return(file_path)
#     } else {
#       message("⚠ Not found in: ", basename(file_path))
#       return(NA)
#     }
#   }, error = function(e) {
#     message("⛔ Error reading: ", basename(file_path), " — ", e$message)
#     return(NA)
#   })
# }
# 
# # Run it on all files
# valid_files <- purrr::map_chr(files, check_for_table) %>%
#   na.omit()


# Function to load the ecw_reporting_windows' table from the GrantsDB
load_reporting_windows_table <- function(file_path) {
  tryCatch({
    # Load workbook
    wb <- wb_load(file_path)
    
    # Filter for ecw_reporting_windows table
    rep_table <- wb$tables %>%
      dplyr::filter(tab_name == "ecw_reporting_windows")
    
    if (nrow(rep_table) == 0) {
      message("ℹ️ No 'ecw_reporting_windows' table in: ", basename(file_path))
      return(NULL)
    }
    
    # Take first occurrence (assuming one per file, if more, loop)
    tbl_info <- rep_table[1, ]
    sheet_name <- wb$sheet_names[tbl_info$tab_sheet]
    range <- tbl_info$tab_ref
    
    message("📥 Reading 'ecw_reporting_windows' from: ", basename(file_path), " → ", sheet_name, " @ ", range)
    
    # Load the table data
    df <- tryCatch({
      wb_to_df(wb, sheet = sheet_name, range = range)
    }, error = function(e) {
      message("⛔ Error reading range: ", range, " — ", e$message)
      return(NULL)
    })
    
    # If reading failed or is empty
    if (is.null(df) || nrow(df) == 0 || all(is.na(df))) {
      message("⚠️ Empty 'ecw_reporting_windows' table in: ", basename(file_path))
      return(NULL)
    }
    
    # ✅ Add file name column
    df$file_name <- basename(file_path)
    # ✅ Important: return df properly
    return(df)
    
  }, error = function(e) {
    message("⛔ Failed to process: ", basename(file_path), " — ", e$message)
    return(NULL)
  })
}

# Run load_reporting_windows_table() function on each file in the GrantsDB
reporting_windows <- purrr::map(files, load_reporting_windows_table)

## Processing ----

# Detect report year position in the df
report_year_positions <- purrr::map(reporting_windows, function(df) {
  # Search "Report Year" in the entire data frame
  which(df == "Report Year", arr.ind = TRUE)
})

# Detect report year row in the df
report_year_rows <- purrr::map_int(report_year_positions, function(pos) {
  if (nrow(pos) > 0) {
    return(min(pos[, "row"], na.rm = TRUE))
  } else {
    return(NA_integer_)
  }
})

# Extract df
cleaned_reporting_windows <- purrr::map2_dfr(
  reporting_windows,
  report_year_rows,
  function(df, header_row) {
    if (is.na(header_row)) return(NULL)  # skip if no header found
    
    # Slice from header row to end
    df_clean <- df[header_row:nrow(df), , drop = FALSE]
    
    # Assign header
    colnames(df_clean) <- df_clean[1, ]
    df_clean <- df_clean[-1, , drop = FALSE]
    
    # Clean up column names (empty → default), then make unique
    colnames(df_clean) <- ifelse(
      is.na(colnames(df_clean)) | trimws(colnames(df_clean)) == "",
      paste0("col_", seq_along(colnames(df_clean))),
      colnames(df_clean)
    )
    colnames(df_clean) <- make.names(colnames(df_clean), unique = TRUE)
    
    df_clean %>%
      mutate(across(everything(), as.character)) %>%
      janitor::clean_names() %>%
      filter(!if_all(everything(), ~ is.na(.) | . == ""))
  }
)

df <- cleaned_reporting_windows


df %<>%
  mutate(
    FileName = pmap_chr(across(starts_with("ecw_")), ~ {
      vals <- c(...)  # unpack arguments
      vals <- vals[!is.na(vals) & vals != ""]  # remove NA and empty strings
      if (length(vals) > 0) vals else NA_character_
    })
  )

df <- df[, 7:ncol(df)]

df %<>% filter(grepl("window", report_year, ignore.case = TRUE))
df %<>% filter(!is.na(reporting_period_up_to))
df %<>% filter(type_of_reporting != "Progress")

df %<>%
  mutate(GRID = ifelse(
    is.na(ar_grid_2021), grid, ar_grid_2021
  ))

df %<>%
  mutate(
    LeadGranteeGRN = str_split_fixed(GRID, " \\| ", 8)[,5] %>% str_trim(),
    LeadGranteeGRN = trimws(LeadGranteeGRN)
  )


## Join with GMS ----
grants_db <- read.csv("data/arr/grants_db.csv", encoding = "UTF-8")

df %<>%
  left_join(grants_db, by = c("LeadGranteeGRN" = "GMGRN"))

## Final cleaning and processing ----
df %<>%
  rename(GMGRN = LeadGranteeGRN)

df %<>%
  select(ProgrammeID,
         GMGRN,
         report_year,
         type_of_reporting,
         reporting_period_up_to,
         Currentenddate,
         submission_deadline, 
         m_e_review,
         education_team_review,
         GRID,
         FileName)

# Clean dates 
df %<>%
  mutate(
    reporting_period_up_to = as.Date(reporting_period_up_to, format = "%Y-%m-%d"),
    Currentenddate = as.Date(Currentenddate, format = "%Y-%m-%d")
  )

# CheckDays variable
df %<>%
  mutate(CheckDays = as.numeric(reporting_period_up_to - Currentenddate))


## Save ----
write.csv(df, "data/arr/rw/reporting_window.csv", row.names = FALSE)


rm(list = ls())
