
#setup ----
rm(list = ls())
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readxl)
library(lubridate)
# 
# source("scripts/prep_env.R")
today <- today()
cutoff_date <- as.Date("2025-05-21")
#folder_path <- paste0("/Users/carlytubbs/Library/CloudStorage/OneDrive-SharedLibraries-UNICEF/Education Cannot Wait - Documents/Monitoring and Evaluation/09 - System improvements/06 - GMS-IM/01 - R Combiners/01 - CR/01 - Codebooks/", Sys.Date(), "/")
#dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

#kobo api ----
username <- username_kobo
password <- password_kobo  # Preferably use environment variables for security
token <- token_kobo
asset_uid <- "a3ccqSDG8VAf6CcbZVRDMX"
# API endpoint to request XLSX export
export_url <- paste0("https://eu.kobotoolbox.org/api/v2/assets/", asset_uid, "/exports/")

export_body <- list(
  type = "xls",
  lang = "_xml",                     # '_xml' gives labels in that language
  fields_from_all_versions = FALSE,
  hierarchy_in_labels = FALSE,
  group_sep = "/",
  multiple_select = "both"        # ✅ 'compact' = single column with space-separated values
)

# Send the export request with proper token header
res <- POST(
  url = export_url,
  body = export_body,
  encode = "json",
  add_headers(Authorization = paste("Token", token))
)
export_response <- content(res, as = "parsed", encoding = "UTF-8")
export_response
uid <- export_response$uid
export_url <- paste0("https://eu.kobotoolbox.org/api/v2/assets/a3ccqSDG8VAf6CcbZVRDMX/exports/", uid, "/")
repeat {
  res_status <- GET(
    url = export_url,
    add_headers(Authorization = paste("Token", token))
  )
  
  export_status <- content(res_status, as = "parsed", encoding = "UTF-8")
  cat("Status:", export_status$status, "\n")
  
  if (export_status$status == "complete") {
    file_url <- export_status$result
    break
  }
  
  Sys.sleep(3)  # wait 3 seconds before retrying
}

res_file <- GET(
  url = file_url,
  write_disk("data/aoe_kobo_export.xlsx", overwrite = TRUE),
  add_headers(Authorization = paste("Token", token))
)
# Check if successful
if (status_code(res_file) == 200) {
  cat("✅ File downloaded successfully: kobo_export.xlsx\n")
} else {
  cat("❌ Failed to download file. Status:", status_code(res_file), "\n")
}

# initial kobo cleaning ----
file_path <- "data/aoe_kobo_export.xlsx"
sheet_names <- excel_sheets(file_path)

# Split by underscore
split_names <- str_split(sheet_names, "_")

# Generate base names (first part)
base_names <- map_chr(split_names, ~ .x[1])

# Identify duplicates
dups <- base_names[duplicated(base_names) | duplicated(base_names, fromLast = TRUE)]

# For duplicates, append second part if available
short_names <- map2_chr(split_names, base_names, function(parts, base) {
  if (base %in% dups && length(parts) > 1) {
    paste0(base, "_", parts[2])
  } else {
    base
  }
})

# Optional: clean up names (make valid R variable names)
short_names <- make.names(tolower(short_names), unique = TRUE)


# Now remove "_repeat" suffix if still present
short_names <- gsub("_repeat$", "1", short_names)
short_names <- gsub("_repeat.1$", "2", short_names)
short_names <- gsub("_repeat.2$", "3", short_names)

# Optional: show result
short_names

# Read and assign each sheet
walk2(sheet_names, short_names, function(raw, short) {
  df <- read_excel(file_path, sheet = raw)
  assign(short, df, envir = .GlobalEnv)
})

## rename main df ----
df <- ecw.availability.of.evidence...
rm(ecw.availability.of.evidence...)

## load labels ----

survey <- read_excel("data/utilities/aoe_kobo_codes.xlsx", sheet = "survey")
choices <- read_excel("data/utilities/aoe_kobo_codes.xlsx", sheet = "choices")

# --- Step 1: Create a map from question name to list_name (for select_one and select_multiple) ---
survey_map <- survey %>%
  filter(str_detect(type, "select_one|select_multiple")) %>%
  mutate(
    type_full = type,
    type = if_else(str_detect(type, "select_multiple"), "select_multiple", "select_one"),
    list_name = str_remove(type_full, "select_one |select_multiple ")
  ) %>%
  select(name, list_name, type)

# --- Step 2: Create a named list of lookup tables per list_name ---
choice_maps <- choices %>%
  mutate(name = as.character(name)) %>%
  split(.$list_name) %>%
  map(~ select(.x, name, label))

# --- Step 3: Apply labeling function to each column automatically ---
apply_labels <- function(data, survey_map, choice_maps) {
  for (i in seq_len(nrow(survey_map))) {
    qname <- survey_map$name[i]
    list_name <- survey_map$list_name[i]
    qtype <- survey_map$type[i]
    
    if (!(qname %in% names(data))) next
    if (!(list_name %in% names(choice_maps))) next
    
    lookup <- choice_maps[[list_name]]
    original <- data[[qname]]
    
    if (qtype == "select_multiple") {
      label_col <- map_chr(original, function(x) {
        if (is.na(x) || x == "") return(NA_character_)
        codes <- str_split(x, " ")[[1]]
        labels <- lookup$label[match(codes, lookup$name)]
        paste(labels, collapse = ", ")
      })
    } else {
      label_col <- lookup$label[match(as.character(original), lookup$name)]
    }
    
    data[[paste0(qname, "_label")]] <- label_col
  }
  
  return(data)
}




