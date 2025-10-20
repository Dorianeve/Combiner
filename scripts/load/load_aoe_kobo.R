rm(list = ls())

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
source("config.yml")


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
  write_disk("data/arr/aoe/aoe_kobo_export.xlsx", overwrite = TRUE),
  add_headers(Authorization = paste("Token", token))
)

# Check if successful
if (status_code(res_file) == 200) {
  cat("✅ File downloaded successfully: kobo_export.xlsx\n")
} else {
  cat("❌ Failed to download file. Status:", status_code(res_file), "\n")
}
