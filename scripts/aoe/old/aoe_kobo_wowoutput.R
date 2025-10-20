
source("scripts/aoe/aoe_kobo_prepdata.R")

library(officer)
library(flextable)
library(dplyr)
library(stringr)
library(lubridate)

# 1) Set up paths
template_path <- "output data/aoe/template.docx"
out_dir       <- "output data/aoe"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 2) Find all your *_clean_word tibbles
word_sheets <- ls(pattern = "_clean_word$")

# 3) Columns you might ever want in your tables
table_cols <- c("text","theme","themeo","outcome","outcomeo")

for(sheet in word_sheets) {
  df_word  <- get(sheet, envir = .GlobalEnv)
  base_name <- str_remove(sheet, "_clean_word$")
  doc_path  <- file.path(out_dir, paste0(base_name, ".docx"))
  
  # 4) Start from the template
  doc <- read_docx(path = template_path)
  
  # 5) (Optionally) clear out any existing auto‐data in the template
  #     e.g. if you used content controls or bookmarks, you could remove them here.
  #     For now, we'll just append below whatever is already in the template.
  
  # 6) Separator
  doc <- body_add_par(doc,
                      paste0("=== START OF AUTO-ADDED DATA (", Sys.Date(), ") ==="),
                      style = "Normal"
  )
  
  # 7) Loop per Programme ID
  for(pid_val in unique(df_word$pid)) {
    sub_df <- filter(df_word, pid == pid_val)
    
    # Metadata
    country      <- sub_df$country[1]
    likert23     <- sub_df$likert23[1]
    likert24     <- sub_df$likert24[1]
    likert24_alt <- sub_df$likert24_alt[1]
    
    header_txt <- paste0(
      "Programme ID: ", pid_val,
      " | Country: ", country,
      if (!is.na(likert23)) paste0(" | Likert 23: ", likert23) else "",
      if (!is.na(likert24)) paste0(" | Likert 24: ", likert24) else "",
      if (!is.na(likert24_alt)) paste0(" | Likert 24 Alt: ", likert24_alt) else ""
    )
    doc <- body_add_par(doc, header_txt, style = "heading 1")
    
    # Safe table selection
    tbl_df <- sub_df %>% select(any_of(table_cols))
    if (ncol(tbl_df) > 0) {
      ft <- flextable(tbl_df) %>%
        set_table_properties(layout = "fixed") %>%
        fontsize(size = 9, part = "all")
      doc <- body_add_flextable(doc, ft)
    } else {
      doc <- body_add_par(doc, "No data available for this programme.", style = "Normal")
    }
    
    # Blank line
    doc <- body_add_par(doc, "", style = "Normal")
  }
  
  # End marker
  doc <- body_add_par(doc, "=== END OF AUTO-ADDED DATA ===", style = "Normal")
  
  # 8) Save out a fresh copy
  print(doc, target = doc_path)
}
