
## Prep env ----
source("config.yml")
source("requirements/libraries.R")

# Load ----
file_path <- ecw_db

wb <- wb_load(file_path)

# Check for the target table
target_tables <- wb$tables %>%
  dplyr::filter(tab_name == "financial_tracker")
  sheet_name <- wb$sheet_names[target_tables$tab_sheet]
  range <- target_tables$tab_ref
  
df <- wb_to_df(wb, sheet = sheet_name, range = range)
  
names(df) <- df[2,]
df <- df[-(1:2), ]
df <- df[,-(46:62)]

names(df) <- trimws(gsub("\\.", "", names(df)))
names(df) <- gsub("\\s+", "", names(df))

df %<>%
  filter(Typeofinvestment != "Total")

write.csv(df, "data/finance combiner/finance_db.csv", row.names = FALSE)
